#![feature(core_intrinsics)]
#![feature(concat_bytes)]
#![cfg_attr(not(test), no_std)]

/*
let C = FATLength * NumberOfFATs

LAYOUT:

1 - Header
2..FATLength - FAT
C..C + ClusterCount * 2*SectorsPerClusterShift - Used Cluster Heap
C + ClusterCount * 2*SectorsPerClusterShift.. - Free Data Area (grow into like heap)
*/

/*
Theres exactly 1:1 ratio between FAT entries and clusters in the cluster heap/free area.

Each cluster in the heap is 4K and has a 32-bit entry header, file, filename, extension. If its a file entry, it should have subsequent headers for its filename and fileinfo (extension).

We cant know which file is what with just the FAT itself. Need access to the Root Dir. Which has child dirs and files. As entries. Since / is a dir, it would have a bunch of children entries after its own metadata that takes up like 2/3 entries. Then you have n entries after that for n children, including . and .. (which could be simulated actually)

E.g.
/
    /boot
/boot would be a file entry in the starting_cluster_offset of /. Which would take 3 entries
You then read that. To then get the children of /boot in the same way
*/

// Serialise should always serialise to a C-like struct. I dont want padding but apparently some unaligned reads/writes could be problematic. Not just for performance, but it could literally not work
// So implict padding for headers/metadata is fine I'd say. The actual data area itself shouldnt have padding or be structured in anyway

// should time modified, created, accessed be stored separately as a yml?
/*
/
    boot/
    config/

"/":
    last_modified: "dd-mm-yyyy"
    children:
      - "boot":
        last_modified: "dd-mm-yyyy"
      - "config":
        last_modified: "dd-mm-yyyy"

Note "." and ".." are implicit. Idk if we have to have it on std::fs though
But on the shell, you can just show it with ls
*/

// Do the entries have to be 32B? I guess its just easier to align them properly for reading and writing?
// Maybe just make it 64B
// or The next divisible after 300B. 512B just for the file header... I think its fine

// NOTE: Assume some global allocator exists (and also the handler for it)

// ------------
// USE LIBS
// ------------

extern crate alloc;

use alloc::{string::String, vec::Vec};
use bincode::{config, decode_from_slice, error::DecodeError, Decode, Encode};
use core::str::from_utf8;
use neutronapi::KTimestamp;

// -------------
// TYPES + CONSTANTS
// -------------

pub const CLUSTER_SIZE_L16TB: u64 = 4096;
// size of actual sectors on disk. Most likely 4K for SSDs
pub const SECTOR_SIZE: u64 = 4096;

pub const FS_NAME: [u8; 7] = *b"QUICKFS";

// -1 just in case rust or some code does something funny
pub const MAX_FILE_SIZE: u64 = u64::MAX - 1;

// we dont support unicode
type Filename = [u8; 256];

// Mostly for firmware use. Firmware will get a hint of what the dir/file is
// Actually IDK. Prob better to just hold that in an actual file as bytes
// Like most other things, so you can isolate them
// #[repr(u8)]
// pub enum FileAttributes {
//     ReadOnly,
//     ReadWrite,
// }

// ------------
// STRUCTURES
// ------------

// Cant be used directly as an EFI system partition, can as an ARCI partition
// Should have its own sector. And the first one of the partition (4K)
// The sector after that follow should always be the FAT. The FAT can take as many sectors as you need but scales with size of the partition (N sectors)
#[repr(C, align(4096))]
#[derive(Debug, Clone, Copy, Encode, Decode)]
pub struct Header {
    // Should just use a proper file in /boot (bootloader) as the boot code without any limits
    fs_name: [u8; 7],
    partition_offset: u64,
    // the size in bytes (or sectors?) of this partition
    vol_length: u64,
    // UNC: could prob be u8. Or we could just assume it comes right after the header. Either 0 or 4K
    fat_offset: u64,
    // Should be N for N clusters. On a 1TB partition with 4K sectors, you have 250 million sectors. Most of which can be used for the area
    fat_length: u64,
    // the offset of the data area (or FAT?) from the start of the header. Prob something like 
    cluster_heap_offset: u64,
    cluster_count: u64,
    // should be 0 or 4 (exFAT). Just 0 in QFS
    // first FAT entry is root dir. It should also point to either EndOfChain or 0
    first_cluster_of_root_dir: u64,
    bytes_per_sector_shift: u8,
    sectors_per_cluster_shift: u8,
    // up to 1 extra FAT for redundancy
    redundant_fat: u8,
    // update on the fly, prob not too important, just nice cache. rounded down
    percent_in_use: u8,
    // if marked as a boot partition, ARCI will search /boot/arcboot
    is_boot_partition: u8,
}

impl Header {
    /// FS Name is always "QuickFS"
    pub fn new(
        partition_offset: u64,
        vol_length: u64,
        fat_offset: u64,
        fat_length: u64,
        cluster_heap_offset: u64,
        cluster_count: u64,
        first_cluster_of_root_dir: u64,
        bytes_per_sector_shift: u8,
        sectors_per_cluster_shift: u8,
        redundant_fat: u8,
        percent_in_use: u8,
        is_boot_partition: u8,
    ) -> Self {
        Self {
            fs_name: FS_NAME,
            partition_offset,
            vol_length,
            fat_offset,
            fat_length,
            cluster_heap_offset,
            cluster_count,
            first_cluster_of_root_dir,
            bytes_per_sector_shift,
            sectors_per_cluster_shift,
            redundant_fat,
            percent_in_use,
            is_boot_partition,
        }
    }
}

impl Default for Header {
    /// Not recommended
    fn default() -> Self {
        Self {
            fs_name: FS_NAME,
            partition_offset: Default::default(),
            vol_length: Default::default(),
            fat_offset: Default::default(),
            fat_length: Default::default(),
            cluster_heap_offset: Default::default(),
            cluster_count: Default::default(),
            first_cluster_of_root_dir: Default::default(),
            bytes_per_sector_shift: Default::default(),
            sectors_per_cluster_shift: Default::default(),
            redundant_fat: Default::default(),
            percent_in_use: Default::default(),
            is_boot_partition: Default::default(),
        }
    }
}

// So you have:
// Cluster 1: (or any)
//  RootDirEntry 512B
//  FileEntry 512B -> start cluster 5
//  FileEntry 512B -> start cluster 12
//  FileEntry 512B -> start cluster 47

// Also a directory entry. A dir is just a file with pointers to other files
// Stored in the actual cluster (4K) data area
// can store up to 128 entries in a cluster for a directory. If more is needed, the dir can point to another cluster and use that (FAT)
// a file entry should actually have a stream entry right after it. Also directories too so you can tell how big they are
// the single stream entry should tell you where the first cluster is and so you can follow the FAT for that too

// IDK if should be aligned to page. I think it kinda makes sense since the each cluster should store at most 1 file entry. But can store its data in the end of the sector
// I think it makes more sense to store as less as possible within the data area so you have a quick cache place. You can also journal there so if the journal val for a file is 0 its fine. But 1 means it still hasnt been fully committed. You set it to 1 and write to journal. Then write to the data
#[repr(C, align(4096))]
pub struct FileEntry {
    // CRC32C checksum
    checksum: u32,
    // a read only file should not be modified. Usually a system file
    // Not really a thing on QFS
    // file_attributes: FileAttributes,
    // I feel like it'd make more sense to store more in another file. I.e. the "user" API
    creation_timestamp: KTimestamp,
    last_modification_timestamp: KTimestamp,
    last_access_timestamp: KTimestamp,
    hash_of_name: u16,
    first_cluster: u32,
    // need to update this (basically a cached field)
    length_of_data: u64,
    filename: Filename,
}

#[repr(C)]
pub struct FATEntry {
    val: u64,
}

#[repr(u32)]
#[derive(PartialOrd, Ord, PartialEq, Eq)]
pub enum FATEntryType {
    Free = 0x0,
    // Pseudo type, used as an else condition
    Pointer,
    Bad = 0xFFFFFFF7,
    EndOfChain = 0xFFFFFFFF,
}

impl FATEntry {
    pub fn new(val: u64) -> Self {
        Self { val }
    }

    pub fn identify(&mut self) -> FATEntryType {
        match self.val {
            0x0 => FATEntryType::Free,
            0xFFFFFFF7 => FATEntryType::Bad,
            0xFFFFFFFF => FATEntryType::EndOfChain,
            _ => FATEntryType::Pointer,
        }
    }
}

// ------------
// INTERNAL API
// ------------

pub fn to_bytes<T: Encode>(t: &T) -> Vec<u8> {
    let res = bincode::encode_to_vec(
        t,
        config::standard()
            .with_little_endian()
            .write_fixed_array_length()
            .with_variable_int_encoding(),
    );

    match res {
        Ok(r) => r,
        Err(_) => panic!("Something went wrong with serialising to bytes"),
    }
}

pub fn bytes_to_str(bytes: &Vec<u8>) -> String {
    let res = core::str::from_utf8(bytes).unwrap_or("Error: couldnt convert bytes into a String");

    String::from(res)
}

// Decode function, make sure QuickFS signature exists
pub fn bytes_to_type<T: Decode>(bytes: &[u8]) -> Option<T> {
    let res: Result<(T, usize), DecodeError> = decode_from_slice(
        bytes,
        config::standard()
            .with_little_endian()
            .write_fixed_array_length()
            .with_variable_int_encoding(),
    );

    match res {
        Ok(r) => Some(r.0),
        Err(_) => None,
    }
}

/// Type must implement FromStr!
macro_rules! retrieve_or_propagate {
    ($in:expr,$in_type:tt) => {
        match $in_type::from_str($in) {
            Ok(i) => i,
            Err(_) => return Err(()),
        }
    };
    ($in:expr,$in_type:tt,$err:expr) => {
        match $in_type::from_str($in) {
            Ok(i) => i,
            Err(_) => return Err($err),
        }
    };
}

// JUST USE FROMSTR

// https://students.cs.byu.edu/~cs345ta/labs/P6-FAT%20Supplement.html
// great stuff

fn read_inodes() {}

fn walk_fs() {}

fn print_fs() {}

// --------------
// TESTS
// --------------

#[test]
fn test_str_to_struct() {
    // bad input
    // NOTE: convert to str
    // Basically, just read from a file into a &[u8] instead and pass that into decode (bytes_to_type)
    let input = from_utf8(b"").unwrap();

    // let res = Header::from_str(input);
    // assert!(res.is_err());

    // good input

    // i think it actually pads the stuff, so use debug

    // NOTE: numerics need to be of the same size, e.g. 64bit, 32bit, 8bit and little endian
    let partition_offset = 0 as u64;
    let vol_length = 0 as u64;
    let fat_offset = 0 as u64;
    let fat_length = 0 as u64;
    let cluster_heap_offset = 0 as u64;
    let cluster_count = 0 as u64;

    let first_cluster_of_root_dir = 0 as u64;
    let bytes_per_sector_shift = 0 as u8;
    let sectors_per_cluster_shift = 0 as u8;
    // NOTE: false == 0 as u8. true == 1 as u8 if padding is required
    let redundant_fat = false as u8;
    let percent_in_use = 0 as u8;
    let is_boot_partition = false as u8;

    // JUST SERIALISE INTO a STRING -> THEN DESERIALISE with FromStr

    // create struct
    let res = Header::new(
        partition_offset,
        vol_length,
        fat_offset,
        fat_length,
        cluster_heap_offset,
        cluster_count,
        first_cluster_of_root_dir,
        bytes_per_sector_shift,
        sectors_per_cluster_shift,
        redundant_fat,
        percent_in_use,
        is_boot_partition,
    );

    println!("res after constructing Header = {:?}", res);

    // serialise
    let input = to_bytes(&res);
    println!("input after serialising to bytes = {:?}", input);

    let res: Option<Header> = bytes_to_type(&input);
    // if Ok, then print it out, otherwise state error and panic
    match res {
        Some(r) => {
            println!("res = {:?}", r);
            let fs_name = r.fs_name;
            if fs_name != FS_NAME {
                panic!(
                    "QuickFS signature not found. Actual signature = {:?}",
                    r.fs_name
                );
            }
        }
        None => {
            panic!("Error: couldn't reserialise header")
        }
    }
}

// ------------
// SYSTEM API
// ------------

// uses the internal api

// VFS API
// read, write, open, close
// in QFS, open() actually reads() it into memory

// in Neutron, there isnt really an 'open'. But since std::fs has open, we do have it
// read() doesnt actually mean open() first. Esp read_to_string() reads the entire thing or just read() means read_to_string

// converts the filepath to a disk path
// by searching the in memory DB of the rootfs tree
// if there, attempt to go there by following the same disk path
// or if the file's beginning offset is cached in the in memory struct (tree), go to that offset directly and follow the chains (FAT)
fn read_to_string(filepath: &str) {
    // walk the fs to see if that file exists
}
