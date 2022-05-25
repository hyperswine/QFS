#![feature(core_intrinsics)]
#![feature(concat_bytes)]
#![cfg_attr(not(test), no_std)]

// ------------
// USE LIBS
// ------------

use core::{
    char::ParseCharError,
    mem,
    num::ParseIntError,
    ops::Range,
    slice,
    str::{from_utf8, FromStr},
};

// assume some global allocator exists (and also the handler for it)
extern crate alloc;

use alloc::{
    string::{String, ToString},
    vec::Vec,
};
use bincode::{config, decode_from_slice, error::DecodeError, Decode, Encode};
use neutronapi::KTimestamp;

use serde::{Deserialize, Serialize};

// ------------
// STRUCTURES
// ------------

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

pub const CLUSTER_SIZE_L16TB: u64 = 4096;

// pub const FS_NAME: [char; 7] = ['Q', 'U', 'I', 'C', 'K', 'F', 'S'];

pub const FS_NAME: [u8; 7] = *b"QUICKFS";

// Cant be used directly as an EFI system partition, can as an ARCI partition
#[repr(C)]
#[derive(Debug, Clone, Copy, Encode, Decode)]
pub struct Header {
    // Should just use a proper file in /boot (bootloader) as the boot code without any limits
    fs_name: [u8; 7],
    partition_offset: u64,
    vol_length: u64,
    fat_offset: u32,
    fat_length: u32,
    cluster_heap_offset: u32,
    cluster_count: u32,
    // should be 0 or 4 (exFAT). Just 0 in QFS
    // first FAT entry is root dir. It should also point to either EndOfChain or 0
    first_cluster_of_root_dir: u32,
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
        fat_offset: u32,
        fat_length: u32,
        cluster_heap_offset: u32,
        cluster_count: u32,
        first_cluster_of_root_dir: u32,
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

// -1 just in case rust or some code does something funny
pub const MAX_FILE_SIZE: u64 = u64::MAX - 1;

#[repr(u8)]
pub enum EntryType {
    // Core
    FileEntry = 0x85,
    ExtensionEntry = 0xC0,
    FilenameEntry = 0xC1,
    // Meta
    AllocationBitmap = 0x81,
    UpcaseTable = 0x82,
    VolumeLabel = 0x83,
}

type DirEntSiz = [u8; 32];

// * ENSURE that each entry is 32 bytes. Just append x bytes to the end of each

#[repr(u16)]
pub enum FileAttributes {
    RD_ONLY,
    RD_WRITE,
}

/// The new() functions set the right entry type vals by default
#[repr(C)]
pub enum DirectoryEntry {
    // Also a directory entry
    // Stored in the actual cluster (4K) data area
    // can store up to 128 entries in a cluster for a directory. If more is needed, the dir can point to another cluster and use that (FAT)
    // a file entry should actually have a stream entry right after it. Also directories too so you can tell how big they are
    // the single stream entry should tell you where the first cluster is and so you can follow the FAT for that too
    FileEntry {
        entry_type: EntryType,
        checksum: u16,
        n_secondary_entries: u8,
        file_attributes: FileAttributes,
        creation_timestamp: KTimestamp,
        last_modification_timestamp: KTimestamp,
        last_access_timestamp: KTimestamp,
        // padding for 32B
        padding: [u8; 8],
    },
    ExtensionEntry {
        // should be flags, filenamelengthinbytes. The first cluster is good
        entry_type: EntryType,
        secondary_files: u8,
        length_of_name: u8,
        hash_of_name: u16,
        first_cluster: u32,
        length_of_data: u64,
        // padding for 32B
        padding: [u8; 17],
    },
    FilenameEntry {
        entry_type: EntryType,
        // if longerr filename, use an extension entry and set flags EXTRA_FILENAME. Doesnt include full name of the path
        flags: FilenameFlags,
        filename: [u8; 30],
    },
}

#[repr(u8)]
pub enum FilenameFlags {
    Standard,
    LongerFilename,
}

impl DirectoryEntry {
    /// @arg current_date => "yyyy-mm-dd"
    pub fn new_file_entry(current_date: &str) -> Result<Self, ()> {
        let curr_date = match KTimestamp::from_yyyy_mm_dd(current_date) {
            Some(d) => d,
            None => return Err(()),
        };

        Ok(Self::FileEntry {
            entry_type: EntryType::FileEntry,
            checksum: 0,
            n_secondary_entries: 0,
            file_attributes: FileAttributes::RD_WRITE,
            creation_timestamp: curr_date,
            last_modification_timestamp: curr_date,
            last_access_timestamp: curr_date,
            padding: Default::default(),
        })
    }

    pub fn new_filename_entry(filename: &[u8; 30]) -> Self {
        Self::FilenameEntry {
            entry_type: EntryType::FilenameEntry,
            flags: FilenameFlags::Standard,
            filename: filename.clone(),
        }
    }

    // Should just make it manually, for now
    // pub fn new_extension_entry() -> Self {
    //     Self::ExtensionEntry {
    //         entry_type: (),
    //         secondary_files: (),
    //         length_of_name: (),
    //         hash_of_name: (),
    //         first_cluster: (),
    //         length_of_data: (),
    //         padding: (),
    //     }
    // }
}

#[repr(C)]
pub struct FATEntry {
    val: u32,
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
    pub fn new(val: u32) -> Self {
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
    let fat_offset = 0 as u32;
    let fat_length = 0 as u32;
    let cluster_heap_offset = 0 as u32;
    let cluster_count = 0 as u32;

    let first_cluster_of_root_dir = 0 as u32;
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
