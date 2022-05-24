#![no_std]
#![feature(core_intrinsics)]

// ------------
// USE LIBS
// ------------

use core::{char::ParseCharError, mem, num::ParseIntError, ops::Range, slice, str::FromStr};

use neutronapi::KTimestamp;

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

pub const CLUSTER_SIZE_L16TB: u64 = 4096;

pub const FS_NAME: [char; 7] = ['Q', 'U', 'I', 'C', 'K', 'F', 'S'];

// Cant be used directly as an EFI system partition, can as an ARCI partition
#[repr(C)]
pub struct Header {
    // Should just use a proper file in /boot (bootloader) as the boot code without any limits
    fs_name: [char; 7],
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
    redundant_fat: bool,
    // update on the fly, prob not too important, just nice cache. rounded down
    percent_in_use: u8,
    // if marked as a boot partition, ARCI will search /boot/arcboot
    is_boot_partition: bool,
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
        redundant_fat: bool,
        percent_in_use: u8,
        is_boot_partition: bool,
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
            fs_name: Default::default(),
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
// API
// ------------

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

impl FromStr for Header {
    type Err = ();

    // just interpret the thing as int or str
    // and assume its in the right format

    /// Takes in a raw string of size Header. Attempts to parse the first sector size of the string
    /// And return a BootSector struct
    /// For use in memory
    fn from_str(raw_string: &str) -> Result<Self, Self::Err> {
        // the header portion
        if raw_string.len() != mem::size_of::<Header>() {
            return Err(());
        }

        let fs_name = &raw_string[0..6];
        if fs_name != "QuickFS" {
            return Err(());
        }

        // 8 Bytes -> 7 8 9 10 11 12 13 14 (basically 7 + 8 since end is exclusive)
        // MANUAL WAY
        // let partition_offset = retrieve_or_propagate!(&raw_string[7..15], u64);

        /*
        Header::new(
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
        )
        */

        let mut header: Header = unsafe { mem::zeroed() };

        let header_size = mem::size_of::<Header>();

        // cast raw string to bytes
        let mut raw_string = raw_string.as_bytes();

        unsafe {
            let header_slice =
                slice::from_raw_parts_mut(&mut header as *mut _ as *mut u8, header_size);
            // `read_exact()` comes from `std::Read` impl for `&[u8]`. Have to use another implementation
            read_exact::ReadExactExt::read_exact_or_eof(&mut raw_string, header_slice).unwrap();
        }

        Ok(header)
    }
}

// JUST USE FROMSTR

fn read_inodes() {}

fn walk_fs() {}

fn print_fs() {}
