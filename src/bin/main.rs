use std::{env, fs::File, io::Read, os::unix::prelude::FileExt};

fn main() -> io::Result<()> {
    // take in command line args
    let args: Vec<String> = env::args().collect();
    // if no args specified, print usage
    if args.len() == 1 {
        println!("Usage: {} <file>", args[0]);
        return;
    }

    // args[1] should be the VFS file
    let qfs_file = &args[1];

    // if qfs_file doesn't exist in path, print error
    if !std::path::Path::new(qfs_file).exists() {
        println!("File {} does not exist", qfs_file);
        return;
    }

    // read qfs_file to string
    // let qfs_file = std::fs::read_to_string(qfs_file).expect("couldn't read qfs_file");

    // open the file
    let mut f = File::open(qfs_file)?;

    const HEADER_OFFSET: usize = 0;
    const HEADER_SIZE: usize = 1000;

    // read the header to string
    let mut header_raw = [0 as u8; HEADER_SIZE];
    f.read_exact(&mut header_raw);

    // load qfs_file (the headers) into memory
    let headers = read_headers(&header_raw);

    // given headers, read inode table to get the list of files
    // and build the filesystem hierarchy into an enum FSTree
    let inodes = read_inodes(&headers, &f);

    // by walking through its hierarchy, mapping each file's name (like an ls -R)
    let res = walk_fs(inodes);

    // print (ls -R /)
    print_fs(res);

    Ok(())
}
