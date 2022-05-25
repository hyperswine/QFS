use quickfs::Header;

// ensure integration tests use std
extern crate std;

#[test]
fn test_add() {
    let res = Header::default();
    println!("res = {:?}", res);
}
