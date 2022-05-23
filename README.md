# QFS

Quick Filesystem for Neutron. Basically a non bad version of exFAT.

To use the GUI, build `neutronfs` frontend. You have to specify a root filesystem which would be `nefs`. And mount a `qfs` partition on it. Then the frontend will be able to load the nefs file and walk/read/write on it.

## CLI

In addition to the lib, qfs comes with a cli to load a QFS file into memory and walk through it.

The CLI requires a working OS as we assume a VFS file formatted with QFS headers (raw file).
