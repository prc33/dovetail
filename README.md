Dovetail JCAM
=============
A prototype implementation of the Join Calclus Abstract Machine.

## Usage
The main build file compiles the included benchmarks, placing the binaries in the `bin` directory.
The JCAM compiler itself, `jcamc`, is also in this directory.
It takes 2 arguments: the JCAM source and the output filename.
This must then be linked against the Dovetail runtime library, the Boehm garbage collector and pthreads.

## Requirements
Dovetail makes use of LLVM via the OCaml bindings. OCaml and LLVM can be installed using OPAM as follows:

```
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh
sh ./opam_installer.sh /usr/local/bin 4.01.0
opam install core
opam install llvm.3.2
```

The Boehm garbage collector library is also required, and expected to be installed in `~/boehm/`. This can be acheived as follows:
```
wget http://www.hpl.hp.com/personal/Hans_Boehm/gc/gc_source/gc.tar.gz
tar xvfz gc.tar.gz
cd gc
./configure --prefix=/home/prc33/boehm --enable-threads=posix --enable-thread-local-alloc --enable-parallel-mark
make
make check
make install
```
