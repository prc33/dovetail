Dovetail JCAM
=============
A prototype implementation of the Join Calclus Abstract Machine.

## Usage
The main build file compiles the included benchmarks, placing the binaries in the `bin` directory.
The JCAM compiler itself, `jcamc`, is also in this directory.
It takes 2 arguments: the JCAM source and the output filename.
This must then be linked against the Dovetail runtime library, the Boehm garbage collector and pthreads.

## Requirements
Dovetail makes use of LLVM via the OCaml bindings. OCaml and LLVM can be installed using OPAM as follows. The LLVM bindings require the normal OS LLVM package to be installed too. Alternatively, add the `-b` option to `opam install llvm.3.4` and then go to the build directory (`~/.opam/4.01.0/build/llvm.3.4`) and run `make install`. Adding `LDFLAGS=~/.opam/4.01.0/lib` to your environment, and a symlink from` llvm-config-3.4` to `llvm-config` should then allow the opam install operation to succeed.

```
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh
sh ./opam_installer.sh /usr/local/bin 4.01.0
opam install core
opam install llvm.3.4
```

The Boehm garbage collector library is also required, and expected to be installed in `~/boehm/`. This can be acheived as follows:
```
wget http://hboehm.info/gc/gc_source/gc-7.4.0.tar.gz
wget http://hboehm.info/gc/gc_source/libatomic_ops-7.4.0.tar.gz
tar xvfz gc-7.4.0.tar.gz
tar xvfz gc-7.4.0.tar.gz
mv libatomic_ops-7.4.0 gc-7.4.0/libatomic_ops
cd gc-7.4.0
./configure --prefix=/home/prc33/boehm --enable-threads=posix --enable-thread-local-alloc --enable-parallel-mark
make
make check
make install
```
