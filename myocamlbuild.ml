open Ocamlbuild_plugin;;

ocaml_lib ~extern:true "llvm";;
ocaml_lib ~extern:true "llvm_bitwriter";;

flag ["link"; "ocaml"; "g++"] (S[A"-cc"; A"g++"; A"-cclib"; A"-rdynamic"; A"-ccopt"; A"-L/home/prc33/.opam/4.01.0/lib"]);;
