#!/bin/bash
# Requires that you have created the asli package with `opam pin add . -k path`, 
# which will have been done if you are using the dockerfile.
# If you are having issues with your changes not getting included (opam says 'no changes'),
# try unpinning and repinning the package:
#   opam pin remove .
#   opam pin add . -k path

cd $HOME/asl-interpreter && eval $(opam env) &&
dune build &&
opam install asli