#!/bin/bash

echo ':gen A64 aarch64_.+' | dune exec asli
dune build
