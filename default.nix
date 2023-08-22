{ pkgs ? import <nixpkgs> { system = builtins.currentSystem; } }:
  pkgs.callPackage ./asli.nix {}

