{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell { buildInputs = [ zlib stack ]; }
