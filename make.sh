#!/usr/bin/env bash

# Initial Setup 
mkdir -p ./build/ 
rm -rf ./build/*

for file in *.hs; do
    ghc "$file"
    rm *.hi *.o # cleanup
    mv "${file%.*}" ./build/ # move binaries to build
done
