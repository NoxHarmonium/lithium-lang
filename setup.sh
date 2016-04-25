#!/bin/bash

# This script sets up the build environment on a Mac
# It assumes you have homebrew installed

brew tap homebrew/versions
brew install llvm35
cabal sandbox init
cabal install --only-dependencies
