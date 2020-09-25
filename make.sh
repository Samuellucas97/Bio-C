#!/bin/bash
cd src
alex tokens.x
ghc tokens.hs

rm *.hi *.o