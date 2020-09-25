#!/bin/bash
cd src
alex tokens.x
ghc parser.hs

rm *.hi *.o