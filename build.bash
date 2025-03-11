#!/bin/bash

mkdir -p obj

if [[ $1 == debug ]]; then
  fpc src/pasfetch.pas -FE"out/" -Fu"inc/" -g -dDEBUG
else
  fpc src/pasfetch.pas -FE"out/" -Fu"inc/" -O4 -Xs -XX
fi
