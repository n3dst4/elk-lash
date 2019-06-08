#!/bin/bash

DIR=$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)

cd $DIR

docker build -t n3dst4/haskell_base .
