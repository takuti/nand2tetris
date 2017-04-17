#!/bin/sh
IFS_bak=$IFS
IFS=$'\n'

runs=(`find . -name "*.asm" | xargs -n 1 -I{} echo 'run {}'`)
sbt clean compile ${runs[*]}

IFS=$IFS_bak
