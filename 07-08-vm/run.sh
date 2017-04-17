#!/bin/sh
IFS_bak=$IFS
IFS=$'\n'

# for all directories which contain *.vm files
runs=(`find . -name "*.vm" | xargs -n 1 dirname | sort --unique | xargs -n 1 -I{} echo 'run {}'`)
sbt clean compile ${runs[*]}

IFS=$IFS_bak
