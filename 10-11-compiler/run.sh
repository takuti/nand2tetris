#!/bin/sh
IFS_bak=$IFS
IFS=$'\n'

# 10: Analyzer which outputs XML files
runs=(`find TestXML -name "*.jack" | xargs -n 1 dirname | sort --unique | xargs -n 1 -I{} echo 'runMain JackAnalyzer {}'`)

# 11: Compiler
runs+=(`find TestVM -name "*.jack" | xargs -n 1 dirname | sort --unique | xargs -n 1 -I{} echo 'runMain JackCompiler {}'`)

sbt clean compile ${runs[*]}

IFS=$IFS_bak
