#!/bin/sh

function measure() {
  ./changeLogList +RTS -N8 -RTS --threads=$1 --changes=$2 --use-change-log=$3 2>changeLog.txt
  TOTAL=`cat changeLog.txt | grep process | wc -l`
  UNIQUE=`cat changeLog.txt | grep process | sort | uniq | wc -l`
  echo $1 $2 $3 $TOTAL $UNIQUE
}

measure 5 20 true
measure 5 30 true
measure 5 40 true
measure 5 50 true
measure 5 80 true
measure 5 100 true

measure 5 20 false
measure 5 30 false
measure 5 40 false
measure 5 50 false
measure 5 80 false
measure 5 100 false

measure 10 20 true
measure 10 30 true
measure 10 40 true
measure 10 50 true
measure 10 80 true
measure 10 100 true

measure 10 20 false
measure 10 30 false
measure 10 40 false
measure 10 50 false
measure 10 80 false
measure 10 100 false

measure 20 20 true
measure 20 30 true
measure 20 40 true
measure 20 50 true

measure 20 20 false
measure 20 30 false
measure 20 40 false
measure 20 50 false
