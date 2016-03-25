#!/bin/sh

function measure() {
  ./changeLogList +RTS -N8 -RTS --threads=$1 --changes=$2 --use-change-log=false 2>changeLog.txt
  WO_TOTAL=`cat changeLog.txt | grep process | wc -l`
  UNIQUE=`cat changeLog.txt | grep process | sort | uniq | wc -l`
  ./changeLogList +RTS -N8 -RTS --threads=$1 --changes=$2 --use-change-log=true 2>changeLog.txt
  W_TOTAL=`cat changeLog.txt | grep process | wc -l`
  echo $1 $2 $UNIQUE $WO_TOTAL $W_TOTAL
}

measure 5 20
measure 5 30
measure 5 40
measure 5 50
measure 5 80
measure 5 100

measure 10 20
measure 10 30
measure 10 40
measure 10 50
measure 10 80
measure 10 100

measure 20 20
measure 20 30
measure 20 40
measure 20 50

measure 50 5
measure 50 10
measure 50 15
