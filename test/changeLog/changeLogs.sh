#!/bin/sh

function measure_list() {
  measure "./changeLogList" $1 $2
}

function measure_map() {
  measure "./changeLogSet" $1 $2
}

function measure() {
  $1 +RTS -N8 -RTS --threads=$2 --changes=$3 --use-change-log=false 2>changeLog.txt
  WO_TOTAL=`cat changeLog.txt | grep process | wc -l`
  UNIQUE=`cat changeLog.txt | grep process | sort | uniq | wc -l`
  $1 +RTS -N8 -RTS --threads=$2 --changes=$3 --use-change-log=true 2>changeLog.txt
  W_TOTAL=`cat changeLog.txt | grep process | wc -l`
  echo $1 $2 $3 $UNIQUE $WO_TOTAL $W_TOTAL
}


ghc -O2 -threaded -rtsopts --make changeLogList.hs
ghc -O2 -threaded -rtsopts --make changeLogSet.hs

echo "list: -------------- "

measure_list 5 20
measure_list 5 30
measure_list 5 40
measure_list 5 50
measure_list 5 80
measure_list 5 100

measure_list 10 20
measure_list 10 30
measure_list 10 40
measure_list 10 50
measure_list 10 80
measure_list 10 100

measure_list 20 20
measure_list 20 30
measure_list 20 40
measure_list 20 50

measure_list 50 5
measure_list 50 10
measure_list 50 15

measure_list 100 5
measure_list 100 10
measure_list 100 15

echo "map: -------------- "

measure_map 5 20
measure_map 5 30
measure_map 5 40
measure_map 5 50
measure_map 5 80
measure_map 5 100

measure_map 10 20
measure_map 10 30
measure_map 10 40
measure_map 10 50
measure_map 10 80
measure_map 10 100

measure_map 20 20
measure_map 20 30
measure_map 20 40
measure_map 20 50

measure_map 50 5
measure_map 50 10
measure_map 50 15

measure_map 100 5
measure_map 100 10
measure_map 100 15
