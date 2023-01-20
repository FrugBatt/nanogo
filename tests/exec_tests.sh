#!/bin/sh

GO_SRC=$(find go/*.go)


for SRC in $GO_SRC; do
  NAME=$(echo $SRC | cut -d '.' -f 1 | cut -d '/' -f 2)
  echo "----- File $NAME -----"
  echo ""
  echo "---------------------"
  echo "-- Correct output ---"
  echo "---------------------"
  # cat "correct_output/$NAME"
  go run "go/$NAME.go"
  echo ""

  echo "---------------------"
  echo "---- Ngoc output ----"
  echo "---------------------"
  ../ngoc "go/$NAME.go" && gcc -no-pie "go/$NAME.s" -o "go/$NAME.out" && ./go/$NAME.out
  echo ""
  read -n 1 -s
done

rm go/*.s go/*.out
