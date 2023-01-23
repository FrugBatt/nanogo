#!/bin/sh

GO_SRC=$(find tests/*.go)


for SRC in $GO_SRC; do
  NAME=$(echo $SRC | cut -d '.' -f 1 | cut -d '/' -f 2)
  echo "----- File $NAME -----"
  echo ""
  echo "---------------------"
  echo "-- Correct output ---"
  echo "---------------------"
  # cat "correct_output/$NAME"
  go run "tests/$NAME.go"
  echo ""

  echo "---------------------"
  echo "---- Ngoc output ----"
  echo "---------------------"
  ./ngoc "tests/$NAME.go" && gcc -no-pie "tests/$NAME.s" -o "tests/$NAME.out" && ./tests/$NAME.out
  echo ""
  read -n 1 -s
done

rm tests/*.s tests/*.out
