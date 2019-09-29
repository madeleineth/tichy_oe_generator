#!/bin/bash -e

small_dict="$(mktemp)"
small_output="$(mktemp)"
trap "rm -f '$small_dict' '$small_output'" EXIT

perltidy -b -novalign -pt=2 -l=120 create_dict31.pl
if ! cmp create_dict31.pl create_dict31.pl.bak ; then
  echo >&2 "perltidy reported un-tidy perl."
  exit 1
fi

head dict_adj-vb-part-num-adv-noun.txt > "$small_dict"
./create_dict31.pl --dictionary "$small_dict" --output "$small_output"

output_lines="$(wc -l < "$small_output")"
if [ "$output_lines" -lt 1000 ] ; then
  echo >&2 "Output is too small: $output_lines in $small_output."
  exit 1
fi

echo Pass.
