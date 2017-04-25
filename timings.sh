#!/bin/bash

echo "Timings" > timings.txt;
for i in `seq 1 40`;  
do
    echo $i >> timings.txt;     
    { time bin/cilly --load=lib/cil/inliner.cma --load=lib/cil/printers.cma --load=lib/cil/cynthesis.cma --docynthesis --cynthesis_print_flags=0 -DNr=$i tests/aes/aes/aesplain.c >/dev/null; } 2>&1 | grep real >> timings.txt;
    { time bin/cilly --load=lib/cil/inliner.cma --load=lib/cil/printers.cma --load=lib/cil/cynthesis.cma --docynthesis --cynthesis_print_flags=0 -DNr=$i tests/aes/aes/aesplain.c >/dev/null; } 2>&1 | grep real >> timings.txt;
    { time bin/cilly --load=lib/cil/inliner.cma --load=lib/cil/printers.cma --load=lib/cil/cynthesis.cma --docynthesis --cynthesis_print_flags=0 -DNr=$i tests/aes/aes/aesplain.c >/dev/null; } 2>&1 | grep real >> timings.txt;
done