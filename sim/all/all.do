make clean
make

#vsim -novopt -t 1ps all_tb
vsim work.main -voptargs="+acc"
log -r /*
do all_wave.do

view wave
view transcript

force -freeze sim:/main/UUT/cmp_mil1553_core/tx_tr_flag 1 0
force -freeze sim:/main/UUT/cmp_mil1553_core/tx_word_cnt 00001 0

run 5 ms



