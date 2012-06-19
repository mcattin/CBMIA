make clean
make

#vsim -novopt -t 1ps receiver_tb
vsim work.main -voptargs="+acc"
log -r /*
do receiver_wave.do

view wave
view transcript

run 5 ms



