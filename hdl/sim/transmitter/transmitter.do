make clean
make

vsim -novopt -t 1ps transmitter_tb
log -r /*
do transmitter_wave.do

view wave
view transcript

run 2 ms



