make clean
make

vsim -novopt -t 1ps core_tb
log -r /*
do core_wave.do

view wave
view transcript

run 2 ms



