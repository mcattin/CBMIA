vsim -novopt -t 1ps mil1553_tx_tb
log -r /*
do mil1553_tx_wave.do

view wave
view transcript

run 800 us



