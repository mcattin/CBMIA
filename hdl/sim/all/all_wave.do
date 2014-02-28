onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -divider {Memory interface}
add wave -noupdate -divider Registers
add wave -noupdate -divider {MIL1553 interface}
add wave -noupdate -divider TX
add wave -noupdate /main/UUT/cmp_mil1553_core/tx_tr_flag
add wave -noupdate -radix unsigned /main/UUT/cmp_mil1553_core/tx_word_cnt
add wave -noupdate -divider RX
add wave -noupdate -radix unsigned /main/UUT/cmp_mil1553_core/rx_word_cnt
add wave -noupdate /main/UUT/cmp_mil1553_core/rx_done_p
add wave -noupdate -divider Transaction
add wave -noupdate -divider {Error counters}
add wave -noupdate /main/UUT/cmp_mil1553_core/rx_nb_word_error_p
add wave -noupdate -radix unsigned /main/UUT/cmp_mil1553_core/rx_nb_word_error_cnt
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {1546525000 ps} 0}
configure wave -namecolwidth 361
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ns
update
WaveRestoreZoom {0 ps} {2100 us}
