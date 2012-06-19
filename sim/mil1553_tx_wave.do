onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/sys_rst_n_i
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/sys_clk_i
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/tx_bit_rate
add wave -noupdate -radix hexadecimal -childformat {{/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(0) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(1) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(2) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(3) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(4) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(5) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(6) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(7) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(8) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(9) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(10) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(11) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(12) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(13) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(14) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(15) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(16) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(17) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(18) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(19) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(20) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(21) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(22) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(23) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(24) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(25) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(26) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(27) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(28) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(29) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(30) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(31) -radix hexadecimal} {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(32) -radix hexadecimal}} -subitemconfig {/mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(0) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(1) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(2) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(3) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(4) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(5) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(6) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(7) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(8) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(9) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(10) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(11) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(12) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(13) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(14) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(15) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(16) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(17) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(18) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(19) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(20) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(21) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(22) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(23) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(24) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(25) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(26) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(27) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(28) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(29) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(30) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(31) {-height 16 -radix hexadecimal} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i(32) {-height 16 -radix hexadecimal}} /mil1553_tx_tb/uut_mil1553_tx/tx_buffer_i
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/tx_send_frame_p_i
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/tx_done_p_o
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/mil1553_tx_o
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/mil1553_txd_o
add wave -noupdate -divider serialiser
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/sys_rst_n_i
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/sys_clk_i
add wave -noupdate -radix hexadecimal /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/tx_buffer_i
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/tx_bit_rate_p_i
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/tx_send_frame_p_i
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/tx_send_frame_p
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/tx_send_frame_p_d
add wave -noupdate -radix unsigned /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/nb_word_to_send
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/tr_flag
add wave -noupdate -radix hexadecimal /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/tx_buffer_encoded
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/tx_fsm_next_state
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/tx_fsm_state
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/transmitting
add wave -noupdate -radix unsigned /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/bit_cnt
add wave -noupdate -radix unsigned /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/word_cnt
add wave -noupdate -radix hexadecimal /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/tx_shift_reg
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/tx_done_p_o
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/mil1553_tx_o
add wave -noupdate /mil1553_tx_tb/uut_mil1553_tx/cmp_mil1553_serialiser/mil1553_txd_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {21162500 ps} 0}
configure wave -namecolwidth 502
configure wave -valuecolwidth 97
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
configure wave -timelineunits ps
update
WaveRestoreZoom {0 ps} {840 us}
