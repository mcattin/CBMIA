onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /main/clk
add wave -noupdate /main/rst
add wave -noupdate /main/tx
add wave -noupdate /main/rx_buffer
add wave -noupdate /main/rx_done
add wave -noupdate /main/rx_in_progress
add wave -noupdate -divider RX
add wave -noupdate /main/UUT/sys_rst_n_i
add wave -noupdate /main/UUT/sys_clk_i
add wave -noupdate /main/UUT/signif_edge_window
add wave -noupdate /main/UUT/sample_manch_bit_p
add wave -noupdate /main/UUT/sample_bit_p
add wave -noupdate /main/UUT/rxd_filt_r_edge_p
add wave -noupdate /main/UUT/rxd_filt_f_edge_p
add wave -noupdate /main/UUT/rxd_filt_edge_p
add wave -noupdate /main/UUT/rxd_filt
add wave -noupdate /main/UUT/rx_in_progress_o
add wave -noupdate /main/UUT/rx_done_p_o
add wave -noupdate /main/UUT/rx_clk_rst
add wave -noupdate -radix hexadecimal /main/UUT/rx_buffer_o
add wave -noupdate -radix unsigned /main/UUT/rx_word_cnt_o
add wave -noupdate /main/UUT/mil1553_rxd_i
add wave -noupdate /main/UUT/mil1553_rx_en_i
add wave -noupdate /main/UUT/adjac_bits_window
add wave -noupdate -divider {RX DEGLITCHER}
add wave -noupdate /main/UUT/cmp_mil1553_rx_deglitcher/sys_rst_n_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deglitcher/sys_clk_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deglitcher/rxd_a_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deglitcher/rxd_sync
add wave -noupdate /main/UUT/cmp_mil1553_rx_deglitcher/rxd_filt_d1
add wave -noupdate /main/UUT/cmp_mil1553_rx_deglitcher/rxd_filt
add wave -noupdate /main/UUT/cmp_mil1553_rx_deglitcher/filt_cnt
add wave -noupdate /main/UUT/cmp_mil1553_rx_deglitcher/rxd_filt_o
add wave -noupdate /main/UUT/cmp_mil1553_rx_deglitcher/rxd_filt_edge_p_o
add wave -noupdate /main/UUT/cmp_mil1553_rx_deglitcher/rxd_filt_r_edge_p_o
add wave -noupdate /main/UUT/cmp_mil1553_rx_deglitcher/rxd_filt_f_edge_p_o
add wave -noupdate -divider {RX CLK}
add wave -noupdate /main/UUT/cmp_mil1553_rx_clk/sys_rst_n_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_clk/sys_clk_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_clk/rxd_edge_p_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_clk/rx_clk_rst_i
add wave -noupdate -radix unsigned /main/UUT/cmp_mil1553_rx_clk/s_margin
add wave -noupdate -radix unsigned /main/UUT/cmp_mil1553_rx_clk/s_period
add wave -noupdate -radix unsigned /main/UUT/cmp_mil1553_rx_clk/s_half_period
add wave -noupdate -radix unsigned /main/UUT/cmp_mil1553_rx_clk/s_period_c
add wave -noupdate /main/UUT/cmp_mil1553_rx_clk/s_period_c_reinit
add wave -noupdate /main/UUT/cmp_mil1553_rx_clk/s_period_c_is_full
add wave -noupdate /main/UUT/cmp_mil1553_rx_clk/s_manch_clk_d1
add wave -noupdate /main/UUT/cmp_mil1553_rx_clk/s_manch_clk
add wave -noupdate /main/UUT/cmp_mil1553_rx_clk/s_bit_clk_d1
add wave -noupdate /main/UUT/cmp_mil1553_rx_clk/s_bit_clk
add wave -noupdate /main/UUT/cmp_mil1553_rx_clk/s_adjac_bits_edge_found
add wave -noupdate /main/UUT/cmp_mil1553_rx_clk/rx_manch_clk_p_o
add wave -noupdate /main/UUT/cmp_mil1553_rx_clk/rx_bit_clk_p_o
add wave -noupdate /main/UUT/cmp_mil1553_rx_clk/rx_adjac_bits_window_o
add wave -noupdate /main/UUT/cmp_mil1553_rx_clk/rx_signif_edge_window_o
add wave -noupdate -divider {RX DESERIALISER}
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/sys_rst_n_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/sys_clk_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rxd_r_edge_p_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rxd_f_edge_p_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rxd_i
add wave -noupdate -radix hexadecimal /main/UUT/cmp_mil1553_rx_deserialiser/rxd_hist
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/signif_edge_window_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/adjac_bits_window_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/sample_manch_bit_p_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/sample_bit_p_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rx_fsm_state
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rx_fsm_next_state
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rx_is_idle
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/detecting_stat_sync
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/receiving_word
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/detecting_data_sync
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/manch_r_edge_p
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/manch_f_edge_p
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/stat_sync_detected
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/data_sync_detected
add wave -noupdate -radix unsigned /main/UUT/cmp_mil1553_rx_deserialiser/bit_cnt_value
add wave -noupdate -radix unsigned /main/UUT/cmp_mil1553_rx_deserialiser/bit_cnt_top
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/bit_cnt_load
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/bit_cnt_is_zero
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/bit_cnt_decr_p
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/arriving_stat_sync
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/arriving_data_sync
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rx_clk_rst_o
add wave -noupdate -radix unsigned /main/UUT/cmp_mil1553_rx_deserialiser/received_word
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/word_ready_p
add wave -noupdate -radix unsigned /main/UUT/cmp_mil1553_rx_deserialiser/word_cnt
add wave -noupdate -radix unsigned /main/UUT/cmp_mil1553_rx_deserialiser/rx_word_cnt_o
add wave -noupdate -radix unsigned -childformat {{/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(0) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(1) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(2) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(3) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(4) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(5) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(6) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(7) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(8) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(9) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(10) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(11) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(12) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(13) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(14) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(15) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(16) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(17) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(18) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(19) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(20) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(21) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(22) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(23) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(24) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(25) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(26) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(27) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(28) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(29) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(30) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(31) -radix unsigned} {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(32) -radix unsigned}} -subitemconfig {/main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(0) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(1) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(2) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(3) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(4) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(5) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(6) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(7) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(8) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(9) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(10) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(11) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(12) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(13) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(14) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(15) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(16) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(17) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(18) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(19) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(20) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(21) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(22) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(23) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(24) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(25) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(26) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(27) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(28) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(29) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(30) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(31) {-height 16 -radix unsigned} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o(32) {-height 16 -radix unsigned}} /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rx_in_progress_o
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rx_done_p_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {113875203 ps} 0}
configure wave -namecolwidth 450
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
WaveRestoreZoom {0 ps} {435843782 ps}
