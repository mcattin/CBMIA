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
add wave -noupdate /main/UUT/rx_word_error_o
add wave -noupdate /main/UUT/rx_in_progress_o
add wave -noupdate /main/UUT/rx_glitch_detect_o
add wave -noupdate /main/UUT/rx_done_p_o
add wave -noupdate /main/UUT/rx_clk_rst
add wave -noupdate -radix hexadecimal /main/UUT/rx_buffer_o
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
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rxd_hist
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rxd_stored
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rxd_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/signif_edge_window_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/adjac_bits_window_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/sample_manch_bit_p_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/sample_bit_p_i
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/edge_in_manch_window_p
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
add wave -noupdate -radix hexadecimal /main/UUT/cmp_mil1553_rx_deserialiser/received_word
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/word_ready_p
add wave -noupdate -radix hexadecimal /main/UUT/cmp_mil1553_rx_deserialiser/rx_buffer_o
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rx_in_progress_o
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rx_done_p_o
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rx_glitch_detect_o
add wave -noupdate /main/UUT/cmp_mil1553_rx_deserialiser/rx_word_error_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {149375000 ps} 0}
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
WaveRestoreZoom {74796773 ps} {223953227 ps}
