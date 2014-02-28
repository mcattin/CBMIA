onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /core_tb/uut_mil1553_core/pwr_reset_n_i
add wave -noupdate /core_tb/uut_mil1553_core/sw_rst_p
add wave -noupdate /core_tb/uut_mil1553_core/rst_n
add wave -noupdate /core_tb/uut_mil1553_core/sys_clk_i
add wave -noupdate -divider {Memory interface}
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/g_HW_VERSION
add wave -noupdate /core_tb/uut_mil1553_core/op_done_o
add wave -noupdate /core_tb/uut_mil1553_core/wr_to_mem_i
add wave -noupdate /core_tb/uut_mil1553_core/rd_to_mem_i
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/data_to_mem_i
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/addr_to_mem_i
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/data_from_mem_o
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/to_regs
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/from_regs
add wave -noupdate -divider Registers
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/irq_en_msk_reg
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/irq_src_reg
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/irq_src
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/irq_req_o
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/cmd_reg
add wave -noupdate -divider {MIL1553 interface}
add wave -noupdate /core_tb/uut_mil1553_core/mil1553_txd_o
add wave -noupdate /core_tb/uut_mil1553_core/mil1553_txd_n_o
add wave -noupdate /core_tb/uut_mil1553_core/mil1553_txd
add wave -noupdate /core_tb/uut_mil1553_core/mil1553_tx_rx_n_o
add wave -noupdate /core_tb/uut_mil1553_core/mil1553_tx_n_o
add wave -noupdate /core_tb/uut_mil1553_core/mil1553_tx_en
add wave -noupdate /core_tb/uut_mil1553_core/mil1553_rxd_a_i
add wave -noupdate /core_tb/uut_mil1553_core/mil1553_rx_en
add wave -noupdate -divider TX
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/tx_reg
add wave -noupdate /core_tb/uut_mil1553_core/tx_done_p
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/tx_buffer
add wave -noupdate /core_tb/uut_mil1553_core/tx_send_frame_p
add wave -noupdate -radix unsigned /core_tb/uut_mil1553_core/sent_frame_cnt
add wave -noupdate -divider RX
add wave -noupdate /core_tb/uut_mil1553_core/rx_in_progress
add wave -noupdate /core_tb/uut_mil1553_core/rx_done_p
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/rx_buffer_t
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/rx_buffer
add wave -noupdate -radix unsigned /core_tb/uut_mil1553_core/rx_word_cnt
add wave -noupdate -radix unsigned /core_tb/uut_mil1553_core/rx_rti
add wave -noupdate -radix hexadecimal /core_tb/uut_mil1553_core/rx_reg
add wave -noupdate -radix unsigned /core_tb/uut_mil1553_core/received_frame_cnt
add wave -noupdate -divider Transaction
add wave -noupdate /core_tb/uut_mil1553_core/send_frame_req_p
add wave -noupdate /core_tb/uut_mil1553_core/transaction_progress_d
add wave -noupdate /core_tb/uut_mil1553_core/transaction_progress
add wave -noupdate /core_tb/uut_mil1553_core/transaction_end_p
add wave -noupdate /core_tb/uut_mil1553_core/resp_timeout_p
add wave -noupdate /core_tb/uut_mil1553_core/resp_timeout_cnt_en
add wave -noupdate -radix unsigned /core_tb/uut_mil1553_core/resp_timeout_cnt
add wave -noupdate -divider {Error counters}
add wave -noupdate /core_tb/uut_mil1553_core/rx_parity_error_p
add wave -noupdate /core_tb/uut_mil1553_core/rx_nb_word_error_p
add wave -noupdate -radix unsigned /core_tb/uut_mil1553_core/rx_nb_word_error_cnt
add wave -noupdate /core_tb/uut_mil1553_core/rx_manch_error_p
add wave -noupdate /core_tb/uut_mil1553_core/req_during_trans_p
add wave -noupdate -radix unsigned /core_tb/uut_mil1553_core/req_during_trans_cnt
add wave -noupdate /core_tb/uut_mil1553_core/test_point_o
add wave -noupdate /core_tb/uut_mil1553_core/onewire_b
add wave -noupdate /core_tb/uut_mil1553_core/led_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {299380165 ps} 0}
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
