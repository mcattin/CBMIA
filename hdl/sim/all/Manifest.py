target = "xilinx"
action = "simulation"

files = ["all_tb.sv",
         "cbmia_top.vhd",
         "../../rtl/cbmia_pkg.vhd",
         "../../rtl/decr_cnt.vhd",
         "../../rtl/incr_cnt.vhd",
         "../../rtl/irq_regs.vhd",
         "../../rtl/mem_interface_mux.vhd",
         "../../rtl/mem_interface_pkg.vhd",
         "../../rtl/mem_interface.vhd",
         "../../rtl/mil1553_core.vhd",
         "../../rtl/mil1553_rx_clk.vhd",
         "../../rtl/mil1553_rx_deglitcher.vhd",
         "../../rtl/mil1553_rx_deserialiser.vhd",
         "../../rtl/mil1553_rx.vhd",
         "../../rtl/mil1553_tx_clk.vhd",
         "../../rtl/mil1553_tx_serialiser.vhd",
         "../../rtl/mil1553_tx.vhd",
         "../../rtl/monostable.vhd",
         "../../rtl/one_wire_ds1822.vhd",
         "../../rtl/plx_to_mem_interface.vhd"]
