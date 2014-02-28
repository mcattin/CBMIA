target = "xilinx"
action = "simulation"

files = ["receiver_tb.sv",
         "../../rtl/mem_interface_pkg.vhd",
         "../../rtl/cbmia_pkg.vhd",
         "../../rtl/incr_cnt.vhd",
         "../../rtl/decr_cnt.vhd",
         "../../rtl/mil1553_rx.vhd",
         "../../rtl/mil1553_rx_deglitcher.vhd",
         "../../rtl/mil1553_rx_deserialiser.vhd",
         "../../rtl/mil1553_rx_clk.vhd"]
