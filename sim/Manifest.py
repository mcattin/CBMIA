target = "xilinx"
action = "simulation"

files = ["mil1553_tx_tb.vhd",
         "../rtl/cbmia_pkg.vhd",
         "../rtl/mil1553_tx.vhd",
         "../rtl/mil1553_tx_serialiser.vhd",
         "../rtl/mil1553_tx_clk.vhd"]
