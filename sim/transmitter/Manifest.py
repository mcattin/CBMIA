target = "xilinx"
action = "simulation"

files = ["transmitter_tb.vhd",
         "../../rtl/mem_interface_pkg.vhd",
         "../../rtl/cbmia_pkg.vhd",
         "../../rtl/mil1553_tx.vhd",
         "../../rtl/mil1553_tx_serialiser.vhd",
         "../../rtl/mil1553_tx_clk.vhd"]
