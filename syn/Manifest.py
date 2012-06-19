target = "xilinx"
action = "synthesis"

syn_device = "xc3s1500"
syn_grade = "-4"
syn_package = "fg456"
syn_top = "cbmia_top"
syn_project = "cbmia_top.xise"

files = ["../cbmia_top.ucf"]

modules = { "local" : "../rtl" }
