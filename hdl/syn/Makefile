########################################
#  This file was generated by hdlmake  #
#  http://ohwr.org/projects/hdl-make/  #
########################################

PROJECT := cbmia_top.xise
ISE_CRAP := *.b cbmia_top_summary.html *.tcl cbmia_top.bld cbmia_top.cmd_log *.drc cbmia_top.lso *.ncd cbmia_top.ngc cbmia_top.ngd cbmia_top.ngr cbmia_top.pad cbmia_top.par cbmia_top.pcf cbmia_top.prj cbmia_top.ptwx cbmia_top.stx cbmia_top.syr cbmia_top.twr cbmia_top.twx cbmia_top.gise cbmia_top.unroutes cbmia_top.ut cbmia_top.xpi cbmia_top.xst cbmia_top_bitgen.xwbt cbmia_top_envsettings.html cbmia_top_guide.ncd cbmia_top_map.map cbmia_top_map.mrp cbmia_top_map.ncd cbmia_top_map.ngm cbmia_top_map.xrpt cbmia_top_ngdbuild.xrpt cbmia_top_pad.csv cbmia_top_pad.txt cbmia_top_par.xrpt cbmia_top_summary.xml cbmia_top_usage.xml cbmia_top_xst.xrpt usage_statistics_webtalk.html webtalk.log webtalk_pn.xml run.tcl

#target for performing local synthesis
local:
		echo "project open $(PROJECT)" > run.tcl
		echo "process run {Generate Programming File} -force rerun_all" >> run.tcl
		xtclsh run.tcl


#target for cleaing all intermediate stuff
clean:
		rm -f $(ISE_CRAP)
		rm -rf xst xlnx_auto_*_xdb iseconfig _xmsgs _ngo
    
#target for cleaning final files
mrproper:
		rm -f *.bit *.bin *.mcs

USER:=pawel
SERVER:=htsynth
R_NAME:=cbmia_top

__test_for_remote_synthesis_variables:
		true #dummy

CWD := $(shell pwd)

FILES := ../cbmia_top.ucf \
../rtl/cbmia_top.vhd \
../rtl/cbmia_pkg.vhd \
../rtl/decr_cnt.vhd \
../rtl/incr_cnt.vhd \
../rtl/irq_regs.vhd \
../rtl/mem_interface_mux.vhd \
../rtl/mem_interface_pkg.vhd \
../rtl/mem_interface.vhd \
../rtl/mil1553_core.vhd \
../rtl/mil1553_rx_clk.vhd \
../rtl/mil1553_rx_deglitcher.vhd \
../rtl/mil1553_rx_deserialiser.vhd \
../rtl/mil1553_rx.vhd \
../rtl/mil1553_tx_clk.vhd \
../rtl/mil1553_tx_serialiser.vhd \
../rtl/mil1553_tx.vhd \
../rtl/monostable.vhd \
../rtl/one_wire_ds1822.vhd \
../rtl/plx_to_mem_interface.vhd \
run.tcl \
cbmia_top.xise

#target for running simulation in the remote location
remote: __test_for_remote_synthesis_variables __send __do_synthesis __send_back
__send_back: __do_synthesis
__do_synthesis: __send
__send: __test_for_remote_synthesis_variables

__send:
		ssh $(USER)@$(SERVER) 'mkdir -p $(R_NAME)'
		rsync -Rav $(foreach file, $(FILES), $(shell readlink -f $(file))) $(USER)@$(SERVER):$(R_NAME)

__do_synthesis:
		ssh $(USER)@$(SERVER) 'cd $(R_NAME)$(CWD) && xtclsh run.tcl'

__send_back: 
		cd .. && rsync -av $(USER)@$(SERVER):$(R_NAME)$(CWD) . && cd $(CWD)

#target for removing stuff from the remote location
cleanremote:
		ssh $(USER)@$(SERVER) 'rm -rf $(R_NAME)'

