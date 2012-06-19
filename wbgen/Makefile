WBGEN2=~/projects/wbgen2/wbgen2
RTL=../rtl/wishbone/

cbmia_regs:
	 $(WBGEN2) -l vhdl -V $(RTL)$@.vhd -D $@.htm -C $@.h $@.wb -p $@_pkg.vhd

