`timescale 10fs/10fs

`include "simdrv_defs.svh"

interface IPCI9030
  (
   input LClk,
   input Rst
   );

   wire [31:0] LData ;
   logic        LReadyN;
   logic        LIntAN;
   logic    ADSN;
   logic [23:2] LA;
   logic [3:0]  LBE;
   logic        BlastN;
   logic        LWrRdN;
   logic        LData_out;


   

   modport fpga
     (
      inout  LData,
      input  LReadyN,
      input  LIntAN,
      output ADSN,
      output LA,
      output LBE,
      output BlastN,
      output LWrRdN
      );
   
   task init();
      LWrRdN     = 0;
      LA         =0;
      LBE = 'hf;
      BlastN = 1;
      ADSN = 1;
      LData_out = 0;
   endtask

   assign LData = (LWrRdN ? LData_out : 32'bz);
   
   initial init();
   always@(posedge Rst) init();
endinterface // IPCI9030

typedef virtual IPCI9030 VIPCI9030;

class CBusAccessor_PCI9030 extends CBusAccessor;
   VIPCI9030 vif;
   
   function new(VIPCI9030 _vif);
      vif = _vif;
   endfunction // new

   task read_single(uint64_t addr, ref uint64_t data);
      vif.LWrRdN     <= 1;
      vif.ADSN <= 1;
      @(posedge vif.LClk);

      vif.LA     <= addr >> 2;
      vif.LBE    <= 4'hf;
      vif.BlastN <= 1;
      vif.ADSN   <= 0;
      vif.LWrRdN <= 0;
      @(posedge vif.LClk);

      vif.ADSN <= 1;
      while(vif.LReadyN)
        @(posedge vif.LClk);
      
      data =vif.LData;
      @(posedge vif.LClk);
      vif.LWrRdN <= 1;
      @(posedge vif.LClk);
   endtask // read_single
   
   task write_single(uint64_t addr, uint64_t data);
      vif.ADSN   <= 1;
      vif.BlastN <= 1;
      vif.LWrRdN <= 1;
      vif.LData_out  <= data;

      @(posedge vif.LClk);

      vif.LA <= addr >> 2;
      vif.ADSN <= 0;
      vif.LBE  <= 4'b1111;
      @(posedge vif.LClk);

      vif.ADSN <= 1;
      while(vif.LReadyN)
        @(posedge vif.LClk);

      vif.BlastN <= 0;
      while(vif.LReadyN)
        @(posedge vif.LClk);
      
      @(posedge vif.LClk);
      
   endtask // write_single

   task readm(uint64_t addr[], ref uint64_t data[], input int size, ref int result);
      if(addr.size() != 1)
        $fatal("Sorry, no block reads/writes supported yet.");
      read_single(addr[0], data[0]);
   endtask // readm


    task writem(uint64_t addr[], uint64_t data[], input int size, ref int result);
      if(addr.size() != 1)
        $fatal("Sorry, no block reads/writes supported yet.");
      write_single(addr[0], data[0]);
    endtask // writem
endclass // CBusAccessor_PCI9030
      

 
module main;

   reg clk = 0;
   reg rst_n = 0;

   const time c_clock_period = 30ns;
   
   always #(c_clock_period/2) clk <= ~clk;
   initial begin
      repeat(3) @(posedge clk);
      rst_n = 1;
   end

   IPCI9030 U_PLX
     (
      .LClk(clk),
      .Rst(rst)
      );
   
   m1553nbciTop 
     DUT 
       ( .Rst(rst),
         .clk(clk),
         .M1553RXDataIn (),
         .M1553MDFault (),
         .M1553TX_RXN (),
         .M1553TXN (),
         .M1553TXDataOut(),
         .M1553TXDataOutN (),
         .Ledout(),
         .Xtest (),
         .RS232In (),
         .Rs232Out(),
         .SerialId(),
       
//         .LRead(),
//         .LWrite(),
//         .LReset(),
//         .LCS(),				
         .LAdSN(U_PLX.ADSN),
//         .LALE(),
         .LBLastN(U_PLX.BlastN),
//         .LBTermN(),
         .LA(U_PLX.LA),
         .LData(U_PLX.LData),
//         .LBE(U_PLX.LBE),
         .LWrRdN(U_PLX.LWrRdN),
         .LReadyN(U_PLX.LReadyN),
         .LInt1(),
         .LInt2(),
         .LGPIO()
         );

   initial begin
      uint64_t rv;
      #1us;
      
   end
   
      

endmodule // main

