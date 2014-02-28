`timescale 1ps/1ps

interface IMil1553Tx
  (
   input clk_i,
   output tx_a_o
   );

   logic  clk;
   logic  tx = 1'b1;

   assign clk=clk_i;
   assign tx_a_o=tx;

endinterface // m1553_async


typedef virtual IMil1553Tx VIMil1553Tx;


class CMil1553Tx;

   VIMil1553Tx mil;
   int          clock_speed;
   int          baud_rate;

   time         clock_period;
   time         bit_len;
   time         sync_len;
   time         frame_spacing;

   int          seed;


   function new (int _clock_speed, int _baud_rate, VIMil1553Tx _mil);
      clock_speed = _clock_speed;
      baud_rate = _baud_rate;

      bit_len = 1us;
      sync_len = 3us;
      frame_spacing = 10us;
      clock_period = 1s/_clock_speed;

      seed = 0;

      mil = _mil;
   endfunction // new

   task sync(ref logic waveform[$], input bit command);
      int sync_ticks = (sync_len / clock_period) / 2;
      int i;

      for(i=0;i<sync_ticks;i++)
        waveform.push_back(command);
      for(i=0;i<sync_ticks;i++)
        waveform.push_back(~command);
   endtask

   task sbit(ref logic waveform[$], input bit value);
      int bit_ticks = (bit_len / clock_period) / 2;
      int i;

      for(i=0;i<bit_ticks;i++)
        waveform.push_back(value);
      for(i=0;i<bit_ticks;i++)
        waveform.push_back(~value);
   endtask

   task sgap(ref logic waveform[$]);
      int gap_ticks = (frame_spacing / clock_period) / 2;
      int i;

      // After measuring the output of the differential receiver (75173) on the CBMIA
      // it seems to stay at '1' when the input diff voltage is close to 0V.
      for(i=0;i<gap_ticks;i++)
        waveform.push_back(1'b1);
   endtask // gap

   task add_glitches_edge(ref logic waveform[$], input int prob);
      int i, prev_glitch = 0;

      logic w_new[$] = waveform;


      for(i=2;i<waveform.size(); i++)
        begin
           w_new[i] = waveform[i];

           if($dist_uniform(seed, 0, 100) < prob && (waveform[i-1] != waveform[i])) begin
              w_new[i-2] = ~waveform[i-2];
           end
        end
      waveform = w_new;


   endtask // add_glitches

   task add_glitches_random(ref logic waveform[$], input int prob);
      int i;

      for(i=0;i<waveform.size(); i++)
        begin

           if($dist_uniform(seed, 0, 100) < prob) begin
              waveform[i] = ~waveform[i];
           end
        end

   endtask // add_glitches



   task send(int command, int value);
      logic w[$], t;
      int n_ones = 0, i;

      if(command)
        begin
           sgap(w);
        end
      sync(w, command);
//      add_glitches_random(w, 1);

      for(i=0;i<16;i++)
        begin
           t=value & (1<<(15-i)) ? 1'b1 : 1'b0 ;
           sbit(w, t);
           if(t) n_ones++;
        end

      sbit(w, ~(n_ones % 2));
      //sgap(w);

      i=$dist_uniform(seed, 0, w.size());
      //w[i] = ~w[i]; // add a random glitch somewhere


     // add_glitches(w, 80);

/* transmit the waveform */
       for(i=0;i<w.size(); i++)
        begin
           mil.tx <= w[i];
           @(posedge mil.clk);
        end

   endtask // send

endclass // CMil1553Tx


module main;

   reg clk = 0, clk_rx = 0;
   reg rst = 0;
   wire tx;
   wire l_ads;
   wire l_addr[23:2];
   wire l_data[31:0];
   wire l_rw;
   wire l_ready;
   wire l_int1;
   wire l_int2;

   always #25ns clk <= ~clk;
   always #25.01ns clk_rx <= ~clk_rx;


   IMil1553Tx PHY (clk, tx);

   logic [15:0] sent[$];

   initial begin
      repeat(10) @(posedge clk);
      rst = 1;
   end



   initial begin
      int i;

      CMil1553Tx phy_obj = new (40e6, 1e6, VIMil1553Tx'(PHY));

      #100000ns;

      sent.push_back(1);
      phy_obj.send(1, 1);

      #60000ns;

      sent.push_back(2);
      phy_obj.send(1, 2);
      for(i=1;i<33;i++)
        begin
           sent.push_back(i);
           phy_obj.send(0, i);
        end

      #60000ns;

      sent.push_back(2);
      phy_obj.send(1, 2);
      for(i=1;i<32;i++)
        begin
           sent.push_back(i);
           phy_obj.send(0, i);
        end

      #60000ns;

      sent.push_back(2);
      phy_obj.send(1, 2);
      for(i=1;i<2;i++)
        begin
           sent.push_back(i);
           phy_obj.send(0, i);
        end

   end


   cbmia_top
     UUT(
         .pwr_reset_n_i (rst),
         .clk_i (clk),
         .mil1553_rxd_a_i (tx),
         .l_ads_n_i (l_ads_n),
         .l_address_i (l_addr),
         .l_data_b (l_data),
         .l_ready_n_o (l_ready),
         .l_wr_rd_n_i (l_rw),
         .l_int1_o (l_int1),
         .l_int2_o (l_int2)
         );
   /*
         .mil1553_md_fault_i (),
         .mil1553_tx_rx_n_o (),
         .mil1553_tx_n_o (),
         .mil1553_txd_o (),
         .mil1553_txd_n_o (),
         .led_o (),
         .test_point_o (),
         .rs232_i (),
         .rs232_o (),
         .onewire_b (),
         .l_rd_n_i (),
         .l_wr_n_i (),
         .l_reset_n_i (),
         .l_cs_n_i (),
         .l_ale_i (),
         .l_blast_n_i (),
         .l_btrem_n_o (),
         .l_be_i (),
         .l_gpio_b (),
         .ram_address_o (),
         .ram_data_b (),
         .ram_par_o (),
         .ram_zz_o (),
         .ram_oe_n_o (),
         .ram_lbo_n_o (),
         .ram_gw_n_o (),
         .ram_ce_n_o (),
         .ram_cs0_o (),
         .ram_cs1_n_o (),
         .ram_bwe_n_o (),
         .ram_bw_n_o (),
         .ram_adv_n_o (),
         .ram_adsp_n_o (),
         .ram_adsc_n_o (),
         .ram_clk_o ()
         );
*/
endmodule // main
