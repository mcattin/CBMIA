`timescale 1ps/1ps

interface IMil1553Tx
  (
   input clk_i,
   output tx_a_o,
   output [5:0]nb_words_o
   );

   logic  clk;
   logic  tx = 1'b1;
   logic  [5:0]nb_words = 0;

   assign clk=clk_i;
   assign tx_a_o=tx;
   assign nb_words_o=nb_words;

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
   endtask // sbit

   task sbit_err(ref logic waveform[$], input bit value);
      int bit_ticks = (bit_len / clock_period) / 2;
      int i;

      for(i=0;i<bit_ticks;i++)
        waveform.push_back(value);
      for(i=0;i<bit_ticks;i++)
        waveform.push_back(value);
   endtask

   task sgap(ref logic waveform[$]);
      int gap_ticks = (frame_spacing / clock_period) / 2;
      int i;

      // After measuring the output of the differential receiver (75173) on the CBMIA
      // it seems to stay at '1' when the input diff voltage is close to 0V.
      for(i=0;i<gap_ticks;i++)
        waveform.push_back(1'b1);
   endtask // gap

   task pull_up();
      // set bus to high ('1')
      mil.tx = 1'b1;
   endtask // shigh

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



   task send(int command, int value, int words, int manch_err = 0, int manch_err_pos = 0, int trail_sync = 0);
      logic w[$], t;
      int n_ones = 0, i;

      @(posedge mil.clk) mil.nb_words <= words;

      if(command)
        begin
           sgap(w);
        end
      sync(w, command);
//      add_glitches_random(w, 1);

      for(i=0;i<16;i++)
        begin
           t=value & (1<<(15-i)) ? 1'b1 : 1'b0 ;
           if(manch_err != 0 && manch_err_pos == i)
             sbit_err(w, t);
           else
             sbit(w, t);
           if(t) n_ones++;
        end

      sbit(w, ~(n_ones % 2));

      begin
         if(!command && trail_sync)
           sync(w, command);
        end
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
   wire [5:0]nb_words;


   always #25ns clk <= ~clk;
   always #25.01ns clk_rx <= ~clk_rx;


   IMil1553Tx PHY (clk, tx, nb_words);

   logic [15:0] sent[$];

   initial begin
      repeat(10) @(posedge clk);
      rst = 1;
   end



   initial begin
      int i;

      CMil1553Tx phy_obj = new (40e6, 1e6, VIMil1553Tx'(PHY));

      #100000ns;

      phy_obj.send(1, 3, 3);
      phy_obj.send(0, 1024, 3);
      phy_obj.send(0, 131071, 3);
      phy_obj.pull_up();

      #600000ns;

      phy_obj.send(1, 10, 2);
      phy_obj.send(0, 1024, 2, 1, 10);
      phy_obj.pull_up();

      #600000ns;

      phy_obj.send(1, 10, 2);
      phy_obj.send(0, 1024, 2, 0, 0, 1);
      phy_obj.pull_up();

      #600000ns;

      phy_obj.send(1, 10, 2);
      phy_obj.send(0, 1024, 2, 1, 10, 1);
      phy_obj.pull_up();

      #600000ns;

      phy_obj.send(1, 10, 1);
      phy_obj.pull_up();

      #600000ns;

      phy_obj.send(1, 2, 33);
      for(i=1;i<33;i++)
        begin
           phy_obj.send(0, i, 33);
        end
      phy_obj.pull_up();

      #60000ns;

      phy_obj.send(1, 2, 32);
      for(i=1;i<32;i++)
        begin
           phy_obj.send(0, i, 32);
        end
      phy_obj.pull_up();

      #60000ns;

      phy_obj.send(1, 2, 2);
      for(i=1;i<2;i++)
        begin
           phy_obj.send(0, i, 2);
        end
      phy_obj.pull_up();

      #60000ns;

      phy_obj.send(1, 2, 10);
      for(i=1;i<10;i++)
        begin
           phy_obj.send(0, i, 10);
        end
      phy_obj.pull_up();

      #60000ns;

      phy_obj.send(1, 2, 32);
      for(i=1;i<10;i++)
        begin
           phy_obj.send(0, i, 32);
        end
      phy_obj.pull_up();

/*
      for(i=0;i<5;i++)
        begin
           sent.push_back(i);
           phy_obj.send(1, i);
        end
*/
   end

   wire rx_buffer[0:32][15:0];
   wire rx_in_progress;
   wire rx_done;
   wire rx_parity_error;
   wire rx_manch_error;
   wire rx_word_cnt[5:0];
   wire rx_error;

   int         rx_xfer = 0;

/*
   always@(posedge clk_rx)
     if(d_rdy)
       begin
          automatic int s = sent.pop_front();
          if(d_out != s)
            begin
               $display("Error: xfer %d rx %x should be %x", rx_xfer, d_out, s);

            end
          else
            begin
               $display("OK: xfer %d tx %x -> rx %x", rx_xfer, s, d_out);
            end
          rx_xfer++;
       end
*/

   mil1553_rx
   UUT (
        .sys_rst_n_i (rst),
        .sys_clk_i (clk),
        .mil1553_rxd_i (tx),
        .mil1553_rx_en_i (1'b1),
        .expected_nb_word_i (nb_words),
        .rx_buffer_o (rx_buffer),
        .rx_word_cnt_o (rx_word_cnt),
        .rx_in_progress_o (rx_in_progress),
        .rx_done_p_o (rx_done),
        .rx_parity_error_p_o (rx_parity_error),
        .rx_manch_error_p_o (rx_manch_error),
        .rx_error_p_o (rx_error)
        );


endmodule // main
