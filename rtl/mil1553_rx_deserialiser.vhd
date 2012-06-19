-------------------------------------------------------------------------------
-- Title      : MIL1553 deserialiser
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mil1553_rx_deserialiser.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-02-29
-- Last update: 2012-03-14
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
--
-- Copyright (c) 2009 - 2010 CERN
--
-- This source file is free software; you can redistribute it
-- and/or modify it under the terms of the GNU Lesser General
-- Public License as published by the Free Software Foundation;
-- either version 2.1 of the License, or (at your option) any
-- later version.
--
-- This source is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE.  See the GNU Lesser General Public License for more
-- details.
--
-- You should have received a copy of the GNU Lesser General
-- Public License along with this source; if not, download it
-- from http://www.gnu.org/licenses/lgpl-2.1.html
--
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2012-02-29  1.0      mcattin         Created
-------------------------------------------------------------------------------
-- TODO: - 
--       - 
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
library work;
use work.cbmia_pkg.all;


entity mil1553_rx_deserialiser is

  port (

    -- Global ports
    ----------------------------------------------------------------------------
    sys_rst_n_i : in std_logic;         -- Synchronous system reset (active low)
    sys_clk_i   : in std_logic;         -- System clock

    -- MIL1553 interface
    ----------------------------------------------------------------------------
    rxd_i                : in  std_logic;  -- Serial data input
    rxd_f_edge_p_i       : in  std_logic;  -- Indicates a falling edge on serial input
    rxd_r_edge_p_i       : in  std_logic;  -- Indicates a rising edge on serial input
    sample_bit_p_i       : in  std_logic;  -- Pulse indicating the sampling of a bit
    sample_manch_bit_p_i : in  std_logic;  -- Pulse indicating the sampling of a Manchester bit
    signif_edge_window_i : in  std_logic;  -- Time window where a significant edge is expected
    adjac_bits_window_i  : in  std_logic;  -- Time window where a transition between adjacent
                                           -- bits is expected
    rx_clk_rst_o         : out std_logic;  -- Resets the clk recovery procedure

    -- User interface
    ----------------------------------------------------------------------------
    rx_buffer_o        : out t_rx_buffer_array;  -- Receive buffer
    rx_in_progress_o   : out std_logic;          -- Frame reception in progress
    rx_done_p_o        : out std_logic;          -- End of frame reception
    rx_glitch_detect_o : out std_logic;          -- Glitch detected in serial data
    rx_word_error_o    : out std_logic           -- Received word contains error (parity error, code violation)

    );

end mil1553_rx_deserialiser;


architecture rtl of mil1553_rx_deserialiser is

  type t_rx_fsm_state is (RX_IDLE, RX_SYNC_DETECT, RX_GET_BITS, RX_MANCH_EDGE, RX_DATA_SYNC, RX_ERROR);

  signal rx_fsm_state      : t_rx_fsm_state;
  signal rx_fsm_next_state : t_rx_fsm_state;

  signal manch_r_edge_p : std_logic;
  signal manch_f_edge_p : std_logic;
  signal manch_edge_p   : std_logic;

  signal detecting_stat_sync : std_logic;
  signal rx_is_idle          : std_logic;
  signal receiving_word      : std_logic;
  signal detecting_data_sync : std_logic;

  --signal rxd_hist   : std_logic_vector(63 downto 0);
  --signal rxd_stored : std_logic_vector(63 downto 0);

  signal stat_sync_detected : std_logic;
  signal arriving_stat_sync : std_logic_vector(1 downto 0);

  signal bit_cnt_top     : std_logic_vector(4 downto 0);
  signal bit_cnt_load    : std_logic;
  signal bit_cnt_decr_p  : std_logic;
  signal bit_cnt_value   : std_logic_vector(4 downto 0);
  signal bit_cnt_is_zero : std_logic;

  signal data_sync_detected : std_logic;
  signal arriving_data_sync : std_logic_vector(5 downto 0);

  signal received_word : std_logic_vector(16 downto 0);
  signal word_ready_p  : std_logic;
  signal parity_ok     : std_logic;


begin


  ------------------------------------------------------------------------------
  -- Edges in windows
  ------------------------------------------------------------------------------
  manch_r_edge_p <= rxd_r_edge_p_i and signif_edge_window_i;
  manch_f_edge_p <= rxd_f_edge_p_i and signif_edge_window_i;
  manch_edge_p   <= signif_edge_window_i and (rxd_r_edge_p_i or rxd_f_edge_p_i);

  ------------------------------------------------------------------------------
  -- Receiver FSM
  ------------------------------------------------------------------------------
  p_rx_fsm_sync : process (sys_clk_i)
  begin
    if rising_edge (sys_clk_i) then
      if sys_rst_n_i = '0' then
        rx_fsm_state <= RX_IDLE;
      else
        rx_fsm_state <= rx_fsm_next_state;
      end if;
    end if;
  end process p_rx_fsm_sync;

  p_rx_fsm_transitions : process(rx_fsm_state, rxd_f_edge_p_i, rxd_r_edge_p_i,
                                 stat_sync_detected, bit_cnt_is_zero,
                                 sample_manch_bit_p_i, sample_bit_p_i,
                                 data_sync_detected, manch_edge_p)
  begin
    case rx_fsm_state is

      when RX_IDLE =>

        if rxd_f_edge_p_i = '1' then
          rx_fsm_next_state <= RX_SYNC_DETECT;
        else
          rx_fsm_next_state <= RX_IDLE;
        end if;


      when RX_SYNC_DETECT =>

        if rxd_f_edge_p_i = '1' or rxd_r_edge_p_i = '1' then
          rx_fsm_next_state <= RX_ERROR;
        elsif stat_sync_detected = '1' then
          rx_fsm_next_state <= RX_GET_BITS;
        else
          rx_fsm_next_state <= RX_SYNC_DETECT;
        end if;


      when RX_GET_BITS =>

        if bit_cnt_is_zero = '1' and sample_manch_bit_p_i = '1' then
          rx_fsm_next_state <= RX_DATA_SYNC;
          --elsif edge outside windows then
          --  rx_fsm_next_state <= RX_ERROR;
        elsif sample_bit_p_i = '1' then
          rx_fsm_next_state <= RX_MANCH_EDGE;
        end if;


      when RX_MANCH_EDGE =>

        if manch_edge_p = '1' then
          rx_fsm_next_state <= RX_GET_BITS;
        elsif sample_manch_bit_p_i = '1' then
          rx_fsm_next_state <= RX_ERROR;
        else
          rx_fsm_next_state <= RX_MANCH_EDGE;
        end if;


      when RX_DATA_SYNC =>

        if data_sync_detected = '1' then
          rx_fsm_next_state <= RX_GET_BITS;
          --elsif edge outside signif window then
          --  rx_fsm_next_state <= RX_ERROR;
        elsif bit_cnt_is_zero = '1' and sample_bit_p_i = '1' then
          rx_fsm_next_state <= RX_IDLE;
        else
          rx_fsm_next_state <= RX_DATA_SYNC;
        end if;


      when RX_ERROR =>

        rx_fsm_next_state <= RX_IDLE;


      when others =>

        rx_fsm_next_state <= RX_IDLE;


    end case;
  end process p_rx_fsm_transitions;

  p_rx_fsm_outputs : process(rx_fsm_state)
  begin
    case rx_fsm_state is

      when RX_IDLE =>

        detecting_stat_sync <= '0';
        rx_is_idle          <= '1';
        receiving_word      <= '0';
        detecting_data_sync <= '0';


      when RX_SYNC_DETECT =>

        detecting_stat_sync <= '1';
        rx_is_idle          <= '0';
        receiving_word      <= '0';
        detecting_data_sync <= '0';


      when RX_GET_BITS =>

        detecting_stat_sync <= '0';
        rx_is_idle          <= '0';
        receiving_word      <= '1';
        detecting_data_sync <= '0';


      when RX_MANCH_EDGE =>

        detecting_stat_sync <= '0';
        rx_is_idle          <= '0';
        receiving_word      <= '1';
        detecting_data_sync <= '0';


      when RX_DATA_SYNC =>

        detecting_stat_sync <= '0';
        rx_is_idle          <= '0';
        receiving_word      <= '0';
        detecting_data_sync <= '1';


      when RX_ERROR =>

        detecting_stat_sync <= '0';
        rx_is_idle          <= '0';
        receiving_word      <= '0';
        detecting_data_sync <= '0';


      when others =>

        detecting_stat_sync <= '0';
        rx_is_idle          <= '0';
        receiving_word      <= '0';
        detecting_data_sync <= '0';


    end case;
  end process p_rx_fsm_outputs;

  ------------------------------------------------------------------------------
  -- Store serial input state for sync pattern detection
  ------------------------------------------------------------------------------
  --p_rxd_store : process (sys_clk_i)
  --begin
  --  if rising_edge (sys_clk_i) then
  --    if sys_rst_n_i = '0' then
  --      rxd_hist   <= (others => '0');
  --      rxd_stored <= (others => '0');
  --    else
  --      rxd_hist <= rxd_hist(62 downto 0) & rxd_i;

  --      if rx_is_idle = '1' and rxd_f_edge_p_i = '1' then
  --        rxd_stored <= rxd_hist;
  --      elsif detecting_data_sync = '1' and manch_r_edge_p = '1' then
  --        rxd_stored <= rxd_hist;
  --      end if;
  --    end if;
  --  end if;
  --end process p_rxd_store;

  ------------------------------------------------------------------------------
  -- Status word sync pattern detection
  ------------------------------------------------------------------------------
  p_stat_sync_detect : process (sys_clk_i)
  begin
    if rising_edge (sys_clk_i) then
      if sys_rst_n_i = '0' then
        arriving_stat_sync <= (others => '1');
      else
        if detecting_stat_sync = '0' then
          arriving_stat_sync <= (others => '1');
        elsif detecting_stat_sync = '1' and sample_manch_bit_p_i = '1' then
          arriving_stat_sync <= arriving_stat_sync(0) & rxd_i;
        end if;
      end if;
    end if;
  end process p_stat_sync_detect;

  stat_sync_detected <= '1' when arriving_stat_sync = c_STAT_SYNC_FIELD else '0';

  ------------------------------------------------------------------------------
  -- Bit counter
  ------------------------------------------------------------------------------
  cmp_bit_cnt : decr_cnt
    generic map(
      g_COUNTER_WIDTH => 5
      )
    port map(
      sys_clk_i         => sys_clk_i,
      sys_rst_n_i       => sys_rst_n_i,
      counter_top_i     => bit_cnt_top,
      counter_load_i    => bit_cnt_load,
      counter_decr_i    => bit_cnt_decr_p,
      counter_o         => bit_cnt_value,
      counter_is_zero_o => bit_cnt_is_zero
      );

  bit_cnt_top <= std_logic_vector(to_unsigned(3, bit_cnt_top'length)) when receiving_word = '1' and bit_cnt_is_zero = '1' else
                 std_logic_vector(to_unsigned(17, bit_cnt_top'length));

  bit_cnt_load <= '1' when detecting_stat_sync = '1' else
                  data_sync_detected   when (detecting_data_sync = '1') and bit_cnt_is_zero = '1' else
                  sample_manch_bit_p_i when (receiving_word = '1') and bit_cnt_is_zero = '1'      else
                  '0';

  bit_cnt_decr_p <= sample_bit_p_i;

  ------------------------------------------------------------------------------
  -- Data word sync pattern detection
  ------------------------------------------------------------------------------
  p_data_sync_detect : process (sys_clk_i)
  begin
    if rising_edge (sys_clk_i) then
      if sys_rst_n_i = '0' then
        arriving_data_sync <= (others => '1');
      else
        if detecting_data_sync = '0' then
          arriving_data_sync <= (others => '1');
        elsif detecting_data_sync = '1' and sample_manch_bit_p_i = '1' then
          arriving_data_sync <= arriving_data_sync(4 downto 0) & rxd_i;
        end if;
      end if;
    end if;
  end process p_data_sync_detect;

  data_sync_detected <= '1' when arriving_data_sync = c_DATA_SYNC_FIELD else '0';

  ------------------------------------------------------------------------------
  -- Shift register for data sampling and deserialising
  ------------------------------------------------------------------------------
  p_data_shift_reg : process (sys_clk_i)
  begin
    if rising_edge (sys_clk_i) then
      if sys_rst_n_i = '0' then
        received_word <= (others => '0');
      elsif receiving_word = '1' and sample_bit_p_i = '1' then
        received_word <= received_word(15 downto 0) & rxd_i;
      end if;
    end if;
  end process p_data_shift_reg;

  word_ready_p <= receiving_word and bit_cnt_is_zero and sample_manch_bit_p_i;
  parity_ok <= f_parity_check(received_word);

  ------------------------------------------------------------------------------
  -- 
  ------------------------------------------------------------------------------


  ------------------------------------------------------------------------------
  -- 
  ------------------------------------------------------------------------------
  rx_clk_rst_o       <= rx_is_idle;
  rx_buffer_o        <= (others => (others => '0'));
  rx_done_p_o        <= '0';
  rx_in_progress_o   <= '0';
  rx_glitch_detect_o <= '0';
  rx_word_error_o    <= '0';


end architecture rtl;