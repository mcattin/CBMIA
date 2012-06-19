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


entity mil1553_rx_deserialiser is

  port (

    -- Global ports
    ----------------------------------------------------------------------------
    sys_rst_n_i : in std_logic;         -- Synchronous system reset (active low)
    sys_clk_i   : in std_logic;         -- System clock

    -- MIL1553 interface
    ----------------------------------------------------------------------------
    rxd_i                : in  std_logic;  -- Serial data input
    rxd_f_edge_i         : in  std_logic;  -- Indicates a falling edge on serial input
    rxd_r_edge_i         : in  std_logic;  -- Indicates a rising edge on serial input
    sample_bit_i         : in  std_logic;  -- Pulse indicating the sampling of a bit
    sample_manch_bit_i   : in  std_logic;  -- Pulse indicating the sampling of a Manchester bit
    signif_edge_window_i : in  std_logic;  -- Time window where a significant edge is expected
    adjac_bits_window_i  : in  std_logic;  -- Time window where a transition between adjacent
                                           -- bits is expected
    rx_clk_rst_o         : out std_logic;  -- Resets the clk recovery procedure

    -- User interface
    ----------------------------------------------------------------------------
    rx_word_o          : out t_tx_buffer_array;  -- Receive buffer
    rx_in_progress_o   : out std_logic;          -- Frame reception in progress
    rx_done_o          : out std_logic;          -- End of frame reception
    rx_glitch_detect_o : out std_logic;          -- Glitch detected in serial data
    rx_word_error_o    : out std_logic           -- Received word contains error (parity error, code violation)

    );

end mil1553_rx_deserialiser;


architecture rtl of mil1553_rx_deserialiser is

  type t_rx_fsm_state is (RX_IDLE, RX_SYNC_DETECT, RX_GET_BITS, RX_DATA_SYNC, RX_ERROR);

  signal rx_fsm_state      : t_rx_fsm_state;
  signal rx_fsm_next_state : t_rx_fsm_state;

  signal detecting_sync : std_logic;
  signal sync_detected  : std_logic;
  signal arriving_sync  : std_logic_vector(1 downto 0);

begin


  ------------------------------------------------------------------------------
  -- Receiver FSM
  ------------------------------------------------------------------------------
  p_rx_fsm_sync : process (sys_clk_i)
  begin
    if rising_edge (sys_clk_i) then
      if sys_rst_n_i = '0' then
        rx_fsm_state <= IDLE;
      else
        rx_fsm_state <= rx_fsm_next_state;
      end if;
    end if;
  end process p_rx_fsm_sync;

  p_rx_fsm_transitions : process
  begin
    case rx_fsm_state is

      when RX_IDLE =>

        if rxd_f_edge_i = '1' then
          rx_fsm_next_state <= RX_SYNC_DETECT;
        else
          rx_fsm_next_state <= RX_IDLE;
        end if;


      when RX_SYNC_DETECT =>

        if rxd_f_edge_i = '1' or rxd_r_edge_i = '1' then
          rx_fsm_next_state <= RX_ERROR;
        elsif sync_detected = '1' then
          rx_fsm_next_state <= RX_GET_BITS;
        else
          rx_fsm_next_state <= RX_SYNC_DETECT;
        end if;


      when RX_GET_BITS =>

        if bit_cnt_is_zero = '1' and sample_manch_bit_i = '1' then
          rx_fsm_next_state <= RX_DATA_SYNC;
        elsif edge outside windows then
          rx_fsm_next_state <= RX_ERROR;
        else
          rx_fsm_next_state <= RX_GET_BITS;
        end if;


      when RX_DATA_SYNC =>

        


      when RX_ERROR =>

        rx_fsm_next_state <= RX_IDLE;


      when others =>

        rx_fsm_next_state <= RX_IDLE;


    end case;
  end process p_rx_fsm_transitions;

  p_rx_fsm_outputs : process
  begin
    case rx_fsm_state is

      when RX_IDLE =>

        detecting_sync      <= '0';
        rx_idle             <= '1';
        receiving_word      <= '0';
        detecting_data_sync <= '0';


      when RX_SYNC_DETECT =>

        detecting_sync      <= '1';
        rx_idle             <= '0';
        receiving_word      <= '0';
        detecting_data_sync <= '0';


      when RX_GET_BITS =>

        detecting_sync      <= '0';
        rx_idle             <= '0';
        receiving_word      <= '1';
        detecting_data_sync <= '0';


      when RX_DATA_SYNC =>

        detecting_sync      <= '0';
        rx_idle             <= '0';
        receiving_word      <= '0';
        detecting_data_sync <= '1';


      when RX_ERROR =>

        detecting_sync      <= '0';
        rx_idle             <= '0';
        receiving_word      <= '0';
        detecting_data_sync <= '0';


      when others =>

        detecting_sync      <= '0';
        rx_idle             <= '0';
        receiving_word      <= '0';
        detecting_data_sync <= '0';


    end case;
  end process p_rx_fsm_outputs;

  ------------------------------------------------------------------------------
  -- Sync pattern detection
  ------------------------------------------------------------------------------
  p_sync_detect : process (sys_clk_i)
  begin
    if rising_edge (sys_clk_i) then
      if sys_rst_n_i = '0' then
        arriving_sync <= (others => '0');
      elsif detecting_sync = '1' and sample_manch_bit_i = '1' then
        arriving_sync <= arriving_sync(0) & rxd_i;
      end if;
    end if;
  end process p_sync_detect;

  sync_detected <= '1' when arriving_sync = c_STAT_SYNC_FIELD else '0';


  ------------------------------------------------------------------------------
  -- Shift register for data sampling and deserialising
  -----------------------------------------------------------------------------
  p_data_shift_reg : process (sys_clk_i)
  begin
    if rising_edge (sys_clk_i) then
      if sys_rst_n_i = '0' then
        received_word <= (others => '0');
      elsif receiving_word = '1' and sample_bit_i = '1' then
        receiving_word <= receiving_word(30 downto 0) & rxd_i;
      end if;
    end if;
  end process p_data_shift_reg;

  ------------------------------------------------------------------------------
  -- 
  ------------------------------------------------------------------------------
  cmp_bit_cnt : decr_cnt
    generic map(
      g_counter_lgth => 4
      )
    port map(
      sys_clk_i         => sys_clk_i,
      sys_rst_n_i       => sys_rst_n_i,
      counter_top_i     => bit_cnt_top,
      counter_load_i    => bit_cnt_load_p,
      counter_decr_i    => bit_cnt_decr_p,
      counter_o         => bit_cnt_value,
      counter_is_zero_o => bit_cnt_is_zero
      );

  bit_cnt_top <= std_logic_vector(to_unsigned(18, bit_cnt_top'length))

  bit_cnt_load_p <= '1' when receiving_word = '0' else '0';

  bit_cnt_decr_p <= sample_bit_i when receiving_word = '1' else '0';

  ------------------------------------------------------------------------------
  -- 
  ------------------------------------------------------------------------------

  rx_clk_rst_o <= rx_idle;


end architecture rtl;
