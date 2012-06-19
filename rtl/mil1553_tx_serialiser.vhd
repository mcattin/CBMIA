-------------------------------------------------------------------------------
-- Title      : MIL1553 serialiser
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mil1553_tx_serialiser.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-03-08
-- Last update: 2012-03-29
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: Adds odd parity, encodes in Manchester and transmits
--              words from TX buffer.
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
-- 2012-03-08  1.0      mcattin         Created
-------------------------------------------------------------------------------
-- TODO: - 
--       - 
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
library work;
use work.cbmia_pkg.all;


entity mil1553_tx_serialiser is
  port (

    -- Global ports
    ----------------------------------------------------------------------------
    sys_rst_n_i : in std_logic;         -- Synchronous system reset (active low)
    sys_clk_i   : in std_logic;         -- System clock

    -- MIL1553 interface
    ----------------------------------------------------------------------------
    mil1553_txd_o   : out std_logic;    -- Serial data output
    mil1553_tx_en_o : out std_logic;    -- Serial data output enable (transmission in progress)

    -- User interface
    ----------------------------------------------------------------------------
    tx_bit_rate_p_i   : in  std_logic;          -- Bit rate pulse train, two pulses per bit
                                                -- 1Mb/s => 2 MHz pulse train
    tx_buffer_i       : in  t_tx_buffer_array;  -- Array of 16-bit word to transmit
                                                -- tx_buffer_i(0) = command word
    tx_send_frame_p_i : in  std_logic;          -- Send frame
    tx_done_p_o       : out std_logic           -- Frame transmission finished

    );
end mil1553_tx_serialiser;


architecture rtl of mil1553_tx_serialiser is

  ----------------------------------------------------------------------------
  -- Constants declaration
  ----------------------------------------------------------------------------
  constant c_NB_BIT_IN_WORD : integer := 40;  -- Number of half bit in a word

  ----------------------------------------------------------------------------
  -- Types declaration
  ----------------------------------------------------------------------------
  type t_tx_fsm_states is (TX_IDLE, TX_COMMAND, TX_WORD, TX_DONE);

  ----------------------------------------------------------------------------
  -- Signals declaration
  ----------------------------------------------------------------------------
  signal tx_fsm_next_state : t_tx_fsm_states;
  signal tx_fsm_state      : t_tx_fsm_states;
  signal nb_word_to_send   : unsigned(5 downto 0);
  signal word_cnt          : unsigned(5 downto 0);
  signal bit_cnt           : unsigned(5 downto 0);
  signal tx_buffer_encoded : t_tx_buffer_encoded_array;
  signal tr_flag           : std_logic;
  signal transmitting      : std_logic;
  signal tx_send_frame_p   : std_logic;
  signal tx_send_frame_p_d : std_logic;
  signal tx_shift_reg      : std_logic_vector(39 downto 0);
  signal mil1553_txd       : std_logic;
  signal mil1553_tx_en     : std_logic;
  signal mil1553_tx_en_d   : std_logic;

begin

  ----------------------------------------------------------------------------
  -- Ignoring send requests when transmitting already
  ----------------------------------------------------------------------------
  tx_send_frame_p <= tx_send_frame_p_i and not(transmitting);

  ----------------------------------------------------------------------------
  -- Delays send frame signal to allow initialisation of the serialiser
  ----------------------------------------------------------------------------
  p_send_frame_delay : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if sys_rst_n_i = '0' then
        tx_send_frame_p_d <= '0';
      else
        tx_send_frame_p_d <= tx_send_frame_p;
      end if;
    end if;
  end process p_send_frame_delay;

  ----------------------------------------------------------------------------
  -- Adds odd parity, encode in Manchester and
  -- registers words to send
  ----------------------------------------------------------------------------
  p_data_to_send_reg : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if sys_rst_n_i = '0' then
        tx_buffer_encoded <= (others => (others => '0'));
        tr_flag           <= '0';
        nb_word_to_send   <= (others => '0');
      elsif tx_send_frame_p = '1' then
        -- Store number of words to transmit
        -- If word_count field = 0 => 32 words to transmit
        if tx_buffer_i(0)(c_CMD_WC4 downto c_CMD_WC0) = "00000" then
          nb_word_to_send <= "100000";  -- 32
        else
          nb_word_to_send <= unsigned('0' & tx_buffer_i(0)(c_CMD_WC4 downto c_CMD_WC0));
        end if;
        -- Store transmit/recieve flag
        tr_flag              <= tx_buffer_i(0)(c_CMD_TR);
        -- Encode command word
        tx_buffer_encoded(0) <= c_CMD_SYNC_FIELD & f_manch_encoder(tx_buffer_i(0));
        -- Encode data words
        l_data_encode : for I in 1 to c_TX_BUFFER_SIZE-1 loop
          tx_buffer_encoded(I) <= c_DATA_SYNC_FIELD & f_manch_encoder(tx_buffer_i(I));
        end loop;
      end if;
    end if;
  end process p_data_to_send_reg;

  ----------------------------------------------------------------------------
  -- Transmitter FSM
  ----------------------------------------------------------------------------
  p_tx_fsm_sync : process (sys_clk_i)
  begin
    if rising_edge (sys_clk_i) then
      if sys_rst_n_i = '0' then
        tx_fsm_state <= TX_IDLE;
      else
        tx_fsm_state <= tx_fsm_next_state;
      end if;
    end if;
  end process p_tx_fsm_sync;

  p_tx_fsm_transitions : process(tx_fsm_state, tx_send_frame_p_d, bit_cnt,
                                 tr_flag, word_cnt, tx_bit_rate_p_i,
                                 nb_word_to_send)
  begin
    case tx_fsm_state is

      when TX_IDLE =>
        if tx_send_frame_p_d = '1' then
          tx_fsm_next_state <= TX_COMMAND;
        else
          tx_fsm_next_state <= TX_IDLE;
        end if;

      when TX_COMMAND =>
        if bit_cnt = 0 and tx_bit_rate_p_i = '1' then
          if tr_flag = c_BC2RT then
            tx_fsm_next_state <= TX_WORD;
          else
            tx_fsm_next_state <= TX_DONE;
          end if;
        else
          tx_fsm_next_state <= TX_COMMAND;
        end if;

      when TX_WORD =>
        if bit_cnt = 0 and word_cnt = nb_word_to_send and tx_bit_rate_p_i = '1' then
          tx_fsm_next_state <= TX_DONE;
        else
          tx_fsm_next_state <= TX_WORD;
        end if;

      when TX_DONE =>
        tx_fsm_next_state <= TX_IDLE;

      when others =>
        tx_fsm_next_state <= TX_IDLE;

    end case;
  end process p_tx_fsm_transitions;

  p_tx_fsm_outputs : process(tx_fsm_state)
  begin
    case tx_fsm_state is

      when TX_IDLE =>
        transmitting <= '0';

      when TX_COMMAND =>
        transmitting <= '1';

      when TX_WORD =>
        transmitting <= '1';

      when TX_DONE =>
        transmitting <= '0';

      when others =>
        transmitting <= '0';

    end case;
  end process p_tx_fsm_outputs;

  ----------------------------------------------------------------------------
  -- Bit counter
  ----------------------------------------------------------------------------
  p_bit_cnt : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if sys_rst_n_i = '0' then
        bit_cnt <= (others => '0');
      elsif tx_send_frame_p_d = '1' then
        bit_cnt <= to_unsigned(c_NB_BIT_IN_WORD - 1, bit_cnt'length);
      elsif transmitting = '1' and tx_bit_rate_p_i = '1' then
        if bit_cnt = 0 then
          bit_cnt <= to_unsigned(c_NB_BIT_IN_WORD - 1, bit_cnt'length);
        else
          bit_cnt <= bit_cnt - 1;
        end if;
      end if;
    end if;
  end process p_bit_cnt;

  ----------------------------------------------------------------------------
  -- Word counter
  ----------------------------------------------------------------------------
  p_word_cnt : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if sys_rst_n_i = '0' then
        word_cnt <= (others => '0');
      elsif tx_send_frame_p_d = '1' then
        word_cnt <= (others => '0');
      elsif transmitting = '1' and tx_bit_rate_p_i = '1' and bit_cnt = 0 then
        if word_cnt = nb_word_to_send then
          word_cnt <= (others => '0');
        else
          word_cnt <= word_cnt + 1;
        end if;
      end if;
    end if;
  end process p_word_cnt;

  ----------------------------------------------------------------------------
  -- Output shift register
  ----------------------------------------------------------------------------
  p_shift_reg : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if sys_rst_n_i = '0' then
        tx_shift_reg  <= (others => '0');
        mil1553_txd   <= '0';
        mil1553_tx_en <= '0';
      elsif tx_send_frame_p_d = '1' then
        tx_shift_reg <= tx_buffer_encoded(0);  -- loads command word in shift register
      elsif transmitting = '1' and tx_bit_rate_p_i = '1' then
        if bit_cnt = 0 and word_cnt /= nb_word_to_send then
          tx_shift_reg <= tx_buffer_encoded(to_integer(word_cnt + 1));
        else
          tx_shift_reg <= tx_shift_reg(38 downto 0) & '0';
        end if;
        mil1553_txd   <= tx_shift_reg(39);
        mil1553_tx_en <= '1';
      elsif transmitting = '0' and tx_bit_rate_p_i = '1' then
        mil1553_txd   <= '0';
        mil1553_tx_en <= '0';
      end if;
    end if;
  end process p_shift_reg;

  ----------------------------------------------------------------------------
  -- TX done signal generation
  -- => falling edge of mil1553 tx enable
  ----------------------------------------------------------------------------
  p_tx_done : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if sys_rst_n_i = '0' then
        mil1553_tx_en_d <= '0';
      else
        mil1553_tx_en_d <= mil1553_tx_en;
      end if;
    end if;
  end process p_tx_done;
  tx_done_p_o <= not(mil1553_tx_en) and mil1553_tx_en_d;

  ----------------------------------------------------------------------------
  -- Output assignment
  ----------------------------------------------------------------------------
  mil1553_tx_en_o <= mil1553_tx_en;
  mil1553_txd_o   <= mil1553_txd and mil1553_tx_en;


end architecture rtl;
