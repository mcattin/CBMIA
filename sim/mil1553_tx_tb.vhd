-------------------------------------------------------------------------------
-- Title      : MIL1553 transmitter testbench
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mil1553_tx_tb.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-03-09
-- Last update: 2012-03-09
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: MIL1553 transmitter testbench
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
-- 2012-03-09  1.0      mcattin         Created
-------------------------------------------------------------------------------
-- TODO: - 
--       - 
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
library work;
use work.cbmia_pkg.all;


entity mil1553_tx_tb is

end mil1553_tx_tb;


architecture behavioral of mil1553_tx_tb is

  ----------------------------------------------------------------------------
  -- Components declaration
  ----------------------------------------------------------------------------
  component mil1553_tx
    port (
      sys_rst_n_i       : in  std_logic;          -- Synchronous system reset (active low)
      sys_clk_i         : in  std_logic;          -- System clock
      mil1553_txd_o     : out std_logic;          -- Serial data output
      mil1553_tx_o      : out std_logic;          -- Serial data output enable
      tx_buffer_i       : in  t_tx_buffer_array;  -- Array of 16-bit word to transmit
                                                  -- tx_buffer_i(0) = command word
      tx_send_frame_p_i : in  std_logic;          -- Send frame
      tx_done_p_o       : out std_logic           -- Frame transmission finished
      );
  end component mil1553_tx;

  ----------------------------------------------------------------------------
  -- Constants declaration
  ----------------------------------------------------------------------------
  constant c_CLK_PERIOD : time := 25 ns;

  ----------------------------------------------------------------------------
  -- Signals declaration
  ----------------------------------------------------------------------------
  signal sys_clk         : std_logic         := '0';
  signal sys_rst_n       : std_logic         := '0';
  signal tx_buffer       : t_tx_buffer_array := (others => (others => '0'));
  signal tx_send_frame_p : std_logic         := '0';

  signal mil1553_tx_en : std_logic;
  signal mil1553_txd   : std_logic;
  signal tx_done_p     : std_logic;

begin

  uut_mil1553_tx : mil1553_tx
    port map(
      sys_rst_n_i       => sys_rst_n,
      sys_clk_i         => sys_clk,
      mil1553_txd_o     => mil1553_txd,
      mil1553_tx_o      => mil1553_tx_en,
      tx_buffer_i       => tx_buffer,
      tx_send_frame_p_i => tx_send_frame_p,
      tx_done_p_o       => tx_done_p
      );

  p_clk : process
  begin
    sys_clk <= '0';
    wait for c_CLK_PERIOD/2;
    sys_clk <= '1';
    wait for c_CLK_PERIOD/2;
  end process p_clk;

  p_stimulus : process
  begin

    -- Reset
    sys_rst_n <= '0';
    wait for 100 ns;
    wait until rising_edge(sys_clk);
    sys_rst_n <= '1';
    wait for 500 ns;
    wait until rising_edge(sys_clk);

    -- Init command word
    tx_buffer(0)(c_CMD_SA4 downto c_CMD_SA0) <= "00000";
    tx_buffer(0)(c_CMD_WC4 downto c_CMD_WC0) <= "00000";
    tx_buffer(0)(c_CMD_RT4 downto c_CMD_RT0) <= "00011";
    tx_buffer(0)(c_CMD_TR)                   <= '0';

    -- Init tx buffer (data words)
    for I in 1 to c_TX_BUFFER_SIZE loop
      tx_buffer(I) <= std_logic_vector(to_unsigned(I, 16));
    end loop;

    -- start transmition
    wait for 500 ns;
    wait until rising_edge(sys_clk);
    tx_send_frame_p <= '1';
    wait until rising_edge(sys_clk);
    tx_send_frame_p <= '0';


    -- start transmition
    wait for 10 us;
    wait until rising_edge(sys_clk);
    tx_send_frame_p <= '1';
    wait until rising_edge(sys_clk);
    tx_send_frame_p <= '0';

    wait;
  end process p_stimulus;


end architecture behavioral;
