-------------------------------------------------------------------------------
-- Title      : MIL1553 transmitter testbench
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : transmitter_tb.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-03-09
-- Last update: 2012-03-16
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
use work.mem_interface_pkg.all;

entity core_tb is

end core_tb;


architecture behavioral of core_tb is

  ----------------------------------------------------------------------------
  -- Components declaration
  ----------------------------------------------------------------------------
  component mil1553_core
    generic(
      g_HW_VERSION : std_logic_vector(15 downto 0) := X"0000"
      );
    port (
      pwr_reset_n_i     : in    std_logic;
      sys_clk_i         : in    std_logic;
      mil1553_rxd_a_i   : in    std_logic;
      mil1553_tx_rx_n_o : out   std_logic;
      mil1553_tx_n_o    : out   std_logic;
      mil1553_txd_o     : out   std_logic;
      mil1553_txd_n_o   : out   std_logic;
      led_o             : out   std_logic_vector(6 downto 0);
      test_point_o      : out   std_logic_vector (3 downto 0);
      onewire_b         : inout std_logic;
      rd_to_mem_i       : in    std_logic;
      wr_to_mem_i       : in    std_logic;
      data_from_mem_o   : out   t_int_data;
      addr_to_mem_i     : in    t_int_addr;
      data_to_mem_i     : in    t_int_data;
      op_done_o         : out   std_logic;
      irq_req_o         : out   std_logic_vector(1 downto 0)
      );
  end component mil1553_core;

  ----------------------------------------------------------------------------
  -- Constants declaration
  ----------------------------------------------------------------------------
  constant c_CLK_PERIOD : time := 25 ns;

  ----------------------------------------------------------------------------
  -- Signals declaration
  ----------------------------------------------------------------------------
  signal sys_clk         : std_logic         := '0';
  signal sys_rst_n       : std_logic         := '0';

  signal mil1553_rxd_a : std_logic := '0';
  signal rd_to_mem : std_logic := '0';
  signal wr_to_mem : std_logic := '0';
  signal addr_to_mem : t_int_addr := (others => '0');
  signal data_to_mem : t_int_data := (others => '0');

  signal mil1553_tx_rx_n : std_logic;
  signal mil1553_tx_n : std_logic;
  signal mil1553_txd : std_logic;
  signal mil1553_txd_n : std_logic;
  signal led : std_logic_vector(6 downto 0);
  signal test_point : std_logic_vector(3 downto 0);
  signal onewire : std_logic;
  signal data_from_mem : t_int_data;
  signal op_done : std_logic;
  signal irq_req : std_logic_vector(1 downto 0);

begin

  uut_mil1553_core : mil1553_core
    generic map(
      g_HW_VERSION => X"0201"
      )
    port map(
      pwr_reset_n_i     => sys_rst_n,
      sys_clk_i         => sys_clk,
      mil1553_rxd_a_i   => mil1553_rxd_a,
      mil1553_tx_rx_n_o => mil1553_tx_rx_n,
      mil1553_tx_n_o    => mil1553_tx_n,
      mil1553_txd_o     => mil1553_txd,
      mil1553_txd_n_o   => mil1553_txd_n,
      led_o             => led,
      test_point_o      => test_point,
      onewire_b         => onewire,
      rd_to_mem_i       => rd_to_mem,
      wr_to_mem_i       => wr_to_mem,
      data_from_mem_o   => data_from_mem,
      addr_to_mem_i     => addr_to_mem,
      data_to_mem_i     => data_to_mem,
      op_done_o         => op_done,
      irq_req_o         => irq_req
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
    wait for 50 us;
    wait until rising_edge(sys_clk);

    -- insert stimulus here
    wait for c_CLK_PERIOD*100;
    wait until rising_edge(sys_clk);
    wr_to_mem <= '1';
    data_to_mem <= X"00000001";
    addr_to_mem <= std_logic_vector(to_unsigned(1, addr_to_mem'length));
    wait until rising_edge(sys_clk);
    wr_to_mem <= '0';

    wait for c_CLK_PERIOD*100;
    wait until rising_edge(sys_clk);
    wr_to_mem <= '1';
    data_to_mem(31 downto 16) <= (others => '0');
    data_to_mem(c_CMD_SA4 downto c_CMD_SA0) <= "11110";
    data_to_mem(c_CMD_WC4 downto c_CMD_WC0) <= "00001";
    data_to_mem(c_CMD_RT4 downto c_CMD_RT0) <= "11110";
    data_to_mem(c_CMD_TR)                   <= '1';
    addr_to_mem <= std_logic_vector(to_unsigned(8, addr_to_mem'length));
    wait until rising_edge(sys_clk);
    wr_to_mem <= '0';

    wait until falling_edge(mil1553_tx_rx_n);
    wait for 50 us;

    wait for c_CLK_PERIOD*100;
    wait until rising_edge(sys_clk);
    wr_to_mem <= '1';
    data_to_mem <= X"0001" & X"0002";
    addr_to_mem <= std_logic_vector(to_unsigned(27, addr_to_mem'length));
    wait until rising_edge(sys_clk);
    wr_to_mem <= '0';

    wait for c_CLK_PERIOD*100;
    wait until rising_edge(sys_clk);
    wr_to_mem <= '1';
    data_to_mem <= X"0003" & X"0004";
    addr_to_mem <= std_logic_vector(to_unsigned(28, addr_to_mem'length));
    wait until rising_edge(sys_clk);
    wr_to_mem <= '0';

    wait for c_CLK_PERIOD*100;
    wait until rising_edge(sys_clk);
    wr_to_mem <= '1';
    data_to_mem <= X"0005" & X"0006";
    addr_to_mem <= std_logic_vector(to_unsigned(29, addr_to_mem'length));
    wait until rising_edge(sys_clk);
    wr_to_mem <= '0';

    wait for c_CLK_PERIOD*100;
    wait until rising_edge(sys_clk);
    wr_to_mem <= '1';
    data_to_mem <= X"0000A846";
    addr_to_mem <= std_logic_vector(to_unsigned(8, addr_to_mem'length));
    wait until rising_edge(sys_clk);
    wr_to_mem <= '0';

    wait;
  end process p_stimulus;


end architecture behavioral;
