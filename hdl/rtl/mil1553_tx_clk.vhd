-------------------------------------------------------------------------------
-- Title      : MIL1553 transmitter clock generator
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mil1553_tx_clk.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-03-08
-- Last update: 2012-03-23
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: MIL1553 transmitter clock generator, only supports 1Mb/s bus speed.
--              Outputs a pulse train at 2 MHz (half bit period for Manchester encoder)
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


entity mil1553_tx_clk is
  port (

    -- Global ports
    ----------------------------------------------------------------------------
    sys_rst_n_i : in std_logic;         -- Synchronous system reset (active low)
    sys_clk_i   : in std_logic;         -- System clock

    -- Pulse train output
    ----------------------------------------------------------------------------
    tx_bit_rate_p_o : out std_logic     -- Bit rate for serialiser

    );
end mil1553_tx_clk;


architecture rtl of mil1553_tx_clk is

  ----------------------------------------------------------------------------
  -- Signals declaration
  ----------------------------------------------------------------------------
  signal clk_div_cnt : unsigned(4 downto 0);

begin

  ----------------------------------------------------------------------------
  -- Generates 1Mb/s half bit rate => 2MHz
  -- tx_bit_rate_o = sys_clk_i/20 = 40MHz/20 = 2MHz
  ----------------------------------------------------------------------------
  p_clk_div : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if sys_rst_n_i = '0' then
        clk_div_cnt     <= (others => '0');
        tx_bit_rate_p_o <= '0';
      elsif clk_div_cnt = 0 then
        clk_div_cnt     <= to_unsigned(19, clk_div_cnt'length);
        tx_bit_rate_p_o <= '1';
      else
        clk_div_cnt     <= clk_div_cnt - 1;
        tx_bit_rate_p_o <= '0';
      end if;
    end if;
  end process p_clk_div;


end architecture rtl;
