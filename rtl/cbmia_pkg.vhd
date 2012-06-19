-------------------------------------------------------------------------------
-- Title      : CBMIA package
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : cbmia_pkg.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-02-29
-- Last update: 2012-03-02
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: Package for CBMIA hdl design.
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


package cbmia_pkg is

  -----------------------------------------------------------------------------
  -- Constants declaration
  -----------------------------------------------------------------------------
  c_SYS_CLK_PERIOD         : integer  := 25;  -- ns
  c_PERIODS_CNT_WIDTH      : integer  := 6;   -- Number of bits needed for the
                                              -- Manchester period counter
                                              -- -> at 1Mbit/s
  c_BIT_RATE_SYS_CLK_TICKS : unsigned :=
    to_unsigned((1000/c_SYS_CLK_PERIOD), c_PERIODS_CNT_WIDTH);

  c_DEGLITCH_THRESHOLD : integer := 4;  -- serial input glitch filter threshold
                                        -- pulses < c_DEGLITCH_THRESHOLD * sys_clk ticks
                                        -- are filtered out by the glitch filter

  -----------------------------------------------------------------------------
  -- Functions declaration
  -----------------------------------------------------------------------------
  function log2_ceil(N : natural) return positive;

  -----------------------------------------------------------------------------
  -- Componants declaration
  -----------------------------------------------------------------------------
  component mil1553_rx_deglitcher
    port (
      sys_rst_n_i         : in  std_logic;  -- Synchronous system reset (active low)
      sys_clk_i           : in  std_logic;  -- System clock
      rxd_a_i             : in  std_logic;  -- Serial data input
      rxd_filt_o          : out std_logic;  -- filtered output signal
      rxd_filt_edge_p_o   : out std_logic;  -- indicates an edge on the filtered signal
      rxd_filt_f_edge_p_o : out std_logic   -- indicates a falling edge on the filtered signal
      );
  end component mil1553_rx_deglitcher;

end cbmia_pkg;


package body cbmia_pkg is

  -----------------------------------------------------------------------------
  -- Returns log of 2 of a natural number
  -----------------------------------------------------------------------------
  function log2_ceil(N : natural) return positive is
  begin
    if N <= 2 then
      return 1;
    elsif N mod 2 = 0 then
      return 1 + log2_ceil(N/2);
    else
      return 1 + log2_ceil((N+1)/2);
    end if;
  end;

end cbmia_pkg;
