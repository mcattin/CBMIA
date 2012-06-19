-------------------------------------------------------------------------------
-- Title      : MIL1553 transmitter
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mil1553_tx.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-03-08
-- Last update: 2012-03-23
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: MIL1553 transmitter, only supports 1Mb/s bus speed.
--              Adds odd parity, encodes in Manchester and transmits
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


entity mil1553_tx is
  port (

    -- Global ports
    ----------------------------------------------------------------------------
    sys_rst_n_i : in std_logic;         -- Synchronous system reset (active low)
    sys_clk_i   : in std_logic;         -- System clock

    -- MIL1553 interface
    ----------------------------------------------------------------------------
    mil1553_txd_o   : out std_logic;    -- Serial data output
    mil1553_tx_en_o : out std_logic;    -- Serial data output enable

    -- User interface
    ----------------------------------------------------------------------------
    tx_buffer_i       : in  t_tx_buffer_array;  -- Array of 16-bit word to transmit
                                                -- tx_buffer_i(0) = command word
    tx_send_frame_p_i : in  std_logic;          -- Send frame
    tx_done_p_o       : out std_logic           -- Frame transmission finished

    );
end mil1553_tx;


architecture rtl of mil1553_tx is

  ----------------------------------------------------------------------------
  -- Signals declaration
  ----------------------------------------------------------------------------
  signal tx_bit_rate_p : std_logic;

begin

  ----------------------------------------------------------------------------
  -- Clock generator for 1Mb/s from 40MHz
  ----------------------------------------------------------------------------
  cmp_mil1553_tx_clk : mil1553_tx_clk
    port map(
      sys_rst_n_i     => sys_rst_n_i,
      sys_clk_i       => sys_clk_i,
      tx_bit_rate_p_o => tx_bit_rate_p
      );

  ----------------------------------------------------------------------------
  -- MIL1553 serialiser
  ----------------------------------------------------------------------------
  cmp_mil1553_serialiser : mil1553_tx_serialiser
    port map(
      sys_rst_n_i       => sys_rst_n_i,
      sys_clk_i         => sys_clk_i,
      mil1553_txd_o     => mil1553_txd_o,
      mil1553_tx_en_o   => mil1553_tx_en_o,
      tx_bit_rate_p_i   => tx_bit_rate_p,
      tx_buffer_i       => tx_buffer_i,
      tx_send_frame_p_i => tx_send_frame_p_i,
      tx_done_p_o       => tx_done_p_o
      );


end architecture rtl;
