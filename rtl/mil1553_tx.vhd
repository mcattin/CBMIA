-------------------------------------------------------------------------------
-- Title      : MIL1553 transmitter
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mil1553_tx.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-02-29
-- Last update: 2012-02-29
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

entity mil1553_tx is
  generic(

    );
  port (

    -- Global ports
    ----------------------------------------------------------------------------
    sys_rst_n_i : in std_logic;         -- Synchronous system reset (active low)
    sys_clk_i   : in std_logic;         -- System clock

    -- MIL1553 interface
    ----------------------------------------------------------------------------
    mil1553_txd_o : out std_logic;
    mil1553_tx_o  : out std_logic;

    -- User interface
    ----------------------------------------------------------------------------
    tx_word_i       : in  std_logic_vector(15 downto 0);  -- Word to transmit
                                                          -- 15..0 : word data
                                                          --    16 : word type flag, 0=data, 1=command
    tx_word_valid_i : in  std_logic;                      -- Word valid input
    tx_word_rd_o    : out std_logic;                      -- Word read request
    tx_word_empty_i : in  std_logic;                      -- Word FIFO empty
    tx_send_frame_i : in  std_logic;                      -- Send frame in FIFO
    tx_done_o       : out std_logic                       -- Frame transmission finished

    );
end mil1553_tx;

architecture rtl of mil1553_tx is



begin



end architecture rtl;
