-------------------------------------------------------------------------------
-- Title      : MIL1553 receiver
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mil1553_rx.vhd
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

entity mil1553_rx is
  generic(

    );
  port (

    -- Global ports
    ----------------------------------------------------------------------------
    sys_rst_n_i : in std_logic;         -- Synchronous system reset (active low)
    sys_clk_i   : in std_logic;         -- System clock

    -- MIL1553 interface
    ----------------------------------------------------------------------------
    mil1553_rxd_i   : in std_logic;     -- Serial data input
    mil1553_rx_en_i : in std_logic;     -- Receiver enable

    -- User interface
    ----------------------------------------------------------------------------
    rx_word_o           : out std_logic_vector(19 downto 0);  -- Received word
                                                              -- 15..0 : word data
                                                              --    16 : word type flag, 0=data, 1=status
                                                              --    17 : error flag, 0=no error, 1=word contains error
                                                              --    18 : parity error flag, 0=no error, 1=error
                                                              --    19 : Manchester code violation flag, 0=no error, 1=error
    rx_word_wr_o        : out std_logic;                      -- Received word write strobe
    rx_done_o           : out std_logic;                      -- End of frame reception
    rx_glitch_detect_o  : out std_logic;                      -- Glitch detected in serial data
    rx_word_error_o     : out std_logic                       -- Received word contains error (parity error, code violation)

    );
end mil1553_rx;

architecture rtl of mil1553_rx is



begin



end architecture rtl;
