-------------------------------------------------------------------------------
-- Title      : Memory interface package
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mem_interface_pkg.vhd
-- Author     : Pablo Antonio Alvarez Sanchez
-- Company    : CERN (BE-CO-HT)
-- Created    : 2004-09-27
-- Last update: 2012-03-16
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: This package contains types and constants declaration for
--              the memory interface -> address decoder
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
-- 2004-09-27  1.0      paas            Created
-- 2007-08-16  1.1      nouchi          Modified for CBMIA
-- 2012-03-09  1.2      mcattin         Clean-up, add license
-------------------------------------------------------------------------------
-- TODO: -
--       -
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;


package mem_interface_pkg is


  constant c_ADDR_TOP   : integer := 16#FFFF#;
  constant c_ADDR_WIDTH : integer := 14;
  constant c_DATA_WIDTH : integer := 32;

  subtype t_mem_addr is integer range 0 to c_ADDR_TOP;
  type t_rw is (
    rw,                                 --Read Write
    ro,                                 --Read Only
    wo,                                 --Write Only
    cr                                  --Clear on Read
    );

  type t_addr_rw is
  record
    AddL     : t_mem_addr;
    AddH     : t_mem_addr;
    Delay    : integer;
    Rw       : t_rw;
    PosToSel : integer;
  end record;

  constant c_NB_MEM_POS : integer := 45;
  subtype  t_mem_pos is integer range 0 to c_NB_MEM_POS - 1;

  constant c_IRQ_SRC_POS        : t_mem_pos := 0;
  constant c_IRQ_EN_POS         : t_mem_pos := 1;
  constant c_DBG0_POS           : t_mem_pos := 2;
  constant c_STAT_POS           : t_mem_pos := 3;
  constant c_CMD_POS            : t_mem_pos := 4;
  constant c_DBG1_POS           : t_mem_pos := 5;
  constant c_ID_MSB_POS         : t_mem_pos := 6;
  constant c_ID_LSB_POS         : t_mem_pos := 7;
  constant c_TX_REG_POS         : t_mem_pos := 8;
  constant c_RX_REG_POS         : t_mem_pos := 9;
  constant c_RX_BUF0_POS        : t_mem_pos := 10;
  constant c_RX_BUF1_POS        : t_mem_pos := 11;
  constant c_RX_BUF2_POS        : t_mem_pos := 12;
  constant c_RX_BUF3_POS        : t_mem_pos := 13;
  constant c_RX_BUF4_POS        : t_mem_pos := 14;
  constant c_RX_BUF5_POS        : t_mem_pos := 15;
  constant c_RX_BUF6_POS        : t_mem_pos := 16;
  constant c_RX_BUF7_POS        : t_mem_pos := 17;
  constant c_RX_BUF8_POS        : t_mem_pos := 18;
  constant c_RX_BUF9_POS        : t_mem_pos := 19;
  constant c_RX_BUF10_POS       : t_mem_pos := 20;
  constant c_RX_BUF11_POS       : t_mem_pos := 21;
  constant c_RX_BUF12_POS       : t_mem_pos := 22;
  constant c_RX_BUF13_POS       : t_mem_pos := 23;
  constant c_RX_BUF14_POS       : t_mem_pos := 24;
  constant c_RX_BUF15_POS       : t_mem_pos := 25;
  constant c_RX_BUF16_POS       : t_mem_pos := 26;
  constant c_TX_BUF0_POS        : t_mem_pos := 27;
  constant c_TX_BUF1_POS        : t_mem_pos := 28;
  constant c_TX_BUF2_POS        : t_mem_pos := 29;
  constant c_TX_BUF3_POS        : t_mem_pos := 30;
  constant c_TX_BUF4_POS        : t_mem_pos := 31;
  constant c_TX_BUF5_POS        : t_mem_pos := 32;
  constant c_TX_BUF6_POS        : t_mem_pos := 33;
  constant c_TX_BUF7_POS        : t_mem_pos := 34;
  constant c_TX_BUF8_POS        : t_mem_pos := 35;
  constant c_TX_BUF9_POS        : t_mem_pos := 36;
  constant c_TX_BUF10_POS       : t_mem_pos := 37;
  constant c_TX_BUF11_POS       : t_mem_pos := 38;
  constant c_TX_BUF12_POS       : t_mem_pos := 39;
  constant c_TX_BUF13_POS       : t_mem_pos := 40;
  constant c_TX_BUF14_POS       : t_mem_pos := 41;
  constant c_TX_BUF15_POS       : t_mem_pos := 42;
  constant c_FAULT_ADDR_LOW_POS : t_mem_pos := 43;
  constant c_FAULT_ADDR_POS     : t_mem_pos := 44;

  type     t_addr_mapping is array (0 to c_NB_MEM_POS - 1) of t_addr_rw;
  constant ADDTABLE : t_addr_mapping :=
    (
      c_IRQ_SRC_POS        => (AddL => 0, AddH => 0, Delay => 0, rw => cr, PosToSel => c_IRQ_SRC_POS),
      c_IRQ_EN_POS         => (AddL => 1, AddH => 1, Delay => 0, rw => rw, PosToSel => c_IRQ_EN_POS),
      c_DBG0_POS           => (AddL => 2, AddH => 2, Delay => 0, rw => rw, PosToSel => c_DBG0_POS),
      c_STAT_POS           => (AddL => 3, AddH => 3, Delay => 0, rw => rw, PosToSel => c_STAT_POS),
      c_CMD_POS            => (AddL => 4, AddH => 4, Delay => 0, rw => rw, PosToSel => c_CMD_POS),
      c_DBG1_POS           => (AddL => 5, AddH => 5, Delay => 0, rw => rw, PosToSel => c_DBG1_POS),
      c_ID_MSB_POS         => (AddL => 6, AddH => 6, Delay => 0, rw => ro, PosToSel => c_ID_MSB_POS),
      c_ID_LSB_POS         => (AddL => 7, AddH => 7, Delay => 0, rw => ro, PosToSel => c_ID_LSB_POS),
      c_TX_REG_POS         => (AddL => 8, AddH => 8, Delay => 0, rw => ro, PosToSel => c_TX_REG_POS),
      c_RX_REG_POS         => (AddL => 9, AddH => 9, Delay => 0, rw => ro, PosToSel => c_RX_REG_POS),
      c_RX_BUF0_POS        => (AddL => 10, AddH => 10, Delay => 0, rw => ro, PosToSel => c_RX_BUF0_POS),
      c_RX_BUF1_POS        => (AddL => 11, AddH => 11, Delay => 0, rw => ro, PosToSel => c_RX_BUF1_POS),
      c_RX_BUF2_POS        => (AddL => 12, AddH => 12, Delay => 0, rw => ro, PosToSel => c_RX_BUF2_POS),
      c_RX_BUF3_POS        => (AddL => 13, AddH => 13, Delay => 0, rw => ro, PosToSel => c_RX_BUF3_POS),
      c_RX_BUF4_POS        => (AddL => 14, AddH => 14, Delay => 0, rw => ro, PosToSel => c_RX_BUF4_POS),
      c_RX_BUF5_POS        => (AddL => 15, AddH => 15, Delay => 0, rw => ro, PosToSel => c_RX_BUF5_POS),
      c_RX_BUF6_POS        => (AddL => 16, AddH => 16, Delay => 0, rw => ro, PosToSel => c_RX_BUF6_POS),
      c_RX_BUF7_POS        => (AddL => 17, AddH => 17, Delay => 0, rw => ro, PosToSel => c_RX_BUF7_POS),
      c_RX_BUF8_POS        => (AddL => 18, AddH => 18, Delay => 0, rw => ro, PosToSel => c_RX_BUF8_POS),
      c_RX_BUF9_POS        => (AddL => 19, AddH => 19, Delay => 0, rw => ro, PosToSel => c_RX_BUF9_POS),
      c_RX_BUF10_POS       => (AddL => 20, AddH => 20, Delay => 0, rw => ro, PosToSel => c_RX_BUF10_POS),
      c_RX_BUF11_POS       => (AddL => 21, AddH => 21, Delay => 0, rw => ro, PosToSel => c_RX_BUF11_POS),
      c_RX_BUF12_POS       => (AddL => 22, AddH => 22, Delay => 0, rw => ro, PosToSel => c_RX_BUF12_POS),
      c_RX_BUF13_POS       => (AddL => 23, AddH => 23, Delay => 0, rw => ro, PosToSel => c_RX_BUF13_POS),
      c_RX_BUF14_POS       => (AddL => 24, AddH => 24, Delay => 0, rw => ro, PosToSel => c_RX_BUF14_POS),
      c_RX_BUF15_POS       => (AddL => 25, AddH => 25, Delay => 0, rw => ro, PosToSel => c_RX_BUF15_POS),
      c_RX_BUF16_POS       => (AddL => 26, AddH => 26, Delay => 0, rw => ro, PosToSel => c_RX_BUF16_POS),
      c_TX_BUF0_POS        => (AddL => 27, AddH => 27, Delay => 0, rw => ro, PosToSel => c_TX_BUF0_POS),
      c_TX_BUF1_POS        => (AddL => 28, AddH => 28, Delay => 0, rw => ro, PosToSel => c_TX_BUF1_POS),
      c_TX_BUF2_POS        => (AddL => 29, AddH => 29, Delay => 0, rw => ro, PosToSel => c_TX_BUF2_POS),
      c_TX_BUF3_POS        => (AddL => 30, AddH => 30, Delay => 0, rw => ro, PosToSel => c_TX_BUF3_POS),
      c_TX_BUF4_POS        => (AddL => 31, AddH => 31, Delay => 0, rw => ro, PosToSel => c_TX_BUF4_POS),
      c_TX_BUF5_POS        => (AddL => 32, AddH => 32, Delay => 0, rw => ro, PosToSel => c_TX_BUF5_POS),
      c_TX_BUF6_POS        => (AddL => 33, AddH => 33, Delay => 0, rw => ro, PosToSel => c_TX_BUF6_POS),
      c_TX_BUF7_POS        => (AddL => 34, AddH => 34, Delay => 0, rw => ro, PosToSel => c_TX_BUF7_POS),
      c_TX_BUF8_POS        => (AddL => 35, AddH => 35, Delay => 0, rw => ro, PosToSel => c_TX_BUF8_POS),
      c_TX_BUF9_POS        => (AddL => 36, AddH => 36, Delay => 0, rw => ro, PosToSel => c_TX_BUF9_POS),
      c_TX_BUF10_POS       => (AddL => 37, AddH => 37, Delay => 0, rw => ro, PosToSel => c_TX_BUF10_POS),
      c_TX_BUF11_POS       => (AddL => 38, AddH => 38, Delay => 0, rw => ro, PosToSel => c_TX_BUF11_POS),
      c_TX_BUF12_POS       => (AddL => 39, AddH => 39, Delay => 0, rw => ro, PosToSel => c_TX_BUF12_POS),
      c_TX_BUF13_POS       => (AddL => 40, AddH => 40, Delay => 0, rw => ro, PosToSel => c_TX_BUF13_POS),
      c_TX_BUF14_POS       => (AddL => 41, AddH => 41, Delay => 0, rw => ro, PosToSel => c_TX_BUF14_POS),
      c_TX_BUF15_POS       => (AddL => 42, AddH => 42, Delay => 0, rw => ro, PosToSel => c_TX_BUF15_POS),
      c_FAULT_ADDR_LOW_POS => (AddL => 44, AddH => 16#0b8F#, Delay => 0, rw => ro, PosToSel => c_FAULT_ADDR_LOW_POS),
      c_FAULT_ADDR_POS     => (AddL => 16#0BC0#, AddH => 16#FFFF#, Delay => 0, rw => ro, PosToSel => c_FAULT_ADDR_POS)
      );


  constant c_CMD_RST_BIT : integer := 0;


  subtype t_int_data is std_logic_vector(c_DATA_WIDTH - 1 downto 0);
  subtype t_int_addr is std_logic_vector(c_ADDR_WIDTH - 1 downto 0);
  type    t_mux_data_array is array (0 to c_NB_MEM_POS -1) of t_int_data;
  subtype t_selected_pos is std_logic_vector(c_NB_MEM_POS - 1 downto 0);

  type t_cont_to_mem is
  record
    Data        : t_int_data;           -- std_logic_vector(31 downto 0);
    Add         : t_int_addr;           -- std_logic_vector(addtop downto 0);
    AddOffSet   : t_int_addr;
    SelectedPos : t_selected_pos;       -- register to be accessed
    WrEn        : t_selected_pos;       -- register to be written std_logic_vector(c_NB_MEM_POS - 1 downto 0);
    RdEn        : t_selected_pos;
    Wr          : std_logic;
    Rd          : std_logic;
  end record;

  type t_mem_to_cont_cell is
  record
    Data   : t_int_data;
    RdDone : std_logic;
  end record;

  type t_mem_to_cont is array (integer range <>) of t_mem_to_cont_cell;


end mem_interface_pkg;


package body mem_interface_pkg is



end mem_interface_pkg;
