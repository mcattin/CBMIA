-------------------------------------------------------------------------------
-- Title      : Memory interface package
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mem_interface_pkg.vhd
-- Author     : Pablo Antonio Alvarez Sanchez
-- Company    : CERN (BE-CO-HT)
-- Created    : 2004-09-27
-- Last update: 2012-03-12
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


  constant DPRAM_COUNT_HIS : integer := 1;
  constant ADDTOP          : integer := 16#FFFF#;  -- All address are in Long word
  constant ADDLENGTH       : integer := 14;  --18; -- 23 - 2 + 1       ;
  constant DATALENGTH      : integer := 32;

  subtype RamAType is integer range 0 to ADDTOP;
  type RwType is (
    rw,                                 --Read Write
    ro,                                 --Read Only
    wo,                                 --Write Only
    cr                                  --Clear on Read
    );

  type AddRwRecord is
  record
    AddL     : RamAType;
    AddH     : RamAType;
    Delay    : integer;
    Rw       : RwType;
    PosToSel : integer;
  end record;

  constant NUMMEMPOSITION : integer := 45;
  subtype  MEMPOSITION is integer range 0 to NUMMEMPOSITION - 1;

  constant InterruptSourceP : MEMPOSITION := 0;
  constant InterruptEnableP : MEMPOSITION := 1;
  constant RTIPRESENTP      : MEMPOSITION := 2;
  constant SOURCEREGP       : MEMPOSITION := 3;
  constant CommandP         : MEMPOSITION := 4;
  constant CIRCADDP         : MEMPOSITION := 5;

  constant MIDP        : MEMPOSITION := 6;
  constant LIDP        : MEMPOSITION := 7;
  constant TXREGP      : MEMPOSITION := 8;
  constant RXREGP      : MEMPOSITION := 9;
  constant W1          : MEMPOSITION := 10;
  constant W2          : MEMPOSITION := 11;
  constant W3          : MEMPOSITION := 12;
  constant W4          : MEMPOSITION := 13;
  constant W5          : MEMPOSITION := 14;
  constant W6          : MEMPOSITION := 15;
  constant W7          : MEMPOSITION := 16;
  constant W8          : MEMPOSITION := 17;
  constant W9          : MEMPOSITION := 18;
  constant W10         : MEMPOSITION := 19;
  constant W11         : MEMPOSITION := 20;
  constant W12         : MEMPOSITION := 21;
  constant W13         : MEMPOSITION := 22;
  constant W14         : MEMPOSITION := 23;
  constant W15         : MEMPOSITION := 24;
  constant W16         : MEMPOSITION := 25;
  constant W17         : MEMPOSITION := 26;
  constant TXW1        : MEMPOSITION := 27;
  constant TXW2        : MEMPOSITION := 28;
  constant TXW3        : MEMPOSITION := 29;
  constant TXW4        : MEMPOSITION := 30;
  constant TXW5        : MEMPOSITION := 31;
  constant TXW6        : MEMPOSITION := 32;
  constant TXW7        : MEMPOSITION := 33;
  constant TXW8        : MEMPOSITION := 34;
  constant TXW9        : MEMPOSITION := 35;
  constant TXW10       : MEMPOSITION := 36;
  constant TXW11       : MEMPOSITION := 37;
  constant TXW12       : MEMPOSITION := 38;
  constant TXW13       : MEMPOSITION := 39;
  constant TXW14       : MEMPOSITION := 40;
  constant TXW15       : MEMPOSITION := 41;
  constant TXW16       : MEMPOSITION := 42;
  constant FAULTADDLOW : MEMPOSITION := 43;
  constant FAULTADD    : MEMPOSITION := 44;

  constant BLOCKSIZE : integer := 63;

  type     ADDMAPPINGType is array (0 to NUMMEMPOSITION - 1) of AddRwRecord;
  constant ADDTABLE : ADDMAPPINGType :=
    (
      InterruptSourceP => (AddL => 0, AddH => 0, Delay => 0, rw => cr, PosToSel => 0),
      InterruptEnableP => (AddL => 1, AddH => 1, Delay => 0, rw => rw, PosToSel => 1),
      RTIPRESENTP      => (AddL => 2, AddH => 2, Delay => 0, rw => rw, PosToSel => RTIPRESENTP),
      SOURCEREGP       => (AddL => 3, AddH => 3, Delay => 0, rw => rw, PosToSel => SOURCEREGP),
      CommandP         => (AddL => 4, AddH => 4, Delay => 0, rw => rw, PosToSel => CommandP),
      CIRCADDP         => (AddL => 5, AddH => 5, Delay => 0, rw => rw, PosToSel => CIRCADDP),
      MIDP             => (AddL => 6, AddH => 6, Delay => 0, rw => ro, PosToSel => MIDP),
      LIDP             => (AddL => 7, AddH => 7, Delay => 0, rw => ro, PosToSel => LIDP),
      TXREGP           => (AddL => 8, AddH => 8, Delay => 0, rw => ro, PosToSel => TXREGP),
      RXREGP           => (AddL => 9, AddH => 9, Delay => 0, rw => ro, PosToSel => RXREGP),
      W1               => (AddL => 10, AddH => 10, Delay => 0, rw => ro, PosToSel => W1),
      W2               => (AddL => 11, AddH => 11, Delay => 0, rw => ro, PosToSel => W2),
      W3               => (AddL => 12, AddH => 12, Delay => 0, rw => ro, PosToSel => W3),
      W4               => (AddL => 13, AddH => 13, Delay => 0, rw => ro, PosToSel => W4),
      W5               => (AddL => 14, AddH => 14, Delay => 0, rw => ro, PosToSel => W5),
      W6               => (AddL => 15, AddH => 15, Delay => 0, rw => ro, PosToSel => W6),
      W7               => (AddL => 16, AddH => 16, Delay => 0, rw => ro, PosToSel => W7),
      W8               => (AddL => 17, AddH => 17, Delay => 0, rw => ro, PosToSel => W8),
      W9               => (AddL => 18, AddH => 18, Delay => 0, rw => ro, PosToSel => W9),
      W10              => (AddL => 19, AddH => 19, Delay => 0, rw => ro, PosToSel => W10),
      W11              => (AddL => 20, AddH => 20, Delay => 0, rw => ro, PosToSel => W11),
      W12              => (AddL => 21, AddH => 21, Delay => 0, rw => ro, PosToSel => W12),
      W13              => (AddL => 22, AddH => 22, Delay => 0, rw => ro, PosToSel => W13),
      W14              => (AddL => 23, AddH => 23, Delay => 0, rw => ro, PosToSel => W14),
      W15              => (AddL => 24, AddH => 24, Delay => 0, rw => ro, PosToSel => W15),
      W16              => (AddL => 25, AddH => 25, Delay => 0, rw => ro, PosToSel => W16),
      W17              => (AddL => 26, AddH => 26, Delay => 0, rw => ro, PosToSel => W17),
      TXW1             => (AddL => 27, AddH => 27, Delay => 0, rw => ro, PosToSel => TXW1),
      TXW2             => (AddL => 28, AddH => 28, Delay => 0, rw => ro, PosToSel => TXW2),
      TXW3             => (AddL => 29, AddH => 29, Delay => 0, rw => ro, PosToSel => TXW3),
      TXW4             => (AddL => 30, AddH => 30, Delay => 0, rw => ro, PosToSel => TXW4),
      TXW5             => (AddL => 31, AddH => 31, Delay => 0, rw => ro, PosToSel => TXW5),
      TXW6             => (AddL => 32, AddH => 32, Delay => 0, rw => ro, PosToSel => TXW6),
      TXW7             => (AddL => 33, AddH => 33, Delay => 0, rw => ro, PosToSel => TXW7),
      TXW8             => (AddL => 34, AddH => 34, Delay => 0, rw => ro, PosToSel => TXW8),
      TXW9             => (AddL => 35, AddH => 35, Delay => 0, rw => ro, PosToSel => TXW9),
      TXW10            => (AddL => 36, AddH => 36, Delay => 0, rw => ro, PosToSel => TXW10),
      TXW11            => (AddL => 37, AddH => 37, Delay => 0, rw => ro, PosToSel => TXW11),
      TXW12            => (AddL => 38, AddH => 38, Delay => 0, rw => ro, PosToSel => TXW12),
      TXW13            => (AddL => 39, AddH => 39, Delay => 0, rw => ro, PosToSel => TXW13),
      TXW14            => (AddL => 40, AddH => 40, Delay => 0, rw => ro, PosToSel => TXW14),
      TXW15            => (AddL => 41, AddH => 41, Delay => 0, rw => ro, PosToSel => TXW15),
      TXW16            => (AddL => 42, AddH => 42, Delay => 0, rw => ro, PosToSel => TXW16),
      FAULTADDLOW      => (AddL => 44, AddH => 16#0b8F#, Delay => 0, rw => ro, PosToSel => FAULTADDLOW),
      FAULTADD         => (AddL => 16#0BC0#, AddH => 16#FFFF#, Delay => 0, rw => ro, PosToSel => FAULTADD)
      );

  constant CommandRESETP    : integer := 0;
  constant CommandENABLEP   : integer := 1;   -- Polling is enable if = 1
  constant CommandDISABLEP  : integer := 2;   -- Polling is disable if = 1
  constant CommandBUSSPEEDP : integer := 31;  -- bit 31 and 30 of the register to set the M1553 Bus Speed

  constant RESET_ACTIVE : std_logic := '0';

  type     KINDOFMUXType is (USEMUXZ, USEMUXCOMB);
  constant KINDOFMUX : KINDOFMUXType := USEMUXCOMB;

  subtype IntDataType is std_logic_vector(DATALENGTH - 1 downto 0);
  subtype IntAddrOutType is std_logic_vector(ADDLENGTH - 1 downto 0);
  type    MuxDataArrType is array (0 to NUMMEMPOSITION -1) of IntDataType;
  subtype MuxSelType is std_logic_vector(NUMMEMPOSITION -1 downto 0);
  subtype SelectedPosType is std_logic_vector(NUMMEMPOSITION - 1 downto 0);
  type    SelRamDataType is array (0 to NUMMEMPOSITION - 1) of IntDataType;

  type ContToMemType is
  record
    Data        : IntDataType;          -- std_logic_vector(31 downto 0);
    Add         : IntAddrOutType;       -- std_logic_vector(addtop downto 0);
    AddOffSet   : IntAddrOutType;
    SelectedPos : SelectedPosType;      -- register to be accessed
    WrEn        : SelectedPosType;      -- register to be written std_logic_vector(NUMMEMPOSITION - 1 downto 0);
    RdEn        : SelectedPosType;
    Wr          : std_logic;
    Rd          : std_logic;
  end record;

  type MemToContCellType is
  record
    Data   : IntDataType;
    RdDone : std_logic;
  end record;

  type MemToContType is array (integer range <>) of MemToContCellType;
  --function ExtendStdLogicVector(signal S   : std_logic_vector;
  --                              constant n : integer) return std_logic_vector ;

end mem_interface_pkg;

package body mem_interface_pkg is

  --function ExtendStdLogicVector(signal S : std_logic_vector; constant n : integer) return std_logic_vector is
  --  variable SL : std_logic_vector(n - 1 downto 0);
  --begin
  --  for I in SL'range loop
  --    if I > S'left then
  --      SL(I) := '0';
  --    else
  --      SL(I) := S(I);
  --    end if;
  --  end loop;
  --  return SL;
  --end ExtendStdLogicVector;

end mem_interface_pkg;
