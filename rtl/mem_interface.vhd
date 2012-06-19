-------------------------------------------------------------------------------
-- Title      : Memory interface
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : bus_interface.vhd
-- Author     : Pablo Antonio Alvarez Sanchez
-- Company    : CERN (BE-CO-HT)
-- Created    : 2002-09-30
-- Last update: 2012-03-12
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: The purpose of this entity is to make a link between
--              the host bus and the registers and ram of the FPGA.
--
--              Basically it consists of an address decoder and
--              a data multiplexor implemented in a case when structure.
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
-- 2002-09-30  1.0      paas            Created
-- 2004-02-27  2.0      paas            Modified to generate a 1 tick RdEn, WrEn
-- 2012-03-09  2.1      mcattin         Clean-up, add license
-------------------------------------------------------------------------------
-- TODO: -
--       -
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;
library work;
use work.mem_interface_pkg.all;


entity mem_interface is
  port (
    Clk  : in std_logic;
    RstN : in std_logic;

    -- Interface
    IntRead     : in  std_logic;        -- Interface Read Signal
    IntWrite    : in  std_logic;        -- Interface Write Signal
    DataFromInt : in  IntDataType;      -- Data From interface
    IntAdd      : in  IntAddrOutType;   -- Address From interface
    OpDone      : out std_logic;        -- Operation Done, Read or Write Finished
    DataToInt   : out IntDataType;      -- Data going from Control to the Interface

    -- Memory
    ContToMem : out ContToMemType;                          -- Data going from Control to the Registers
                                                            -- This consists of Data + Enable + Read + Write
    MemToCont : in  MemToContType(0 to NUMMEMPOSITION - 1)  -- Data Array  From the Registers to the Control
                                                            -- Data + Done
    );
end mem_interface;



architecture rtl of mem_interface is

  component mem_interface_mux
    generic(
      INDH : integer := 10;
      INDL : integer := 0
      );
    port(
      IntAdd    : in  IntAddrOutType;
      DoingOpC  : in  std_logic;
      DoneRam   : in  SelectedPosType;
      DataArray : in  MuxDataArrType;
      Sel       : out SelectedPosType;
      AddL      : out IntAddrOutType;
      Done      : out std_logic;
      ThisIs    : out std_logic;
      DataOut   : out IntDataType
      );
  end component mem_interface_mux;


  signal iSelectedPos, nxSelectedPos     : SelectedPosType;
  signal nxSelectedPosH, nxSelectedPosHR : SelectedPosType;
  signal nxSelectedPosLR, nxSelectedPosL : SelectedPosType;
  signal thisIsL, thisIsH                : std_logic;
  signal thisIsLR, thisIsHR              : std_logic;

  signal addLL, addLLR : IntAddrOutType;
  signal addLH, addLHR : IntAddrOutType;

  signal dataFromIntDelayed                : IntDataType;
  signal intAddDelayed, addL, addOffSet    : IntAddrOutType;
  signal intReadDelayed, intWriteDelayed   : std_logic;
  signal intReadDelayed2, intWriteDelayed2 : std_logic;
  signal intReadDelayed3, intWriteDelayed3 : std_logic;
  signal opDoneMuxL, opDoneMuxLR           : std_logic;
  signal opDoneMuxH, opDoneMuxHR           : std_logic;
  signal iDataToIntH, iDataToIntHR         : IntDataType;
  signal iDataToIntL, iDataToIntLR         : IntDataType;

  signal iOpDone, opDoneMux : std_logic;
  signal doingOpC           : std_logic;
  signal iDataToInt         : IntDataType;
  signal dataMuxArrayIn     : MuxDataArrType;
  signal doneRam            : SelectedPosType;
  signal iOpDoneD           : std_logic;
begin


-- Address Decoder + Data Multiplexor
  UROMMUXH : mem_interface_mux
    generic map (INDH => NUMMEMPOSITION-1,
                 INDL => NUMMEMPOSITION/2)
    port map(
      IntAdd    => IntAdd,
      DoingOpC  => doingOpC,
      DoneRam   => doneRam,
      DataArray => dataMuxArrayIn,
      Sel       => nxSelectedPosH,
      AddL      => addLH,
      ThisIs    => thisIsH,
      Done      => opDoneMuxH,
      DataOut   => iDataToIntH
      );


  UROMMUXL : mem_interface_mux
    generic map (INDH => NUMMEMPOSITION/2 -1,
                 INDL => 0)
    port map(
      IntAdd    => IntAdd,
      DoingOpC  => doingOpC,
      DoneRam   => doneRam,
      DataArray => dataMuxArrayIn,
      Sel       => nxSelectedPosL,
      AddL      => addLL,
      ThisIs    => thisIsL,
      Done      => opDoneMuxL,
      DataOut   => iDataToIntL
      );

  process(Clk)
  begin
    if rising_edge(Clk) then
      if RstN = '0' then
        iDataToIntHR    <= (others => '0');
        iDataToIntLR    <= (others => '0');
        nxSelectedPosHR <= (others => '0');
        nxSelectedPosLR <= (others => '0');
        opDoneMuxHR     <= '0';
        opDoneMuxLR     <= '0';
        addLHR          <= (others => '0');
        addLLR          <= (others => '0');
        thisIsLR        <= '0';
        thisIsHR        <= '0';
      else
        iDataToIntHR    <= iDataToIntH;
        iDataToIntLR    <= iDataToIntL;
        nxSelectedPosHR <= nxSelectedPosH;
        nxSelectedPosLR <= nxSelectedPosL;
        opDoneMuxHR     <= opDoneMuxH;
        opDoneMuxLR     <= opDoneMuxL;
        addLHR          <= addLH;
        addLLR          <= addLL;
        thisIsLR        <= thisIsL;
        thisIsHR        <= thisIsH;
      end if;
    end if;
  end process;

  iDataToInt    <= iDataToIntHR when opDoneMuxHR = '1' else iDataToIntLR;
  addL          <= addLHR       when thisIsHR = '1'    else addLLR;
  opDoneMux     <= opDoneMuxHR or opDoneMuxLR;
  nxSelectedPos <= nxSelectedPosHR or nxSelectedPosLR;

  process(Clk)
  begin
    if rising_edge(Clk) then
      if RstN = '0' then

        iSelectedPos    <= (others => '0');
        intReadDelayed  <= '0';
        intWriteDelayed <= '0';

        intReadDelayed2  <= '0';
        intWriteDelayed2 <= '0';
        intReadDelayed3  <= '0';
        intWriteDelayed3 <= '0';
        iOpDoneD         <= '0';
        iOpDone          <= '0';
      else
        iSelectedPos       <= nxSelectedPos;
        dataFromIntDelayed <= DataFromInt;
        intAddDelayed      <= IntAdd;
        addOffSet          <= IntAdd - addL;

        if opDoneMux = '1' then
          DataToInt <= iDataToInt;
        end if;
        iOpDone         <= opDoneMux;
        intWriteDelayed <= IntWrite;

        intReadDelayed <= IntRead;

        intReadDelayed2  <= intReadDelayed;
        intWriteDelayed2 <= intWriteDelayed;
        intReadDelayed3  <= intReadDelayed2;
        intWriteDelayed3 <= intWriteDelayed2;
        iOpDoneD         <= iOpDone;
      end if;
    end if;
  end process;

  ContToMem.SelectedPos <= iSelectedPos;                --(nxSelectedPos'left downto FIRSTRAMPOS);
  OpDone                <= (not iOpDoneD) and iOpDone;  -- and (IntRead or IntWrite);

  doingOpC <= (IntRead or IntWrite);

  G0 : for I in 0 to NUMMEMPOSITION - 1 generate
    doneRam(I) <= MemToCont(I).RdDone;
  end generate;

  G2 : for I in nxSelectedPos'range generate
    dataMuxArrayIn(I) <= MemToCont(I).Data;
  end generate;

  ContToMem.Data <= dataFromIntDelayed;

  ContToMem.Add       <= intAddDelayed;
  ContToMem.AddOffSet <= addOffSet;


  ContToMem.Rd <= intReadDelayed2 and IntRead;  --and  (not(iOpDone)); -- RdMemFlag and not(iOpDone);
  ContToMem.Wr <= intWriteDelayed2 and IntWrite;  --and  (not(iOpDone)); -- intWriteDelayed;

  G3 : for I in iSelectedPos'range generate
    ContToMem.WrEn(I) <= intWriteDelayed2 and (not intWriteDelayed3) and iSelectedPos(I);
    ContToMem.RdEn(I) <= intReadDelayed2 and (not intReadDelayed3) and iSelectedPos(I);
  end generate;

end rtl;

