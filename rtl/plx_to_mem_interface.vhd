-------------------------------------------------------------------------------
-- Title      : PLX local bus to memory interface
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : plx_to_mem_interface.vhd
-- Author     : Pablo Antonio Alvarez Sanchez
-- Company    : CERN (BE-CO-HT)
-- Created    : 2008-05-26
-- Last update: 2012-03-12
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
-- 2008-05-26  1.0      paas            Version with Watchdog from Pablo
-- 2012-03-09  1.1      mcattin         Clean-up, add license
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


entity plx_to_mem_interface is

  generic(
    LALEFT  : integer := 2;
    LARIGHT : integer := 23
    );

  port (
    LClk    : in    std_logic;
    RstN    : in    std_logic;
    LAdSN   : in    std_logic;
    LA      : in    std_logic_vector(LARIGHT downto LALEFT);
    LData   : inout std_logic_vector(31 downto 0);
    LWrRdN  : in    std_logic;
    LReadyN : out   std_logic;

    AddrMem     : out IntAddrOutType;
    ReadMem     : out std_logic;
    WriteMem    : out std_logic;
    OpDone      : in  std_logic;
    DataFromMem : in  std_logic_vector(31 downto 0);
    DataToMem   : out std_logic_vector(31 downto 0)
    );

end plx_to_mem_interface;


architecture rtl of plx_to_mem_interface is

  constant ReadMemPos  : integer := 0;
  constant WriteMemPos : integer := 1;
  constant OpDonePos   : integer := 2;
  constant LReadyPos   : integer := 3;
  constant idlePos     : integer := 4;
  constant idlePos2    : integer := 5;

  constant idleSt               : std_logic_vector(5 downto 0) := "000000";  --00
  constant wrRdyNLowSt          : std_logic_vector(5 downto 0) := "011000";  --18
  constant wrRdyNHighWtWrDoneSt : std_logic_vector(5 downto 0) := "010010";  --12

  constant waitOpDoneToWr : std_logic_vector(5 downto 0) := "000010";  --02
  constant waitOpDoneToRd : std_logic_vector(5 downto 0) := "100010";  --22

  constant rdRdyNHighWtRdDoneSt : std_logic_vector(5 downto 0) := "010001";  --21
  constant rdRdyNLowSt          : std_logic_vector(5 downto 0) := "001000";  --04

  constant LAONE : std_logic_vector(LARIGHT - LALEFT downto 0) := conv_std_logic_vector(1, LARIGHT - LALEFT + 1);

  signal LAIOFF, LAReg, LARegSum : std_logic_vector(LARIGHT - LALEFT downto 0);
  signal LARegT                  : std_logic_vector(LARIGHT - LALEFT downto 0);

  signal inLDReg                           : std_logic_vector (31 downto 0);
  signal outLDReg                          : std_logic_vector(31 downto 0);
  signal LAdSNIOFF, LAdSNReg               : std_logic;
  signal LCsReg                            : std_logic;
  signal WrReg                             : std_logic;
  signal readyWr                           : std_logic;
  signal pciSt, nxPciSt                    : std_logic_vector(5 downto 0);
  signal startWriteMem, startReadMem       : std_logic;
  signal iLReadyN                          : std_logic;
  signal lAdBus, lAdT                      : std_logic;
  signal watchdog                          : std_logic_vector(6 downto 0);
  signal incWatchDog, iRstN, nxIncWatchDog : std_logic;

begin

  process(LClk)
  begin
    if rising_edge(LClk) then
      if incWatchDog = '1' then
        watchdog <= watchdog + conv_std_logic_vector(1, watchdog'length);
      else
        watchdog <= conv_std_logic_vector(0, watchdog'length);
      end if;
      incWatchDog <= nxIncWatchDog;
    end if;
  end process;

  iRstN <= (not watchdog(watchdog'left)) and RstN;

  process(LClk, iRstN)
  begin
    if rising_edge(LClk) then
      if iRstN = '0' then
        LAReg     <= (others => '0');
        LARegT    <= (others => '0');
        inLDReg   <= (others => '0');
        LAdSNReg  <= '1';
        WrReg     <= '1';
        iLReadyN  <= '1';
        pciSt     <= idleSt;
        LAIOFF    <= (others => '0');
        LAdSNIOFF <= '1';

      else

        LAIOFF <= LA;

        if lAdBus = '1' then
          LAReg <= LAIOFF;
        elsif lAdT = '1' then
          LAReg <= LARegT;
        end if;

        if LAdSNReg = '1' then
          LARegT <= LAIOFF;
        end if;

        if pciSt(LReadyPos) = '1' then
          inLDReg <= LData;
        end if;

        LAdSNIOFF <= LAdSN;
        LAdSNReg  <= LAdSNIOFF;
        WrReg     <= LWrRdN;
        pciSt     <= nxPciSt;

      end if;
    end if;
  end process;

  outLDReg  <= DataFromMem;
  LReadyN   <= not (pciSt(LReadyPos));
  ReadMem   <= pciSt(ReadMemPos);
  WriteMem  <= pciSt(WriteMemPos);
  DataToMem <= inLDReg;
  LData     <= outLDReg when LWrRdN = '0' else (others => 'Z');

  process(pciSt, startWriteMem, opDone, startReadMem)
  begin
    nxIncWatchDog <= '0';
    nxPciSt       <= idleSt;
    lAdBus        <= '0';
    lAdT          <= '0';
    case pciSt is
      when idleSt => if startWriteMem = '1' then
                       nxPciSt <= wrRdyNLowSt;
                       lAdBus  <= '1';
                     elsif startReadMem = '1' then
                       nxPciSt <= rdRdyNHighWtRdDoneSt;
                       lAdBus  <= '1';
                     end if;
      when wrRdyNLowSt =>
        nxPciSt <= wrRdyNHighWtWrDoneSt;

      when wrRdyNHighWtWrDoneSt =>
        nxIncWatchDog <= '1';
        if startWriteMem = '1' then
          if opDone = '1' then          --opDoneRe = '1' and LBLastNReg = '0' then
            nxPciSt <= wrRdyNLowSt;
            lAdT    <= '1';
          else
            nxPciSt <= waitOpDoneToWr;
          end if;

        elsif startReadMem = '1' then
          nxIncWatchDog <= '1';
          if opDone = '1' then          --opDoneRe = '1' and LBLastNReg = '0' then
            nxPciSt <= rdRdyNHighWtRdDoneSt;
            lAdT    <= '1';
          else
            nxPciSt <= waitOpDoneToRd;
          end if;
        elsif opDone = '1' then         --opDoneRe = '1' and LBLastNReg = '0' then
          nxPciSt <= idleSt;
        else
          nxPciSt <= wrRdyNHighWtWrDoneSt;
        end if;
      when waitOpDoneToWr =>
        nxIncWatchDog <= '1';
        if opDone = '1' then            --opDoneRe = '1' and LBLastNReg = '0' then
          nxPciSt <= wrRdyNLowSt;
          lAdT    <= '1';
        else
          nxPciSt <= waitOpDoneToWr;
        end if;
      when waitOpDoneToRd =>
        nxIncWatchDog <= '1';
        if opDone = '1' then            --opDoneRe = '1' and LBLastNReg = '0' then
          nxPciSt <= rdRdyNHighWtRdDoneSt;
          lAdT    <= '1';
        else
          nxPciSt <= waitOpDoneToRd;
        end if;
      when rdRdyNHighWtRdDoneSt =>
        nxIncWatchDog <= '1';
        if opDone = '1' then            --and LBLastNReg = '0' then
          nxPciSt <= rdRdyNLowSt;
        else
          nxPciSt <= rdRdyNHighWtRdDoneSt;
        end if;
      when rdRdyNLowSt =>
        nxPciSt <= idleSt;
      when others => nxPciSt <= idleSt;

    end case;
  end process;

  AddrMem <= LAReg(AddrMem'range);

  startWriteMem <= (WrReg) and (not LAdSNReg);
  startReadMem  <= not(WrReg) and (not LAdSNReg);


end rtl;
