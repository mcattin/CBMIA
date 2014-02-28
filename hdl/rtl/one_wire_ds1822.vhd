-------------------------------------------------------------------------------
-- Title      : One-wire master interface for DS1822
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : one_wire_ds1822.vhd
-- Author     : Pablo Antonio Alvarez Sanchez
-- Company    : CERN (BE-CO-HT)
-- Created    : ?
-- Last update: 2012-03-13
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: One-wire master interface for DS1822
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
--             1.0      palvarez        Start from the interface for the DS2401.
--                                      Change Id Header to 0x"33" and add temperature request.
--             1.1      palvarez        Fixed bug on the state machine. It was sending a constant header
--                                      waitConvOp state removed
--             1.2      palvarez        Fixed bug on reset pulse
--             1.3      palvarez        Fixed bug skip command.
--             1.4      palvarez        Removed reset from conv and read temp commands. Added frequency generic
-- 2009-10-20  1.5      mcattin         Add comments, change to NUMERIC_STD library, delay pps 1 tick for the command FSM transitions
-- 2009-10-21  1.6      mcattin         Temp port size 16 bits (instead of 32), shift register was to small (64 bits)
--                                      to hold all data when reading temperature (increased to 72 bits)
-------------------------------------------------------------------------------
-- TODO: - 
--       - 
-------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;


entity one_wire_ds1822 is

  generic (
    FREQ : integer := 40                --Frequency in MHz
    );

  port(
    Clk  : in std_logic;
    RstN : in std_logic;

    SerialId : inout std_logic;                      -- IO to be connected to the chip (DS1822)
    Id       : out   std_logic_vector(63 downto 0);  -- ID value
    Temp     : out   std_logic_vector(15 downto 0);  -- Temperature value (refreshed every second)
    IdRead   : out   std_logic;                      -- ID value is valid
    Pps      : in    std_logic;                      -- Pulse per second (for temperature read)
    IdOk     : out   std_logic                       -- Same as IdRead, but not reset with RstN !!
    );

end one_wire_ds1822;


architecture rtl of one_wire_ds1822 is


  type stateOperationsType is (readIdOp, skipRomOp1, convertOp1, convertOp2, skipRomOp2, readTempOp);

  type stateCommandType is (resetCm, prepWriteCm, writeCm, prepReadCm, readCm, idleCm);
--                                                      resetConv, prepWriteConv, writeConv, readConv,
--                                                      resetTemp prepWriteTemp,writeTemp,prepReadTemp,ReadTemp,idleTemp);
  type stateBitType is (synPulseBit, holdBit, idleBit);

  constant slotCounterSTART        : unsigned(15 downto 0) := to_unsigned(0*FREQ/40, 16);
  constant slotCounterSTARTPLUSONE : unsigned(15 downto 0) := slotCounterSTART + 1;

  constant slotCounterSET     : unsigned(15 downto 0) := to_unsigned(60*FREQ/40, 16);
  constant slotCounterREAD    : unsigned(15 downto 0) := to_unsigned(600*FREQ/40, 16);
  constant slotCounterSTOP    : unsigned(15 downto 0) := to_unsigned(3600*FREQ/40, 16);
  constant slotCounterPRESTOP : unsigned(15 downto 0) := to_unsigned((3600-60)*FREQ/40, 16);

  constant READIDHEADER   : std_logic_vector(7 downto 0) := X"33";
  constant CONVERTHEADER  : std_logic_vector(7 downto 0) := X"44";
  constant READTEMPHEADER : std_logic_vector(7 downto 0) := X"BE";
  constant SKIPHEADER     : std_logic_vector(7 downto 0) := X"CC";

  constant IDLEFT      : integer   := 71;
  constant IDRIGHT     : integer   := 8;
  constant TEMPLEFT    : integer   := 15;
  constant TEMPRIGHT   : integer   := 0;
  constant TEMPDONEBIT : std_logic := '0';  -- The serial line is asserted to this value
                                            -- by the ds1822 when the temperature conversion is ready

  constant TEMPLENGTH : unsigned(7 downto 0) := to_unsigned(72, 8);
  constant IDLENGTH   : unsigned(7 downto 0) := to_unsigned(64, 8);
  signal   bitTop     : unsigned(7 downto 0);

  signal doReadBit, doWriteBit, doReset                       : std_logic;
  signal slotCounter                                          : unsigned(15 downto 0);
  signal startPulse, endPulse, setValue, readValue, initPulse : std_logic;
  signal stateOp, nxStateOp                                   : stateOperationsType;

  signal stateCm, nxStateCm                                    : stateCommandType;
  signal stateBit, nxStateBit                                  : stateBitType;
  signal bitCounter                                            : unsigned(7 downto 0);
  signal serialIdOut, iserialIdOe, nxSerialIdOut, nxSerialIdOe : std_logic;
  signal serialIdr                                             : std_logic;
  signal endWriteCm, endReadCm                                 : std_logic;
  signal incBitCnt, resetBitCnt                                : std_logic;
  signal shiftHeader, ldCmRg                                   : std_logic;
  signal cmRg                                                  : std_logic_vector(71 downto 0);
  signal shiftedHeader                                         : std_logic_vector(7 downto 0);
  signal preInitPulse                                          : std_logic;

  signal CrcVec, C                 : std_logic_vector(7 downto 0);
  signal crcOk, init, preReadPulse : std_logic;
  signal D                         : std_logic_vector(55 downto 0);
  signal iIdRead                   : std_logic;
  signal header                    : std_logic_vector(7 downto 0);
  signal loadTemp, loadId          : std_logic;
  signal commandOnly               : std_logic;
  signal pps_d                     : std_logic;


begin

  ------------------------------------------------------------------------------
  -- Serial data line
  -- In tri-state, when not writing data out
  SerialId <= serialIdOut when iserialIdOe = '1' else 'Z';
  ------------------------------------------------------------------------------


  ------------------------------------------------------------------------------
  -- PPS 1 clock tick delay
  PpsDelay_p : process (Clk)
  begin
    if rising_edge(Clk) then
      pps_d <= Pps;
    end if;
  end process PpsDelay_p;
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  -- Operations state machine
  OpFsmTransition_p : process(Clk)
  begin
    if rising_edge(Clk) then
      if RstN = '0' then
        stateOp <= readIdOp;
      else
        stateOp <= nxStateOp;
      end if;
    end if;
  end process OpFsmTransition_p;

  OpFsmStates_p : process(stateOp, Pps, crcOk)
  begin
    nxStateOp <= readIdOp;
    case stateOp is

      when readIdOp =>
        if Pps = '1' and crcOk = '1' then
          nxStateOp <= convertOp1;
        else
          nxStateOp <= stateOp;
        end if;

      when convertOp1 =>
        if Pps = '1' then
          nxStateOp <= skipRomOp1;
        else
          nxStateOp <= stateOp;
        end if;

      when skipRomOp1 =>
        if Pps = '1' then
          nxStateOp <= readTempOp;
        else
          nxStateOp <= stateOp;
        end if;

      when readTempOp =>
        if Pps = '1' then
          nxStateOp <= skipRomOp2;
        else
          nxStateOp <= stateOp;
        end if;

      when skipRomOp2 =>
        if Pps = '1' then
          nxStateOp <= convertOp2;
        else
          nxStateOp <= stateOp;
        end if;

      when convertOp2 =>
        if Pps = '1' then
          nxStateOp <= skipRomOp1;
        else
          nxStateOp <= stateOp;
        end if;

    end case;
  end process OpFsmStates_p;

  OpFsmOutputs_p : process(stateOp, stateCm, crcOk, pps, commandOnly)
  begin
    header      <= READIDHEADER;
    bitTop      <= IDLENGTH;
    loadTemp    <= '0';
    loadId      <= '0';
    commandOnly <= '0';

    case stateOp is

      when readIdOp =>
        header <= READIDHEADER;
        bitTop <= IDLENGTH;
        if stateCm = idleCm then
          loadId <= crcOk;
        end if;

      when convertOp1 =>
        header      <= CONVERTHEADER;
        commandOnly <= '1';

      when skipRomOp1 =>
        header      <= SKIPHEADER;
        commandOnly <= '1';

      when readTempOp =>
        header <= READTEMPHEADER;
        bitTop <= TEMPLENGTH;
        if stateCm = idleCm then
          loadTemp <= crcOk and pps;
        end if;

      when skipRomOp2 =>
        header      <= SKIPHEADER;
        commandOnly <= '1';

      when convertOp2 =>
        header      <= CONVERTHEADER;
        commandOnly <= '1';

      when others => null;

    end case;
  end process OpFsmOutputs_p;
  ------------------------------------------------------------------------------


  ------------------------------------------------------------------------------
  -- Commands state machine
  CmFsmTransitions_p : process(Clk)
  begin
    if rising_edge(Clk) then
      if RstN = '0' then
        stateCm <= resetCm;
      else
        stateCm <= nxStateCm;
      end if;
    end if;
  end process CmFsmTransitions_p;

  CmFsmStates_p : process(stateCm, startPulse, endWriteCm, endReadCm, crcOk, stateOp, commandOnly, pps_d)
  begin
    nxStateCm <= resetCm;
    case stateCm is
      when resetCm =>
        if startPulse = '1' then
          nxStateCm <= prepWriteCm;
        else
          nxStateCm <= stateCm;
        end if;

      when prepWriteCm =>
        if startPulse = '1' then
          nxStateCm <= writeCm;
        else
          nxStateCm <= stateCm;
        end if;

      when writeCm =>
        if endWriteCm = '1' then
          if commandOnly = '0' then
            nxStateCm <= prepReadCm;
          else
            nxStateCm <= idleCm;
          end if;
        else
          nxStateCm <= stateCm;
        end if;

      when prepReadCm =>
        if startPulse = '1' then
          nxStateCm <= readCm;
        else
          nxStateCm <= stateCm;
        end if;

      when readCm =>
        if endReadCm = '1' then
          nxStateCm <= idleCm;
        else
          nxStateCm <= stateCm;
        end if;

      when idleCm =>
        if stateOp = readIdOp then
          if crcOk = '0' then
            nxStateCm <= resetCm;
          else
            nxStateCm <= stateCm;
          end if;
        elsif stateOp = readTempOp then                              -- At this moment I will send a Conv Temp command
          if pps_d = '1' then
            nxStateCm <= prepWriteCm;
          else
            nxStateCm <= stateCm;
          end if;
        elsif (stateOp = convertOp1) or (stateOp = convertOp2) then  -- At this moment I will restart a Temp read
          if pps_d = '1' then
            nxStateCm <= prepWriteCm;
          else
            nxStateCm <= stateCm;
          end if;
        elsif (stateOp = skipRomOp1) or (stateOp = skipRomOp2) then  -- At this moment I will restart
          if pps_d = '1' then
            nxStateCm <= resetCm;
          else
            nxStateCm <= stateCm;
          end if;
        else
          nxStateCm <= stateCm;
        end if;
    end case;
  end process CmFsmStates_p;

  CmFsmOutputs_p : process(stateCm, bitCounter, preReadPulse, crcVec, startPulse,
                         shiftedHeader, initPulse, readValue, preInitPulse)
  begin
    incBitCnt     <= '0';
    nxSerialIdOut <= '0';
    shiftHeader   <= '0';
    ldCmRg        <= '0';
    nxserialIdOe  <= '0';
    resetBitCnt   <= '0';
    init          <= '0';
    crcOk         <= '0';
    case stateCm is
      when resetCm =>
        resetBitCnt   <= '1';
        nxSerialIdOut <= '0';
        nxserialIdOe  <= '1';
        init          <= startPulse;
      when prepWriteCm =>
        resetBitCnt   <= startPulse;
        nxserialIdOe  <= '0';
        nxSerialIdOut <= '0';
      when writeCm =>
        shiftHeader   <= startPulse;
        incBitCnt     <= startPulse;
        resetBitCnt   <= '0';
        nxSerialIdOut <= shiftedHeader(0) and (not initPulse);
        if bitCounter < to_unsigned(7, bitCounter'length) then
          nxserialIdOe <= not preInitPulse;
        else
          nxserialIdOe <= not preReadPulse;
        end if;
      when prepReadCm =>
        resetBitCnt   <= startPulse;
        nxserialIdOe  <= '0';
        nxSerialIdOut <= '0';
      when readCm =>
        incBitCnt     <= startPulse;
        resetBitCnt   <= '0';
        nxSerialIdOut <= not initPulse;
        ldCmRg        <= readValue;
        nxserialIdOe  <= initPulse;
      when idleCm =>
        if crcVec = x"00" then
          crcOk <= '1';
        else
          crcOk <= '0';
        end if;
        init <= '1';
    end case;
  end process CmFsmOutputs_p;
  ------------------------------------------------------------------------------


  ------------------------------------------------------------------------------
  -- Generates time slots
  -- Reset pulse
  -- Read time slot
  -- Write time slots
  process(Clk)
  begin
    if rising_edge(Clk) then
      if RstN = '0' then
        slotCounter(slotCounter'left)             <= '1';
        slotCounter(slotCounter'left -1 downto 0) <= (others => '0');
        startPulse                                <= '0';
        endPulse                                  <= '0';
        setValue                                  <= '0';
        readValue                                 <= '0';
        initPulse                                 <= '0';
        preInitPulse                              <= '0';
        preReadPulse                              <= '0';
      else

        -- Slot counter
        if init = '1' then
          slotCounter(slotCounter'left)              <= '1';
          slotCounter(slotCounter'left - 1 downto 0) <= (others => '0');
        elsif slotCounter = slotCounterSTOP then
          slotCounter <= (others => '0');
        else
          slotCounter <= slotCounter + 1;
        end if;

        -- Time slot start pulse
        if slotCounter = slotCounterSTART then
          startPulse <= '1';
        else
          startPulse <= '0';
        end if;

        if ((slotCounter > slotCounterSTART) and (slotCounter < slotCounterSET)) then
          initPulse <= '1';
        else
          initPulse <= '0';
        end if;

        if ((slotCounter > slotCounterPRESTOP) and (slotCounter < slotCounterSTOP)) then
          preInitPulse <= '1';
        else
          preInitPulse <= '0';
        end if;

        if (((slotCounter > slotCounterPRESTOP) and (slotCounter <= slotCounterSTOP)) or
            (slotCounter                                         <= slotCounterSTARTPLUSONE)) then
          preReadPulse <= '1';
        else
          preReadPulse <= '0';
        end if;

        -- End of time slot pulse
        if slotCounter = slotCounterSTART then
          endPulse <= '1';
        else
          endPulse <= '0';
        end if;

        -- Pulse to write value on serial link
        if slotCounter = slotCounterSET then
          setValue <= '1';
        else
          setValue <= '0';
        end if;

        -- Pulse to read value on serial link
        if slotCounter = slotCounterREAD then
          readValue <= '1';
        else
          readValue <= '0';
        end if;
      end if;
    end if;
  end process;
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  -- Data serializer bit counter
  BitCnt_p : process(Clk)
  begin
    if rising_edge(Clk) then
      if RstN = '0' then
        bitCounter <= (others => '0');
      else
        if resetBitCnt = '1' then
          bitCounter <= (others => '0');
        elsif incBitCnt = '1' then
          bitCounter <= bitCounter + 1;
        end if;
      end if;
    end if;
  end process BitCnt_p;
  ------------------------------------------------------------------------------


  ------------------------------------------------------------------------------
  -- Data serializer shift register
  ShiftReg_p : process(Clk)
  begin
    if rising_edge(Clk) then
      if RstN = '0' then
        shiftedHeader <= READIDHEADER;
        cmRg          <= (others => '0');
        SerialIdr     <= '0';
        serialIdOut   <= '0';
        iserialIdOe   <= '0';
        Id            <= (others => '0');
        iIdRead       <= '0';
        IdRead        <= '0';
        crcVec        <= (others => '0');
        Temp          <= (others => '0');
      else
        -- Samples serial input
        SerialIdr <= SerialId;

        -- Shifts command out
        if init = '1' then
          shiftedHeader <= header;
        elsif shiftHeader = '1' then
          shiftedHeader(shiftedHeader'left-1 downto 0) <= shiftedHeader(shiftedHeader'left downto 1);
          shiftedHeader(shiftedHeader'left)            <= '0';
        end if;

        -- Computes CRC on read data (include the received CRC itself, if no errror crcVec = X"00")
        if init = '1' then
          crcVec <= (others => '0');
        elsif ldCmRg = '1' then
          crcVec(0)          <= serialIdr xor crcVec(7);
          crcVec(3 downto 1) <= crcVec(2 downto 0);
          crcVec(4)          <= (serialIdr xor crcVec(7)) xor crcVec(3);
          crcVec(5)          <= (serialIdr xor crcVec(7)) xor crcVec(4);
          crcVec(7 downto 6) <= crcVec(6 downto 5);
        end if;

        -- Stores incomming data
        if (ldCmRg = '1') then
          cmRg(cmRg'left - 1 downto 0) <= cmRg(cmRg'left downto 1);
          cmRg(cmRg'left)              <= serialIdr;
        end if;

        -- Updates serial ouptut data
        serialIdOut <= nxSerialIdOut;

        -- Updates serial output enable
        iserialIdOe <= nxSerialIdOe;

        -- Stores ID in register
        if (loadId = '1')then
          iIdRead <= '1';
          Id      <= cmRg(IDLEFT downto IDRIGHT);
        end if;

        -- Stores Temperature in register
        if (loadTemp = '1')then
          Temp <= cmRg(TEMPLEFT downto TEMPRIGHT);
        end if;

        -- Delays ID read
        IdRead <= iIdRead;
      end if;
    end if;
  end process ShiftReg_p;
  ------------------------------------------------------------------------------


  ------------------------------------------------------------------------------
  -- Value on Id port is valid
  process(Clk)
  begin
    if rising_edge(Clk) then
      if stateCm = idleCm then
        IdOk <= crcOk;
      end if;
    end if;
  end process;
  ------------------------------------------------------------------------------


  ------------------------------------------------------------------------------
  -- Detects end of read or end of write command
  endWriteCm <= '1' when (bitCounter = to_unsigned(7, bitCounter'length)) and (incBitCnt = '1') else '0';
  endReadCm  <= '1' when (bitCounter = bitTop)                                                  else '0';
  ------------------------------------------------------------------------------

end rtl;

