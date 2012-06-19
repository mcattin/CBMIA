-------------------------------------------------------------------------------
-- Title      : MIL1553 core
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mil1553_core.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-03-12
-- Last update: 2012-03-13
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: MIL1553 bus controller. In this design, the
--              MIL1553 bus speed is fixed at 1Mbit/s.
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
-- 2012-03-12  1.0      mcattin         Created
-------------------------------------------------------------------------------
-- TODO: - 
--       - 
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
library work;
use work.cbmia_pkg.all;
use work.bus_interface_pkg.all;


entity mil1553_core is
  generic(
    g_HW_VERSION : std_logic_vector(15 downto 0) := X"0000"
    );
  port (

    -- Global ports
    ----------------------------------------------------------------------------
    pwr_reset_n_i : in std_logic;       -- Power-ON reset
    sys_clk_i     : in std_logic;       -- System clock

    -- MIL1553 interface
    ----------------------------------------------------------------------------
    mil1553_rxd_a_i   : in  std_logic;  -- Serial data input
    mil1553_tx_rx_n_o : out std_logic;  -- Transmitter/receiver enable
    mil1553_tx_n_o    : out std_logic;  -- Enable transmitter
    mil1553_txd_o     : out std_logic;  -- Serial data output
    mil1553_txd_n_o   : out std_logic;  -- Serial data output inversed

    -- Front panel LEDs
    ----------------------------------------------------------------------------
    led_o : out std_logic_vector(7 downto 0);

    -- Test points
    ----------------------------------------------------------------------------
    test_point_o : out std_logic_vector (3 downto 0);

    -- One wire interface to DS1822 (unique ID + thermometer)
    ----------------------------------------------------------------------------
    onewire_b : inout std_logic;

    -- Bus interface
    ----------------------------------------------------------------------------
    rd_to_mem_i     : in  std_logic;       -- Read strobe to memory interface
    wr_to_mem_i     : in  std_logic;       -- Write strobe to memory interface
    data_from_mem_o : out IntDataType;     -- Data from memory interface
    addr_to_mem_i   : in  IntAddrOutType;  -- Address to memory interface
    data_to_mem_i   : in  IntDataType;     -- Data to memory interface
    op_done_o       : out std_logic;       -- Operation done from memory interface

    -- Interrupts request
    ----------------------------------------------------------------------------
    irq_req_o : out std_logic_vector(1 downto 0)

    );
end mil1553_core;


architecture rtl of mil1553_core is

  ----------------------------------------------------------------------------
  -- Signals declaration
  ----------------------------------------------------------------------------
  signal to_regs   : ContToMemType;     -- Data going from Control to the Registers
                                        -- This consists of Data + Enable + Read + Write
  signal from_regs : MemToContType(0 to NUMMEMPOSITION - 1);

  signal irq_src        : std_logic_vector(31 downto 0);
  signal irq_src_reg    : std_logic_vector(31 downto 0);
  signal irq_en_msk_reg : std_logic_vector(31 downto 0);


begin

  ----------------------------------------------------------------------------
  -- synchronous system reset (power-on and PCI command)
  ----------------------------------------------------------------------------
  rst_n <= pwr_reset_n_i and not(ResetFromPci);

  ----------------------------------------------------------------------------
  -- Components instantiation
  ----------------------------------------------------------------------------
  cmp_mem_interface : mem_interface
    port map(
      Clk         <= sys_clk_i,
      RstN        <= pwr_reset_n_i,
      IntRead     <= rd_to_mem_i,
      IntWrite    <= wr_to_mem_i,
      DataFromInt <= data_to_mem_i,
      IntAdd      <= addr_to_mem_i,
      OpDone      <= op_done_o,
      DataToInt   <= data_from_mem_o,
      ContToMem   <= to_regs,
      MemToCont   <= from_regs
      );

  cmp_irq_regs : irq_regs
    generic map(
      g_REG_WIDTH <= 32
      )
    port map(
      clk_i          <= sys_clk_i,
      rst_n_i        <= pwr_reset_n_i,
      irq_src_i      <= irq_src,
      data_wr_i      <= ContToMem.Data,
      irq_en_wren_i  <= ContToMem.WrEn(InterruptEnableP),
      irq_en_o       <= irq_en_msk_reg,
      irq_src_o      <= irq_src_reg,
      irq_src_rden_i <= ContToMem.RdEn(InterruptSourceP),
      irq_req_o      <= irq_req_o
      );

  cmp_one_wire_ds1822 : one_wire_ds1822
    generic map(
      FREQ <= 40                        -- in MHz
      )
    port map(
      Clk      <= sys_clk_i,
      RstN     <= rst_n,
      SerialId <= onewire_b,
      Id       <= open,                 -- 64-bit
      Temp     <= open,                 -- 16-bit
      IdRead   <= open,
      Pps      <= '0',                  -- pulse per second for temp read
      IdOk     <= open,
      );

  ----------------------------------------------------------------------------
  -- Registers readback
  ----------------------------------------------------------------------------
  p_reg_rd : process(irq_en_msk_reg, irq_src_reg,
                     id, DRTIReg, ExtWriteAdd, DataReceived,
                     StrBitNotBusy, StrBitBusy, CommandReg, DisablePoll, ErrorReg,
                     iRTIReceived, DataToSend, WordToSend, LWDataReceived, WordData,
                     TXWordData, RTIAddress, iM1553TX_RXN, M1553ActivRx, FrameType,
                     data_ready_cnt, start_me_cnt, write_during_tx_cnt,
                     write_req_during_tx_cnt)
  begin

    l_rd_done : for I in MemToCont'range loop
      MemToCont(I).rdDone <= '0';
    end loop;

    MemToCont(MIDP).data             <= std_logic_vector(data_ready_cnt);  --id(63 downto 32);
    MemToCont(LIDP).data             <= std_logic_vector(start_me_cnt);    --id(31 downto 0);
    MemToCont(CommandP).data         <= CommandReg;
    MemToCont(RTIPRESENTP).Data      <= DRTIReg;
    MemToCont(CIRCADDP).Data         <= std_logic_vector(write_during_tx_cnt) & std_logic_vector(write_req_during_tx_cnt);
    MemToCont(FAULTADDLOW).Data      <= x"12345678";                       -- in case of outside address range
    MemToCont(FAULTADD).Data         <= x"87654321";                       -- in case of outside address range
    MemToCont(SOURCEREGP).data       <= StrBitNotBusy & StrBitBusy & '0' & DisablePoll & FrameType & M1553ActivRx & iM1553TX_RXN & RTIAddress & ErrorReg & HARDVERSION;
    MemToCont(InterruptEnableP).Data <= irq_en_msk_reg;
    MemToCont(InterruptSourceP).Data <= iRTIReceived & DataToSend(4 downto 0) & irq_src_reg(21 downto 0);
    MemToCont(TXREGP).Data           <= WordToSend & DataToSend;
    MemToCont(RXREGP).Data           <= LWDataReceived;

    WDATALOOP : for I in 0 to 16 loop    -- Read the RX buffer - 16 because RX buffer is 17 x 32 bits
      MemToCont(W1 + I).Data <= WordData(I*2 + 1) & WordData(I*2);
    end loop;
    TXWDATALOOP : for I in 0 to 15 loop  -- Read the TX buffer
      MemToCont(TXW1 + I).Data <= TXWordData(I*2 + 1) & TXWordData(I*2);
    end loop;

  end process;

  ----------------------------------------------------------------------------
  -- Fill the TX buffer from PCI Bus
  ----------------------------------------------------------------------------
  l_tx_buffer : for I in 0 to 15 generate
    process(sys_clk_i)
    begin
      if rising_edge(sys_clk_i) then
        if rst_n = '0' then
          TXWordData(2*I)   <= (others => '0');
          TXWordData(2*I+1) <= (others => '0');
        elsif ContToMem.WrEn(TXW1+I) = '1' then
          TXWordData(2*I)   <= ContToMem.Data(15 downto 0);
          TXWordData(2*I+1) <= ContToMem.Data(31 downto 16);
        end if;
      end if;
    end process;
  end generate;

  ------------------------------------------------------------------------------
  -- Command register
  ------------------------------------------------------------------------------
  p_cmd_reg : process(sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        BusSpeed     <= DEFAULT_BUSSPEED;
        CommandReg   <= (others => '0');
        DisablePoll  <= '1';
        ResetFromPci <= '0';
      elsif ContToMem.WrEn(CommandP) = '1' then
        CommandReg   <= ContToMem.Data;  --DataRec;
        DisablePoll  <= ContToMem.Data(CommandENABLEP);
        ResetFromPci <= ContToMem.Data(CommandRESETP);
        BusSpeed     <= ContToMem.Data(CommandBUSSPEEDP downto CommandBUSSPEEDP - 1);
      else
        ResetFromPci              <= '0';
        CommandReg(CommandRESETP) <= '0';
      end if;
    end if;
  end process p_cmd_reg;


end architecture rtl;
