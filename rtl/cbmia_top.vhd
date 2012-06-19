-------------------------------------------------------------------------------
-- Title      : Top level entity of CBMIA hdl design
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : cbmia_top.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-02-29
-- Last update: 2012-03-13
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: Top entity of CBMIA hdl design.
--              The CBMIA is a MIL1553 bus controller. In this design, the
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
-- 2012-02-29  1.0      mcattin         Created
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


entity cbmia_top is
  generic(
    g_HW_VERSION : std_logic_vector(15 downto 0) := X"0201"
    );
  port (
    -- description -> net name in schematics


    -- Power-ON reset -> PWRESETN
    ----------------------------------------------------------------------------
    pwr_reset_n_i : in std_logic;

    -- System clock -> CLKX1
    ----------------------------------------------------------------------------
    clk_i : in std_logic;

    -- MIL1553 interface
    ----------------------------------------------------------------------------
    -- Serial data input -> RXDATA3V
    mil1553_rxd_a_i    : in  std_logic;
    -- Manchester decoder fault? -> MDFAULT3V (not connected in schematics)
    mil1553_md_fault_i : in  std_logic;
    -- Low  = enable receiver and disable transmitter
    -- High = disable receiver and enable transmitter -> TX-RXN
    mil1553_tx_rx_n_o  : out std_logic;
    -- Low = enable transmitter -> TXN
    mil1553_tx_n_o     : out std_logic;
    -- Serial data output -> TXDATA
    mil1553_txd_o      : out std_logic;
    -- Serial data output inversed -> TXDATAN
    mil1553_txd_n_o    : out std_logic;

    -- Front panel LEDs -> LEDOUT7 .. LEDOUT0
    ----------------------------------------------------------------------------
    led_o : out std_logic_vector(7 downto 0);

    -- Test points -> X3 .. X0
    ----------------------------------------------------------------------------
    test_point_o : out std_logic_vector (3 downto 0);

    -- RS232 interface
    ----------------------------------------------------------------------------
    -- Serial data input -> RS232IN
    rs232_i : in  std_logic;
    -- Serial data output -> RS232OUT
    rs232_o : out std_logic;

    -- One wire interface to DS1822 (unique ID + thermometer) -> SERNUM
    ----------------------------------------------------------------------------
    onewire_b : inout std_logic;

    -- PLX PCI9030 local bus interface (non-multiplexed local bus mode)
    -- See "PCI 9030 Data Book Version 1.4", chapter 11 "PIN DESCRIPTION"
    ----------------------------------------------------------------------------
    -- Read strobe -> LREAD
    l_rd_n_i    : in    std_logic;
    -- Write strobe -> LWRITE
    l_wr_n_i    : in    std_logic;
    -- Reset from PCI -> LRESET
    l_reset_n_i : in    std_logic;
    -- Chip Select -> LCS1, LCS0
    l_cs_n_i    : in    std_logic_vector(1 downto 0);
    -- Address strobe -> LADS
    l_ads_n_i   : in    std_logic;
    -- Address latch enable -> LALE
    l_ale_i     : in    std_logic;
    -- Burst last -> LBLAST
    l_blast_n_i : in    std_logic;
    -- Burst Terminate -> LBTERM
    l_btrem_n_o : out   std_logic;
    -- Address bus -> LADDRESS23 .. LADDRESS2
    l_address_i : in    std_logic_vector(23 downto 2);
    -- Data bus -> LDATA
    l_data_b    : inout std_logic_vector(31 downto 0);
    -- Byte enable -> LBE3 .. LBE0
    l_be_i      : in    std_logic_vector(3 downto 0);
    -- Write/Read -> LWR
    l_wr_rd_n_i : in    std_logic;
    -- Ready -> LREADY
    l_ready_n_o : out   std_logic;
    -- Interrupt 1 -> LINT1
    l_int1_o    : out   std_logic;
    -- Interrrupt 2 -> LINT2
    l_int2_o    : out   std_logic;
    -- General purpose I/O -> LGPIO
    l_gpio_b    : inout std_logic_vector(8 downto 4);


    -- SRAM interface (not used)
    ----------------------------------------------------------------------------
    ram_address_o : out   std_logic_vector(17 downto 0);
    ram_data_b    : inout std_logic_vector(31 downto 0);
    ram_par_o     : out   std_logic_vector(4 downto 1);
    ram_zz_o      : out   std_logic;
    ram_oe_n_o    : out   std_logic;
    ram_lbo_n_o   : out   std_logic;
    ram_gw_n_o    : out   std_logic;
    ram_ce_n_o    : out   std_logic;
    ram_cs0_o     : out   std_logic;
    ram_cs1_n_o   : out   std_logic;
    ram_bwe_n_o   : out   std_logic;
    ram_bw_n_o    : out   std_logic_vector(4 downto 1);
    ram_adv_n_o   : out   std_logic;
    ram_adsp_n_o  : out   std_logic;
    ram_adsc_n_o  : out   std_logic;
    ram_clk_o     : out   std_logic
    );
end cbmia_top;


architecture rtl of cbmia_top is

  ----------------------------------------------------------------------------
  -- Signals declaration
  ----------------------------------------------------------------------------
  signal pwr_rst_sync_n : std_logic_vector(1 downto 0);
  signal pwr_rst_n      : std_logic;

  signal rd_to_mem     : std_logic;       -- Read strobe to memory interface
  signal wr_to_mem     : std_logic;       -- Write strobe to memory interface
  signal data_from_mem : IntDataType;     -- Data from memory interface
  signal addr_to_mem   : IntAddrOutType;  -- Address to memory interface
  signal data_to_mem   : IntDataType;     -- Data to memory interface
  signal op_done       : std_logic;       -- Operation done from memory interface
                                          -- Read or Write finished
  signal irq_req       : std_logic_vector(1 downto 0);


begin

  ----------------------------------------------------------------------------
  -- Synchronises power-on reset to system clock
  ----------------------------------------------------------------------------
  p_rst : process (clk)
  begin
    if rising_edge(clk) then
      pwr_rst_sync_n <= pwr_rst_sync_n(0) & pwr_reset_n_i;
    end if;
  end process p_rst;
  pwr_rst_n <= pwr_rst_sync_n(1);

  ----------------------------------------------------------------------------
  -- Components instantiation
  ----------------------------------------------------------------------------
  cmp_plx_to_mem_interface : plx_to_mem_interface
    generic map(
      LALEFT  <= 2,
      LARIGHT <= 23
      )
    port map(
      LClk        <= clk_i,
      RstN        <= pwr_rst_n,
      LAdSN       <= l_ads_n_i,
      LA          <= l_address_i,
      LData       <= l_data_b,
      LWrRdN      <= l_wr_rd_n_i,
      LReadyN     <= l_ready_n_o,
      AddrMem     <= addr_to_mem,
      ReadMem     <= rd_to_mem,
      WriteMem    <= wr_to_mem,
      OpDone      <= op_done,
      DataFromMem <= data_from_mem,
      DataToMem   <= data_to_mem
      );

  cmp_mil1553_core : mil1553_core
    generic map(
      g_HW_VERSION <= g_HW_VERSION
      )
    port map(
      pwr_reset_n_i     <= pwr_rst_n,
      sys_clk_i         <= clk_i,
      mil1553_rxd_a_i   <= mil1553_rxd_a_i,
      mil1553_tx_rx_n_o <= mil1553_tx_rx_n_o,
      mil1553_tx_n_o    <= mil1553_tx_n_o,
      mil1553_txd_o     <= mil1553_txd_o,
      mil1553_txd_n_o   <= mil1553_txd_n_o,
      led_o             <= led_o,
      test_point_o      <= test_point_o,
      onewire_b         <= onewire_b,
      rd_to_mem_i       <= rd_to_mem,
      wr_to_mem_i       <= wr_to_mem,
      data_from_mem_o   <= data_from_mem,
      addr_to_mem_i     <= addr_to_mem,
      data_to_mem_i     <= data_to_mem,
      op_done_o         <= op_done,
      irq_req_o         <= irq_req
      );

  l_int1_o <= irq_req(0);
  l_int2_o <= irq_req(1);

  ----------------------------------------------------------------------------
  -- Unused output assigment
  ----------------------------------------------------------------------------

  -- PLX
  l_btrem_n_o <= '1';
  l_gpio_b    <= (others => 'Z');

  -- RS232
  rs232_o <= '0';

  -- SRAM
  ram_address_o <= (others => '0');
  ram_data_b    <= (others => 'Z');
  ram_par_o     <= (others => '0');
  ram_zz_o      <= '1';
  ram_oe_n_o    <= '1';
  ram_lbo_n_o   <= '1';
  ram_gw_n_o    <= '1';
  ram_ce_n_o    <= '1';
  ram_cs0_o     <= '0';
  ram_cs1_n_o   <= '1';
  ram_bwe_n_o   <= '1';
  ram_bw_n_o    <= (others => '1');
  ram_adv_n_o   <= '1';
  ram_adsp_n_o  <= '1';
  ram_adsc_n_o  <= '1';
  ram_clk_o     <= '0';


end architecture rtl;
