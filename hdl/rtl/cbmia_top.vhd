-------------------------------------------------------------------------------
-- Title      : Top level entity of CBMIA hdl design
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : cbmia_top.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-02-29
-- Last update: 2013-08-26
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
-- DATE        VERSION  AUTHOR          DESCRIPTION
-- 2012-02-29  2.01     mcattin         Created from original CBMIA design.
-- 2012-03-19  2.02     mcattin         Doesn't start RX FSM when transmitting,
--                                      add temperature readout.
-- 2012-03-21  2.03     mcattin         Add TR flag in the interrupt source reg
-- 2012-03-22  2.04     mcattin         Add error flags in interrupt source reg,
--                                      add more status registers at the end
--                                      of the memory map, add test point mux.
-- 2012-03-29  2.05     mcattin         Fix a bug in word count comparator.
--                                      When sub-address/mode field of a command
--                                      word is either 0 or 31, the word count
--                                      field indicates a mode code (not a word
--                                      count).
-- 2012-03-30  2.06     mcattin         Don't stop reception on Manchester error,
--                                      only generate a error pulse.
-- 2012-04-04  2.07     mcattin         Add a test to take the recepetion FSM out
--                                      of reception state in case of Manchester
--                                      error after a frame (e.g. spurious data
--                                      sync pattern at the end of a frame).
-- 2012-04-12  2.08     mcattin         Frame reception ends either when expected
--                                      nb of words is received or when the bus
--                                      is quiet ( = no transitions are detected
--                                      on the bus for 3.2us).
--                                      When a Manchester error is detected, the
--                                      reception is stopped and waits for the
--                                      bus to be quiet to give the IRQ.
--                                      Internal word counters are now 6-bit.
--                                      Additional reg, reception error counter.
-- 2012-04-16  2.09     mcattin         Add response timeout event counter.
-- 2012-04-16  2.10     mcattin         Change RX FSM to always pass to DONE
--                                      state before going back to IDLE state.
-- 2012-04-16  2.11     mcattin         Replace "Transaction end" by "irq req"
--                                      in the test points muxes.
-- 2013-06-25  2.12     mcattin         Add a register to save the unique id read
--                                      from the DS1822 chip.
-- 2013-08-26  2.13     mcattin         Connects PLX reset output line (l_reset_n_i)
--                                      to FPGA logic.
-------------------------------------------------------------------------------
-- TODO: - 
--       - 
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
library work;
use work.cbmia_pkg.all;
use work.mem_interface_pkg.all;
library UNISIM;
use UNISIM.VComponents.all;

entity cbmia_top is
  generic(
    g_HW_VERSION : std_logic_vector(15 downto 0) := X"0213"
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
  signal rst_inputs : std_logic;
  signal rst_sync_n : std_logic_vector(1 downto 0);
  signal rst_n      : std_logic;

  signal sys_clk : std_logic;

  signal l_ready_n : std_logic;

  signal rd_to_mem     : std_logic;     -- Read strobe to memory interface
  signal wr_to_mem     : std_logic;     -- Write strobe to memory interface
  signal data_from_mem : t_int_data;    -- Data from memory interface
  signal addr_to_mem   : t_int_addr;    -- Address to memory interface
  signal data_to_mem   : t_int_data;    -- Data to memory interface
  signal op_done       : std_logic;     -- Operation done from memory interface
                                        -- Read or Write finished
  signal irq_req       : std_logic_vector(1 downto 0);

  signal led : std_logic_vector(6 downto 0);


begin

  ----------------------------------------------------------------------------
  -- Clock buffer (BUFG)
  ---------------------------------------------------------------------------
  cmp_bufg : BUFG
    port map (
      I => clk_i,
      O => sys_clk
      );

  ----------------------------------------------------------------------------
  -- Synchronises power-on and PLX9030 reset inputs with system clock
  ----------------------------------------------------------------------------
  rst_inputs <= pwr_reset_n_i and l_reset_n_i;

  p_rst : process (sys_clk)
  begin
    if rising_edge(sys_clk) then
      rst_sync_n <= rst_sync_n(0) & rst_inputs;
    end if;
  end process p_rst;
  rst_n <= rst_sync_n(1);

  ----------------------------------------------------------------------------
  -- PLX to memory interface
  ----------------------------------------------------------------------------
  cmp_plx_to_mem_interface : plx_to_mem_interface
    generic map(
      LALEFT  => 2,
      LARIGHT => 23
      )
    port map(
      LClk        => sys_clk,
      RstN        => rst_n,
      LAdSN       => l_ads_n_i,
      LA          => l_address_i,
      LData       => l_data_b,
      LWrRdN      => l_wr_rd_n_i,
      LReadyN     => l_ready_n,
      AddrMem     => addr_to_mem,
      ReadMem     => rd_to_mem,
      WriteMem    => wr_to_mem,
      OpDone      => op_done,
      DataFromMem => data_from_mem,
      DataToMem   => data_to_mem
      );

  l_ready_n_o <= l_ready_n;

  ----------------------------------------------------------------------------
  -- MIL1553 core
  ----------------------------------------------------------------------------
  cmp_mil1553_core : mil1553_core
    generic map(
      g_HW_VERSION => g_HW_VERSION
      )
    port map(
      pwr_reset_n_i     => rst_n,
      sys_clk_i         => sys_clk,
      mil1553_rxd_a_i   => mil1553_rxd_a_i,
      mil1553_tx_rx_n_o => mil1553_tx_rx_n_o,
      mil1553_tx_n_o    => mil1553_tx_n_o,
      mil1553_txd_o     => mil1553_txd_o,
      mil1553_txd_n_o   => mil1553_txd_n_o,
      led_o             => led,
      test_point_o      => test_point_o,
      onewire_b         => onewire_b,
      rd_to_mem_i       => rd_to_mem,
      wr_to_mem_i       => wr_to_mem,
      data_from_mem_o   => data_from_mem,
      addr_to_mem_i     => addr_to_mem,
      data_to_mem_i     => data_to_mem,
      op_done_o         => op_done,
      irq_req_o         => irq_req
      );

  l_int1_o <= irq_req(0);
  l_int2_o <= irq_req(1);

  ----------------------------------------------------------------------------
  -- PCI bus access LED
  -- LED 6 (green)
  ----------------------------------------------------------------------------
  cmp_monostable_led7 : monostable
    generic map(
      g_INPUT_POLARITY  => '0',
      g_OUTPUT_POLARITY => '1',
      g_OUTPUT_RETRIG   => false,
      g_OUTPUT_LENGTH   => c_LED_MONOSTABLE_LENGTH
      )
    port map(
      rst_n_i   => rst_n,
      clk_i     => sys_clk,
      trigger_i => l_ready_n,
      pulse_o   => led_o(6)
      );

  led_o(5 downto 0) <= led(5 downto 0);
  led_o(7)          <= led(6);

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
