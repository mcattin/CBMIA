-------------------------------------------------------------------------------
-- Title      : PLX PCI9030 local bus to wishbone interface
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : plx_to_wb.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-02-29
-- Last update: 2012-03-01
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: PLX PCI9030 local bus to wishbone interface
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
-- TODO: - Watchdog ??
--       - 
-------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

entity plx_to_wb is
  port (
    -- Global ports
    ----------------------------------------------------------------------------
    sys_rst_n_i : in std_logic;         -- Synchronous system reset (active low)
    sys_clk_i   : in std_logic;         -- System clock

    -- PLX PCI9030 local bus interface
    ----------------------------------------------------------------------------
    l_rd_n_i    : in    std_logic;                      -- Read strobe
    l_wr_n_i    : in    std_logic;                      -- Write strobe
--    l_reset_n_i : in    std_logic;                      -- Reset from PCI
    l_cs_n_i    : in    std_logic_vector(1 downto 0);   -- Chip Select
    l_ads_n_i   : in    std_logic;                      -- Address strobe
    l_ale_i     : in    std_logic;                      -- Address latch enable
    l_blast_n_i : in    std_logic;                      -- Burst last
    l_btrem_n_o : out   std_logic;                      -- Burst Terminate
    l_address_i : in    std_logic_vector(23 downto 2);  -- Address bus
    l_data_b    : inout std_logic_vector(31 downto 0);  -- Data bus
    l_be_i      : in    std_logic_vector(3 downto 0);   -- Byte enable
    l_wr_rd_n_i : in    std_logic;                      -- Write/Read
    l_ready_n_o : out   std_logic;                      -- Ready
    l_int1_o    : out   std_logic;                      -- Interrupt 1
    l_int2_o    : out   std_logic;                      -- Interrrupt 2
    l_gpio_b    : inout std_logic_vector(8 downto 4);   -- General purpose I/O

    -- Wishbone interface
    ----------------------------------------------------------------------------
    wb_clk_i   : in  std_logic;
    wb_adr_o   : out std_logic_vector(31 downto 0);  -- Address bus output
    wb_dat_o   : out std_logic_vector(31 downto 0);  -- Data bus output
    wb_sel_o   : out std_logic_vector(3 downto 0);   -- Data byte select
    wb_stb_o   : out std_logic;                      -- Strobe
    wb_we_o    : out std_logic;                      -- Write enable
    wb_cyc_o   : out std_logic;                      -- Cycle
    wb_dat_i   : in  std_logic_vector(31 downto 0);  -- Data bus input
    wb_ack_i   : in  std_logic;                      -- Acknowledge
    wb_stall_i : in  std_logic;                      -- Pipeline stall
    wb_irq_i   : in  std_logic                       -- Interrupt request
    );
end plx_to_wb;

architecture rtl of plx_to_wb is

  type   t_wb_state is (WB_IDLE, WB_CYCLE, WB_WAIT_ACK);
  signal wb_state : t_wb_state;

  signal wb_we_t     : std_logic;
  signal l_data_i_d  : std_logic_vector(31 downto 0);
  signal l_data_o_d  : std_logic_vector(31 downto 0);
  signal l_address_d : std_logic_vector(21 downto 0);

  signal start_wr_cyc : std_logic;
  signal start_rd_cyc : std_logic;

begin

  -----------------------------------------------------------------------------
  -- Note:
  --   The PLX PXI9030 local bus is clocked with the same clock as the FPGA,
  --   therefore there is no need to synchronise input signals.
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Unused local bus signals
  -----------------------------------------------------------------------------
  l_gpio_b    <= (others => 'Z');
  l_btrem_n_o <= 'Z';                   -- only used for burst transfers
  --l_cs_n_i(1); -- useless as only the FPGA is connected to the PLX chip
  --l_cs_n_i(0); -- useless as only the FPGA is connected to the PLX chip
  --l_ale_i      -- only used in multiplexed mode

  start_wr_cyc <= l_wr_rd_n_i and not(l_ads_n_i);
  start_rd_cyc <= not(l_wr_rd_n_i) and not(l_ads_n_i);

  -----------------------------------------------------------------------------
  -- Data to local bus
  -----------------------------------------------------------------------------
  l_data_b <= l_data_o_d when l_wr_rd_n_i = '0' else (others => 'Z');

  -----------------------------------------------------------------------------
  -- Registers address from local bus
  -----------------------------------------------------------------------------
  p_addr : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if sys_rst_n_i = '0' then
        l_address_d <= (others => '0');
      elsif l_ads_n_i = '0' then
        l_address_d <= l_address_i;
      end if;
    end if;
  end process p_addr;

  -----------------------------------------------------------------------------
  -- Registers data from local bus
  -----------------------------------------------------------------------------
  p_addr : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if sys_rst_n_i = '0' then
        l_data_i_d <= (others => '0');
      elsif l_blast_n_i = '0' then
        l_data_i_d <= l_data_b;
      end if;
    end if;
  end process p_addr;

  -----------------------------------------------------------------------------
  -- Wishbone master FSM
  -----------------------------------------------------------------------------
  p_wb_fsm : process (sys_clk_i, sys_rst_n_i)
  begin
    if(rst_n_i = c_RST_ACTIVE) then
      wb_state    <= WB_IDLE;
      wb_cyc_o    <= '0';
      wb_stb_o    <= '0';
      wb_we_t     <= '0';
      wb_sel_o    <= "0000";
      wb_dat_o    <= (others => '0');
      wb_adr_o    <= (others => '0');
      l_data_o_d  <= (others => '0');
      l_ready_n_o <= '1';
    elsif rising_edge(sys_clk_i) then
      case wb_state is

        when WB_IDLE =>
          -- Ends write cycle to local bus
          l_ready_n_o <= '1';
          -- Clear bus
          wb_cyc_o    <= '0';
          wb_stb_o    <= '0';
          wb_sel_o    <= "0000";
          -- Wait for a PLX local bus cycle
          if (l_ads_n_i = '0') then
            wb_state <= WB_CYCLE;
          end if;

        when WB_CYCLE =>
          -- Initate a bus cycle
          wb_cyc_o <= '1';
          wb_stb_o <= '1';
          wb_we_t  <= l_wr_rd_n_i;
          wb_sel_o <= l_be_i;
          wb_adr_o <= l_address_d;
          wb_dat_o <= l_data_i_d;
          -- Wait for slave to ack
          wb_state <= WB_WAIT_ACK;

        when WB_WAIT_ACK =>
          if wb_stall_i = '0' then
            wb_stb_o <= '0';
          end if;
          if (wb_ack_i = '1') then
            -- For read cycles, write data to local bus
            if (wb_we_t = '0') then
              l_data_o_d  <= wb_dat_i;
              l_ready_n_o <= '0';
            end if;
            -- End of the bus cycle
            wb_cyc_o <= '0';
            wb_state <= WB_IDLE;
          end if;

        when others =>
          -- Should not get here!
          wb_state <= WB_IDLE;
          wb_cyc_o <= '0';
          wb_stb_o <= '0';
          wb_we_t  <= '0';
          wb_sel_o <= "0000";
          wb_dat_o <= (others => '0');
          wb_adr_o <= (others => '0');

      end case;
    end if;
  end process p_wb_fsm;

  wb_we_o <= wb_we_t;

end architecture rtl;
