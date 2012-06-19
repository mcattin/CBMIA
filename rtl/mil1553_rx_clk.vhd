-------------------------------------------------------------------------------
-- Title      : MIL1553 serial clock recovery
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mil1553_rx_clk.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-03-02
-- Last update: 2012-03-14
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: MIL1553 serial clock recovery
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
-- 2012-03-02  1.0      mcattin         Created, based on nanoFIP clock recovery
-------------------------------------------------------------------------------
-- TODO: - 
--       - 
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
library work;
use work.cbmia_pkg.all;


entity mil1553_rx_clk is
  port (

    -- Global ports
    ----------------------------------------------------------------------------
    sys_rst_n_i : in std_logic;         -- Synchronous system reset (active low)
    sys_clk_i   : in std_logic;         -- System clock

    -- Signal from deglitcher
    ----------------------------------------------------------------------------
    rxd_edge_p_i : in std_logic;        -- Indication of an edge on rxd

    -- Signals from deserialiser
    rx_clk_rst_i     : in  std_logic;   -- resets the clock recovery procedure
    rx_manch_clk_p_o : out std_logic;   -- signal with sys_clk-wide pulses
                                        --  o  on a significant edge 
                                        --  o  between adjacent bits
                                        --  ____|-|___|-|___|-|___

    rx_bit_clk_p_o : out std_logic;     -- signal with sys_clk-wide pulses
                                        --  o between adjacent bits
                                        --  __________|-|_________

    rx_signif_edge_window_o : out std_logic;  -- time window where a significant edge is expected

    rx_adjac_bits_window_o : out std_logic  -- time window where a transition between adjacent
                                            -- bits is expected

    );
end mil1553_rx_clk;

architecture rtl of mil1553_rx_clk is

  -- reception period counter
  signal s_period_c              : unsigned (c_PERIODS_CNT_WIDTH-1 downto 0);
  signal s_period                : unsigned (c_PERIODS_CNT_WIDTH-1 downto 0);
  signal s_margin                : unsigned (c_PERIODS_CNT_WIDTH-1 downto 0);
  signal s_half_period           : unsigned (c_PERIODS_CNT_WIDTH-1 downto 0);
  signal s_period_c_reinit       : std_logic;
  signal s_period_c_is_full      : std_logic;
  -- windows formed, based on the counter
  signal s_adjac_bits_window     : std_logic;
  signal s_signif_edge_window    : std_logic;
  -- fd_rxd signal combined with the windows
  signal s_adjac_bits_edge_found : std_logic;
  signal s_signif_edge_found     : std_logic;
  -- clocks
  signal s_bit_clk               : std_logic;
  signal s_bit_clk_d1            : std_logic;
  signal s_manch_clk             : std_logic;
  signal s_manch_clk_d1          : std_logic;

begin

  ------------------------------------------------------------------------------
  -- 
  ------------------------------------------------------------------------------
  ---------------------------------------------------------------------------------------------------
--                  Generation of windows where edges/ transitions are expected                  --
---------------------------------------------------------------------------------------------------

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  --  # sys_clk ticks for a bit period, defined by the WorldFIP bit rate
  s_period      <= c_BIT_RATE_SYS_CLK_TICKS(to_integer(unsigned(rate_i)));
  s_half_period <= s_period srl 1;      -- 1/2 s_period
  s_margin      <= s_period srl 3;      -- margin for jitter defined as 1/8 of the period


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
-- Instantiation of a wf_incr_counter unit : the rx_counter starts counting after the
-- release of the reset signal rx_osc_rst_i. This takes place after a falling edge on the
-- filtered FD_RXD; this edge should be representing the 1st Manchester 2 (manch.) encoded bit '1'
-- of the PREamble. Starting from this edge, other falling or rising significant edges, are
-- expected around one period (s_period) later. A time window around the expected arrival time is
-- set and its length is defined as 1/4th of the period (1/8th before and 1/8th after the expected
-- time). When the actual edge arrives, the counter is reset.
-- If that first falling edge of FD_RXD is finally proven not to belong to a valid PRE the counter
-- is reinitialialized through the rx_osc_rst_i signal from the wf_rx_deserializer.

  rx_periods_count : incr_counter
    generic map(
      g_counter_lgth => c_PERIODS_COUNTER_LGTH
      )
    port map(
      sys_clk_i         => sys_clk_i,
      counter_reinit_i  => s_period_c_reinit,
      counter_incr_i    => '1',
      counter_is_full_o => open,
      ------------------------------------------
      counter_o         => s_period_c
      );
  ------------------------------------------

  s_period_c_is_full <= '1' when s_period_c = s_period -1 else '0';  -- counter full indicator

  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  -- counter reinitialized: if nfip_rst_i is active                       or
  --                        if rx_osc_rst_i is active                     or
  --                        if an edge is detected in the expected window or
  --                        if it fills up
  s_period_c_reinit <= nfip_rst_i or rx_osc_rst_i or (s_signif_edge_window and fd_rxd_edge_p_i)
                       or s_period_c_is_full;


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
-- Concurrent signal assignments: creation of the windows where
-- "significant edges" and "adjacent bits transitions" are expected on the input signal.
--   o s_signif_edge_window: extends s_margin sys_clk ticks before and s_margin sys_clk ticks after
--     the completion of a period, where significant edges are expected.
--   o s_adjac_bits_window : extends s_margin sys_clk ticks before and s_margin sys_clk ticks after
--     the middle of a period, where transitions between adjacent bits are expected.

  s_signif_edge_window <= '1' when ((s_period_c < s_margin) or
                                    (s_period_c > s_period-1 - s_margin-1)) else '0';


  s_adjac_bits_window <= '1' when ((s_period_c >= s_half_period-s_margin-1) and
                                   (s_period_c < s_half_period+s_margin)) else '0';



---------------------------------------------------------------------------------------------------
--                                      Clocks Generation                                        --
---------------------------------------------------------------------------------------------------

-- Synchronous process rx_clks: the process rx_clk is following the edges that appear on the fd_rxd
-- and constructs two clock signals: rx_manch_clk & rx_bit_clk.

-- The signal rx_manch_clk: is inverted on each significant edge, as well as between adjacent bits
-- The signal rx_bit_clk  : is inverted only between adjacent bits

-- The significant edges are normally expected inside the signif_edge_window. In the cases of a
-- code violation (V+ or V-) no edge will arrive in this window. In this situation rx_manch_clk
-- is inverted right after the end of the signif_edge_window.

-- Edges between adjacent bits are expected inside the adjac_bits_window; if they do not arrive
-- the rx_manch_clk and rx_bit_clk are inverted right after the end of the adjac_bits_window.

  rx_clks : process (sys_clk_i)

  begin
    if rising_edge (sys_clk_i) then
      if (nfip_rst_i = '1') then
        s_manch_clk             <= '0';
        s_bit_clk               <= '0';
        s_bit_clk_d1            <= '0';
        s_manch_clk_d1          <= '0';
        s_signif_edge_found     <= '0';
        s_adjac_bits_edge_found <= '0';

      else
        --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -
        -- regarding significant edges:

        -- looking for a significant edge inside the corresponding window
        if (s_signif_edge_window = '1') and (fd_rxd_edge_p_i = '1') and (s_signif_edge_found = '0') then

          s_manch_clk             <= not s_manch_clk;  -- inversion of rx_manch_clk
          s_signif_edge_found     <= '1';              -- indication that the edge was found
          s_adjac_bits_edge_found <= '0';

          -- if a significant edge is not found where expected (code violation), the rx_manch_clk
          -- is inverted right after the end of the signif_edge_window.
        elsif (s_signif_edge_found = '0') and (s_period_c = s_margin) then

          s_manch_clk             <= not s_manch_clk;
          s_adjac_bits_edge_found <= '0';  -- re-initialization before the
                                           -- next cycle


          --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -
          -- regarding edges between adjacent bits:

          -- looking for an edge inside the corresponding window
        elsif (s_adjac_bits_window = '1') and (fd_rxd_edge_p_i = '1') then

          s_manch_clk             <= not s_manch_clk;  -- inversion of rx_manch_clk
          s_bit_clk               <= not s_bit_clk;    -- inversion of rx_bit_clk
          s_adjac_bits_edge_found <= '1';              -- indication that an edge was found

          s_signif_edge_found <= '0';   -- re-initialization before next cycle


          -- if no edge is detected inside the adjac_bits_edge_window, both clks are inverted right
          -- after the end of it
        elsif (s_adjac_bits_edge_found = '0') and (s_period_c = s_half_period + s_margin) then

          s_manch_clk <= not s_manch_clk;
          s_bit_clk   <= not s_bit_clk;

          s_signif_edge_found <= '0';   -- re-initialization before next cycle
        end if;

        --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        s_manch_clk_d1 <= s_manch_clk;
                                        -- s_manch_clk      : ____|-----|_____|-----|____
                                        -- s_manch_clk_d1   : ______|-----|_____|-----|__
                                        -- rx_manch_clk_p_o : ____|-|___|-|___|-|___|-|__

        s_bit_clk_d1 <= s_bit_clk;
                                        -- s_bit_clk        : ____|-----------|__________
                                        -- s_bit_clk_d1     : ______|-----------|________
                                        -- rx_bit_clk_p_o   : ____|-|_________|-|________

      end if;
    end if;
  end process;



---------------------------------------------------------------------------------------------------
--                                 Concurrent signal assignments                                 --
---------------------------------------------------------------------------------------------------

  rx_manch_clk_p_o <= s_manch_clk_d1 xor s_manch_clk;  -- a 1 sys_clk-wide pulse, after
                                                       --  o a significant edge and
                                                       --  o a new bit
                                                       -- ___|-|___|-|___|-|___

  rx_bit_clk_p_o <= s_bit_clk xor s_bit_clk_d1;  -- a 1 sys_clk-wide pulse, after
                                                 --  o a new bit
                                                 -- _________|-|_________

  rx_signif_edge_window_o <= s_signif_edge_window;

  rx_adjac_bits_window_o <= s_adjac_bits_window;


end architecture rtl;
