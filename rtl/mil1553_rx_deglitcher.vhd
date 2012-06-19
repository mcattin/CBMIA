-------------------------------------------------------------------------------
-- Title      : Deglitcher
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mil1553_rx.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-03-02
-- Last update: 2012-03-02
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: Deglitcher for MIL1553 serial input
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
-- 2012-03-02  1.0      mcattin         Created, based on nanoFIP deglitcher
-------------------------------------------------------------------------------
-- TODO: - 
--       - 
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
library work;
use work.cbmia_pkg.all;


entity mil1553_rx_deglitcher is
  port (

    -- Global ports
    ----------------------------------------------------------------------------
    sys_rst_n_i : in std_logic;         -- Synchronous system reset (active low)
    sys_clk_i   : in std_logic;         -- System clock

    -- Serial input
    ----------------------------------------------------------------------------
    rxd_a_i : in std_logic;             -- Serial data input

    -- Deglitched outputs
    ----------------------------------------------------------------------------
    rxd_filt_o          : out std_logic;  -- filtered output signal
    rxd_filt_edge_p_o   : out std_logic;  -- indicates an edge on the filtered signal
    rxd_filt_f_edge_p_o : out std_logic   -- indicates a falling edge on the filtered signal

    );
end mil1553_rx_deglitcher;

architecture rtl of mil1553_rx_deglitcher is

  signal rxd_sync          : std_logic_vector (1 downto 0);
  signal rxd_filt          : std_logic;
  signal rxd_filt_d1       : std_logic;
  signal rxd_filt_r_edge_p : std_logic;
  signal rxd_filt_f_edge_p : std_logic;
  signal filt_cnt          : unsigned (3 downto 0);

begin

  ------------------------------------------------------------------------------
  -- Synchronises MIL1553 serial input to system clock
  ------------------------------------------------------------------------------
  p_rxd_sync : process (sys_clk_i)
  begin
    if rising_edge (sys_clk_i) then
      if sys_rst_n_i = '0' then
        rxd_sync <= (others => '0');
      else
        rxd_sync <= rxd_sync(0) & rxd_a_i;
      end if;
    end if;
  end process p_rxd_sync;

  ------------------------------------------------------------------------------
  -- Deglitching
  ------------------------------------------------------------------------------
  -- The output signal rxd_filt is updated only after the accumulation of
  -- a sufficient (c_DEGLITCH_THRESHOLD + 1) amount of identical bits. The
  -- signal is therefore cleaned of any glitches up to c_DEGLITCH_THRESHOLD
  -- sys_clk_i ticks long.
  ------------------------------------------------------------------------------
  p_deglitcher : process (sys_clk_i)
  begin
    if rising_edge (sys_clk_i) then
      if sys_rst_n_i = '0' then
        filt_cnt    <= to_unsigned (c_DEGLITCH_THRESHOLD, filt_cnt'length) srl 1;  -- middle value
        rxd_filt    <= '0';
        rxd_filt_d1 <= '0';
      else

        if rxd_sync(1) = '0' then       -- arrival of a '0'

          if filt_cnt /= 0 then         -- counter updated
            filt_cnt <= filt_cnt - 1;
          else
            rxd_filt <= '0';            -- output updated
          end if;

        elsif rxd_sync(1) = '1' then    -- arrival of a '1'

          if filt_cnt /= c_DEGLITCH_THRESHOLD then
            filt_cnt <= filt_cnt + 1;   -- counter updated
          else
            rxd_filt <= '1';            -- output updated
          end if;

        end if;

        rxd_filt_d1 <= rxd_filt;        -- used for the edges detection

      end if;
    end if;
  end process p_deglitcher;

  ------------------------------------------------------------------------------
  -- Edges detection
  ------------------------------------------------------------------------------
  rxd_filt_r_edge_p <= (not rxd_filt_d1) and rxd_filt;  -- pulse upon detection
                                                        -- of a falling edge

  rxd_filt_f_edge_p   <= rxd_filt_d1 and (not rxd_filt);  -- pulse upon detection
                                                          -- of a rising edge
  ------------------------------------------------------------------------------
  -- Output signals assignment
  ------------------------------------------------------------------------------
  rxd_filt_edge_p_o   <= rxd_filt_f_edge_p or rxd_filt_r_edge_p;
  rxd_filt_f_edge_p_o <= rxd_filt_f_edge_p;
  rxd_filt_o          <= rxd_filt;

end architecture rtl;
