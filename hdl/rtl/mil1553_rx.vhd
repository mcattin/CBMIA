-------------------------------------------------------------------------------
-- Title      : MIL1553 receiver
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mil1553_rx.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-02-29
-- Last update: 2012-04-12
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: MIL1553 receiver. The MIL1553 serial input is first filtered
--              through a deglitcher block, then the data are deserialised.
--              A clock recovery block allows to sample the serial data stream
--              and decode the Manchester encoding.
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


entity mil1553_rx is

  port (

    -- Global ports
    ----------------------------------------------------------------------------
    sys_rst_n_i : in std_logic;         -- Synchronous system reset (active low)
    sys_clk_i   : in std_logic;         -- System clock

    -- MIL1553 interface
    ----------------------------------------------------------------------------
    mil1553_rxd_i   : in std_logic;     -- Serial data input
    mil1553_rx_en_i : in std_logic;     -- Receiver enable

    -- User interface
    ----------------------------------------------------------------------------
    expected_nb_word_i  : in  std_logic_vector(5 downto 0);  -- Expected nb of word from RT (including the status word)
    rx_buffer_o         : out t_rx_buffer_array;             -- Receive buffer
    rx_word_cnt_o       : out std_logic_vector(5 downto 0);  -- Number of words in the receive buffer
    rx_in_progress_o    : out std_logic;                     -- Frame reception in progress
    rx_done_p_o         : out std_logic;                     -- End of frame reception
    rx_parity_error_p_o : out std_logic;                     -- Parity error detected
    rx_manch_error_p_o  : out std_logic;                     -- Manchester code violation detected
    rx_error_p_o        : out std_logic                      -- Reception error (watchdog timout)

    );

end mil1553_rx;


architecture rtl of mil1553_rx is

  ----------------------------------------------------------------------------
  -- Signals declaration
  ----------------------------------------------------------------------------
  signal mil1553_rxd        : std_logic;
  signal rx_clk_rst         : std_logic;
  signal sample_manch_bit_p : std_logic;
  signal sample_bit_p       : std_logic;
  signal signif_edge_window : std_logic;
  signal adjac_bits_window  : std_logic;
  signal rxd_filt           : std_logic;
  signal rxd_filt_f_edge_p  : std_logic;
  signal rxd_filt_r_edge_p  : std_logic;
  signal rxd_filt_edge_p    : std_logic;

begin

  ------------------------------------------------------------------------------
  -- Ignore data when transmitting
  ------------------------------------------------------------------------------
  mil1553_rxd <= mil1553_rxd_i and mil1553_rx_en_i;

  ------------------------------------------------------------------------------
  -- MIL1553 clock recovery
  ------------------------------------------------------------------------------
  cmp_mil1553_rx_clk : mil1553_rx_clk
    port map(
      sys_rst_n_i             => sys_rst_n_i,
      sys_clk_i               => sys_clk_i,
      rxd_edge_p_i            => rxd_filt_edge_p,
      rx_clk_rst_i            => rx_clk_rst,
      rx_manch_clk_p_o        => sample_manch_bit_p,
      rx_bit_clk_p_o          => sample_bit_p,
      rx_signif_edge_window_o => signif_edge_window,
      rx_adjac_bits_window_o  => adjac_bits_window
      );

  ------------------------------------------------------------------------------
  -- MIL1553 serial input glitch filter and edge detection
  ------------------------------------------------------------------------------
  cmp_mil1553_rx_deglitcher : mil1553_rx_deglitcher
    port map(
      sys_rst_n_i         => sys_rst_n_i,
      sys_clk_i           => sys_clk_i,
      rxd_a_i             => mil1553_rxd,
      rxd_filt_o          => rxd_filt,
      rxd_filt_edge_p_o   => rxd_filt_edge_p,
      rxd_filt_f_edge_p_o => rxd_filt_f_edge_p,
      rxd_filt_r_edge_p_o => rxd_filt_r_edge_p
      );

  ------------------------------------------------------------------------------
  -- MIL1553 deserialiser and RX buffer
  ------------------------------------------------------------------------------
  cmp_mil1553_rx_deserialiser : mil1553_rx_deserialiser
    port map(
      sys_rst_n_i          => sys_rst_n_i,
      sys_clk_i            => sys_clk_i,
      rxd_i                => rxd_filt,
      rxd_f_edge_p_i       => rxd_filt_f_edge_p,
      rxd_r_edge_p_i       => rxd_filt_r_edge_p,
      sample_bit_p_i       => sample_bit_p,
      sample_manch_bit_p_i => sample_manch_bit_p,
      signif_edge_window_i => signif_edge_window,
      adjac_bits_window_i  => adjac_bits_window,
      rx_en_i              => mil1553_rx_en_i,
      rx_clk_rst_o         => rx_clk_rst,
      expected_nb_word_i   => expected_nb_word_i,
      rx_buffer_o          => rx_buffer_o,
      rx_word_cnt_o        => rx_word_cnt_o,
      rx_in_progress_o     => rx_in_progress_o,
      rx_done_p_o          => rx_done_p_o,
      rx_parity_error_p_o  => rx_parity_error_p_o,
      rx_manch_error_p_o   => rx_manch_error_p_o,
      rx_error_p_o         => rx_error_p_o
      );


end architecture rtl;
