-------------------------------------------------------------------------------
-- Title      : CBMIA package
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : cbmia_pkg.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-02-29
-- Last update: 2012-03-15
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: Package for CBMIA hdl design.
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
use work.mem_interface_pkg.all;


package cbmia_pkg is

  -----------------------------------------------------------------------------
  -- Constants declaration
  -----------------------------------------------------------------------------
  constant c_SYS_CLK_PERIOD         : integer  := 25;  -- in [ns]
  constant c_PERIODS_CNT_WIDTH      : integer  := 6;   -- Number of bits needed for the
                                                       -- Manchester period counter
                                                       -- -> at 1Mbit/s
  constant c_BIT_RATE_SYS_CLK_TICKS : unsigned :=
    to_unsigned((1000/c_SYS_CLK_PERIOD), c_PERIODS_CNT_WIDTH);

  constant c_DEGLITCH_THRESHOLD : integer := 1;  -- Serial input glitch filter threshold
                                                 -- pulses < c_DEGLITCH_THRESHOLD * sys_clk ticks
                                                 -- are filtered out by the glitch filter

  constant c_TX_BUFFER_SIZE : integer := 33;  -- Transmit buffer size, in 16-bit words
  constant c_RX_BUFFER_SIZE : integer := 33;  -- Receive buffer size, in 16-bit words

  -- Bit positions in MIL1553 command word
  constant c_CMD_WC0 : integer := 0;    -- Word count bit 0
  constant c_CMD_WC1 : integer := 1;    -- Word count bit 1
  constant c_CMD_WC2 : integer := 2;    -- Word count bit 2
  constant c_CMD_WC3 : integer := 3;    -- Word count bit 3
  constant c_CMD_WC4 : integer := 4;    -- Word count bit 4
  constant c_CMD_SA0 : integer := 5;    -- Sub-address bit 0
  constant c_CMD_SA1 : integer := 6;    -- Sub-address bit 1
  constant c_CMD_SA2 : integer := 7;    -- Sub-address bit 2
  constant c_CMD_SA3 : integer := 8;    -- Sub-address bit 3
  constant c_CMD_SA4 : integer := 9;    -- Sub-address bit 4
  constant c_CMD_TR  : integer := 10;   -- Transmit/Receive bit
  constant c_CMD_RT0 : integer := 11;   -- RT address bit 0
  constant c_CMD_RT1 : integer := 12;   -- RT address bit 1
  constant c_CMD_RT2 : integer := 13;   -- RT address bit 2
  constant c_CMD_RT3 : integer := 14;   -- RT address bit 3
  constant c_CMD_RT4 : integer := 15;   -- RT address bit 4

  -- Bit positions in MIL1553 status word
  constant c_STA_TF  : integer := 0;    -- Terminal flag bit
  constant c_STA_DBC : integer := 1;    -- Dynamic bus control bit
  constant c_STA_SF  : integer := 2;    -- Sub-system flag bit
  constant c_STA_BUY : integer := 3;    -- Busy bit
  constant c_STA_BRO : integer := 4;    -- Broadcast bit
  constant c_STA_TB  : integer := 5;    -- Transmit buffer busy bit
  constant c_STA_RB  : integer := 6;    -- Receive buffer busy bit
  constant c_STA_TIM : integer := 7;    -- Timeout bit
  constant c_STA_SR  : integer := 8;    -- Service request bit
  constant c_STA_INS : integer := 9;    -- Instrumentation bit
  constant c_STA_ME  : integer := 10;   -- Message error bit
  constant c_STA_RT0 : integer := 11;   -- RT address bit 0
  constant c_STA_RT1 : integer := 12;   -- RT address bit 1
  constant c_STA_RT2 : integer := 13;   -- RT address bit 2
  constant c_STA_RT3 : integer := 14;   -- RT address bit 3
  constant c_STA_RT4 : integer := 15;   -- RT address bit 4

  -- Transmit/receive bit meaning
  constant c_TR_WRITE : std_logic := '0';  -- BC writes to RT
  constant c_TR_READ  : std_logic := '0';  -- BC reads from RT

  -- MIL1553 data encoding
  constant c_MANCH_ZERO      : std_logic_vector(1 downto 0) := "01";      -- Manchester encoded zero
  constant c_MANCH_ONE       : std_logic_vector(1 downto 0) := "10";      -- Manchester encoded one
  constant c_ODD_PARITY      : std_logic                    := '0';       -- Odd parity
  constant c_CMD_SYNC_FIELD  : std_logic_vector(5 downto 0) := "111000";  -- Command word synchronisation field
  constant c_DATA_SYNC_FIELD : std_logic_vector(5 downto 0) := "000111";  -- Data word synchronisation field

  -- MIL1553 decoding
  constant c_STAT_SYNC_FIELD : std_logic_vector(1 downto 0) := "00";  -- Status word synchronisation field (as seen by the deserialiser)

  --
  constant c_LED_MONOSTABLE_LENGTH : natural := 400000;  -- number of system clock ticks, 

  -----------------------------------------------------------------------------
  -- Types declaration
  -----------------------------------------------------------------------------
  type t_tx_buffer_array is array (0 to c_TX_BUFFER_SIZE - 1) of std_logic_vector(15 downto 0);
  type t_tx_buffer_encoded_array is array (0 to c_TX_BUFFER_SIZE - 1) of std_logic_vector(39 downto 0);

  type t_rx_buffer_array is array (0 to c_RX_BUFFER_SIZE - 1) of std_logic_vector(15 downto 0);

  -----------------------------------------------------------------------------
  -- Functions declaration
  -----------------------------------------------------------------------------
  function f_log2_ceil(N          :    natural) return positive;
  function f_manch_encoder(word_i : in std_logic_vector(15 downto 0)) return std_logic_vector;
  function f_parity_check(word_i  : in std_logic_vector(16 downto 0)) return std_logic;

  -----------------------------------------------------------------------------
  -- Componants declaration
  -----------------------------------------------------------------------------
  component plx_to_mem_interface
    generic(
      LALEFT  : integer := 2;
      LARIGHT : integer := 23
      );
    port (
      LClk        : in    std_logic;
      RstN        : in    std_logic;
      LAdSN       : in    std_logic;
      LA          : in    std_logic_vector(LARIGHT downto LALEFT);
      LData       : inout std_logic_vector(31 downto 0);
      LWrRdN      : in    std_logic;
      LReadyN     : out   std_logic;
      AddrMem     : out   IntAddrOutType;
      ReadMem     : out   std_logic;
      WriteMem    : out   std_logic;
      OpDone      : in    std_logic;
      DataFromMem : in    std_logic_vector(31 downto 0);
      DataToMem   : out   std_logic_vector(31 downto 0)
      );
  end component plx_to_mem_interface;

  component mil1553_core
    generic(
      g_HW_VERSION : std_logic_vector(15 downto 0) := X"0000"
      );
    port (
      pwr_reset_n_i     : in    std_logic;
      sys_clk_i         : in    std_logic;
      mil1553_rxd_a_i   : in    std_logic;
      mil1553_tx_rx_n_o : out   std_logic;
      mil1553_tx_n_o    : out   std_logic;
      mil1553_txd_o     : out   std_logic;
      mil1553_txd_n_o   : out   std_logic;
      led_o             : out   std_logic_vector(7 downto 0);
      test_point_o      : out   std_logic_vector (3 downto 0);
      onewire_b         : inout std_logic;
      rd_to_mem_i       : in    std_logic;
      wr_to_mem_i       : in    std_logic;
      data_from_mem_o   : out   IntDataType;
      addr_to_mem_i     : in    IntAddrOutType;
      data_to_mem_i     : in    IntDataType;
      op_done_o         : out   std_logic;
      irq_req_o         : out   std_logic_vector(1 downto 0)
      );
  end component mil1553_core;

  component mem_interface
    port (
      Clk         : in  std_logic;
      RstN        : in  std_logic;
      IntRead     : in  std_logic;
      IntWrite    : in  std_logic;
      DataFromInt : in  IntDataType;
      IntAdd      : in  IntAddrOutType;
      OpDone      : out std_logic;
      DataToInt   : out IntDataType;
      ContToMem   : out ContToMemType;
      MemToCont   : in  MemToContType(0 to NUMMEMPOSITION - 1)
      );
  end component mem_interface;

  component irq_regs
    generic (
      g_REG_WIDTH : integer := 32
      );
    port(
      clk_i          : in  std_logic;
      rst_n_i        : in  std_logic;
      irq_src_i      : in  std_logic_vector(g_REG_WIDTH - 1 downto 0);
      data_wr_i      : in  std_logic_vector(g_REG_WIDTH - 1 downto 0);
      irq_en_wren_i  : in  std_logic;
      irq_en_o       : out std_logic_vector(g_REG_WIDTH - 1 downto 0);
      irq_src_o      : out std_logic_vector(g_REG_WIDTH - 1 downto 0);
      irq_src_rden_i : in  std_logic;
      irq_req_o      : out std_logic_vector(1 downto 0)
      );
  end component irq_regs;

  component one_wire_ds1822
    generic (
      FREQ : integer := 40
      );
    port(
      Clk      : in    std_logic;
      RstN     : in    std_logic;
      SerialId : inout std_logic;
      Id       : out   std_logic_vector(63 downto 0);
      Temp     : out   std_logic_vector(15 downto 0);
      IdRead   : out   std_logic;
      Pps      : in    std_logic;
      IdOk     : out   std_logic
      );
  end component one_wire_ds1822;

  component mil1553_rx_clk
    port (
      sys_rst_n_i             : in  std_logic;  -- Synchronous system reset (active low)
      sys_clk_i               : in  std_logic;  -- System clock
      rxd_edge_p_i            : in  std_logic;  -- Indication of an edge on rxd
      rx_clk_rst_i            : in  std_logic;  -- resets the clock recovery procedure
      rx_manch_clk_p_o        : out std_logic;  -- signal with uclk-wide pulses
      rx_bit_clk_p_o          : out std_logic;  -- signal with uclk-wide pulses
      rx_signif_edge_window_o : out std_logic;  -- time window where a significant edge is expected
      rx_adjac_bits_window_o  : out std_logic   -- time window where a transition between adjacent
      );
  end component mil1553_rx_clk;

  component mil1553_rx_deglitcher
    port (
      sys_rst_n_i         : in  std_logic;  -- Synchronous system reset (active low)
      sys_clk_i           : in  std_logic;  -- System clock
      rxd_a_i             : in  std_logic;  -- Serial data input
      rxd_filt_o          : out std_logic;  -- filtered output signal
      rxd_filt_edge_p_o   : out std_logic;  -- indicates an edge on the filtered signal
      rxd_filt_f_edge_p_o : out std_logic;  -- indicates a falling edge on the filtered signal
      rxd_filt_r_edge_p_o : out std_logic   -- indicates a rising edge on the filtered signal
      );
  end component mil1553_rx_deglitcher;

  component mil1553_rx_deserialiser
    port (
      sys_rst_n_i          : in  std_logic;                     -- Synchronous system reset (active low)
      sys_clk_i            : in  std_logic;                     -- System clock
      rxd_i                : in  std_logic;                     -- Serial data input
      rxd_f_edge_p_i       : in  std_logic;                     -- Indicates a falling edge on serial input
      rxd_r_edge_p_i       : in  std_logic;                     -- Indicates a rising edge on serial input
      sample_bit_p_i       : in  std_logic;                     -- Pulse indicating the sampling of a bit
      sample_manch_bit_p_i : in  std_logic;                     -- Pulse indicating the sampling of a Manchester bit
      signif_edge_window_i : in  std_logic;                     -- Time window where a significant edge is expected
      adjac_bits_window_i  : in  std_logic;                     -- Time window where a transition between adjacent bits is expected
      rx_clk_rst_o         : out std_logic;                     -- Resets the clk recovery procedure
      rx_buffer_o          : out t_rx_buffer_array;             -- Receive buffer
      rx_word_cnt_o        : out std_logic_vector(4 downto 0);  -- Number of words in the receive buffer
      rx_in_progress_o     : out std_logic;                     -- Frame reception in progress
      rx_done_p_o          : out std_logic;                     -- End of frame reception
      rx_parity_error_p_o  : out std_logic;                     -- Parity error detected
      rx_manch_error_p_o   : out std_logic                      -- Manchester code violation detected
      );
  end component mil1553_rx_deserialiser;

  component mil1553_rx
    port (
      sys_rst_n_i         : in  std_logic;                     -- Synchronous system reset (active low)
      sys_clk_i           : in  std_logic;                     -- System clock
      mil1553_rxd_i       : in  std_logic;                     -- Serial data input
      mil1553_rx_en_i     : in  std_logic;                     -- Receiver enable
      rx_buffer_o         : out t_rx_buffer_array;             -- Receive buffer
      rx_word_cnt_o       : out std_logic_vector(4 downto 0);  -- Number of words in the receive buffer
      rx_in_progress_o    : out std_logic;                     -- Frame reception in progress
      rx_done_p_o         : out std_logic;                     -- End of frame reception
      rx_parity_error_p_o : out std_logic;                     -- Parity error detected
      rx_manch_error_p_o  : out std_logic                      -- Manchester code violation detected
      );
  end component mil1553_rx;

  component mil1553_tx_clk
    port (
      sys_rst_n_i   : in  std_logic;    -- Synchronous system reset (active low)
      sys_clk_i     : in  std_logic;    -- System clock
      tx_bit_rate_o : out std_logic     -- Bit rate for serialiser
      );
  end component mil1553_tx_clk;

  component mil1553_tx_serialiser
    port (
      sys_rst_n_i       : in  std_logic;          -- Synchronous system reset (active low)
      sys_clk_i         : in  std_logic;          -- System clock
      mil1553_txd_o     : out std_logic;          -- Serial data output
      mil1553_tx_en_o   : out std_logic;          -- Serial data output enable
      tx_bit_rate_p_i   : in  std_logic;          -- Bit rate pulse train, two pulses per bit
      tx_buffer_i       : in  t_tx_buffer_array;  -- Array of 16-bit word to transmit
      tx_send_frame_p_i : in  std_logic;          -- Send frame
      tx_done_p_o       : out std_logic           -- Frame transmission finished
      );
  end component mil1553_tx_serialiser;

  component mil1553_tx
    port (
      sys_rst_n_i       : in  std_logic;          -- Synchronous system reset (active low)
      sys_clk_i         : in  std_logic;          -- System clock
      mil1553_txd_o     : out std_logic;          -- Serial data output
      mil1553_tx_en_o   : out std_logic;          -- Serial data output enable
      tx_buffer_i       : in  t_tx_buffer_array;  -- Array of 16-bit word to transmit
      tx_send_frame_p_i : in  std_logic;          -- Send frame
      tx_done_p_o       : out std_logic           -- Frame transmission finished
      );
  end component mil1553_tx;

  component incr_cnt
    generic(
      g_COUNTER_WIDTH : natural := 4                                    -- default counter width
      );
    port(
      sys_clk_i         : in  std_logic;
      counter_incr_i    : in  std_logic;                                -- increment enable
      counter_reinit_i  : in  std_logic;                                -- reinitializes counter to 0
      counter_o         : out unsigned (g_COUNTER_WIDTH - 1 downto 0);  -- counter
      counter_is_full_o : out std_logic                                 -- counter full indication
      );
  end component incr_cnt;

  component decr_cnt
    generic(
      g_COUNTER_WIDTH : natural := 4                                           -- default counter width
      );
    port(
      sys_clk_i         : in  std_logic;
      sys_rst_n_i       : in  std_logic;                                       -- resets counter to all '1'
      counter_decr_i    : in  std_logic;                                       -- decrement enable
      counter_load_i    : in  std_logic;                                       -- load enable; loads counter to counter_top_i
      counter_top_i     : in  std_logic_vector(g_COUNTER_WIDTH - 1 downto 0);  -- load value
      counter_o         : out std_logic_vector(g_COUNTER_WIDTH - 1 downto 0);  -- counter
      counter_is_zero_o : out std_logic                                        -- empty counter indication
      );
  end component decr_cnt;

  component monostable
    generic(
      g_INPUT_POLARITY  : std_logic := '1';    --! trigger_i polarity
      g_OUTPUT_POLARITY : std_logic := '1';    --! pulse_o polarity
      g_OUTPUT_RETRIG   : boolean   := false;  --! Retriggerable output monostable
      g_OUTPUT_LENGTH   : natural   := 1       --! pulse_o lenght (in clk_i ticks)
      );
    port (
      rst_n_i   : in  std_logic;               --! Reset (active low)
      clk_i     : in  std_logic;               --! Clock
      trigger_i : in  std_logic;               --! Trigger input pulse
      pulse_o   : out std_logic                --! Monostable output pulse
      );
  end component monostable;


end cbmia_pkg;


package body cbmia_pkg is

  -----------------------------------------------------------------------------
  -- Returns log of 2 of a natural number
  -----------------------------------------------------------------------------
  function f_log2_ceil(N : natural) return positive is
  begin
    if N <= 2 then
      return 1;
    elsif N mod 2 = 0 then
      return 1 + f_log2_ceil(N/2);
    else
      return 1 + f_log2_ceil((N+1)/2);
    end if;
  end;

  -----------------------------------------------------------------------------
  -- Adds odd parity to the input 16-bit word
  -- Returns Manchester encoded word + parity
  -----------------------------------------------------------------------------
  function f_manch_encoder(word_i : std_logic_vector(15 downto 0))
    return std_logic_vector is
    variable v_word_parity : std_logic_vector(16 downto 0);
    variable v_word_o      : std_logic_vector(33 downto 0);
    variable v_parity      : std_logic;
  begin
    -- calculate parity
    v_parity := (not(word_i(0) xor word_i(1) xor word_i(2) xor word_i(3) xor
                     word_i(4) xor word_i(5) xor word_i(6) xor word_i(7) xor
                     word_i(8) xor word_i(9) xor word_i(10) xor word_i(11) xor
                     word_i(12) xor word_i(13) xor word_i(14) xor word_i(15)))
                xor c_ODD_PARITY;
    -- appends parity to word
    v_word_parity := word_i & v_parity;
    -- encode word + parity
    for I in 0 to 16 loop
      if v_word_parity(I) = '1' then
        v_word_o((2*I + 1) downto 2*I) := c_MANCH_ONE;
      else
        v_word_o((2*I + 1) downto 2*I) := c_MANCH_ZERO;
      end if;
    end loop;
    -- returns encoded word + parity
    return v_word_o;
  end;

  -----------------------------------------------------------------------------
  -- Parity of the input 16-bit word + parity
  -- Returns '1' is parity is OK, '0' otherwise
  -----------------------------------------------------------------------------
  function f_parity_check(word_i : std_logic_vector(16 downto 0))
    return std_logic is
    variable v_parity_ok : std_logic;
  begin
    -- check parity
    v_parity_ok := word_i(0) xor word_i(1) xor word_i(2) xor word_i(3) xor
                   word_i(4) xor word_i(5) xor word_i(6) xor word_i(7) xor
                   word_i(8) xor word_i(9) xor word_i(10) xor word_i(11) xor
                   word_i(12) xor word_i(13) xor word_i(14) xor word_i(15) xor
                   word_i(16);
    return v_parity_ok;
  end;

end cbmia_pkg;
