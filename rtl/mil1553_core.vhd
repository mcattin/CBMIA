-------------------------------------------------------------------------------
-- Title      : MIL1553 core
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mil1553_core.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-03-12
-- Last update: 2012-03-15
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
    led_o : out std_logic_vector(6 downto 0);

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

  ------------------------------------------------------------------------------
  -- Signals declaration
  ------------------------------------------------------------------------------
  signal sw_rst_p : std_logic;          -- Software reset, command from PCI
  signal rst_n    : std_logic;

  signal to_regs   : ContToMemType;     -- Data going from Control to the Registers
                                        -- This consists of Data + Enable + Read + Write
  signal from_regs : MemToContType(0 to NUMMEMPOSITION - 1);

  signal irq_src        : std_logic_vector(31 downto 0);
  signal irq_src_reg    : std_logic_vector(31 downto 0);
  signal irq_en_msk_reg : std_logic_vector(31 downto 0);

  signal tx_reg      : std_logic_vector(31 downto 0);
  signal tx_buffer   : t_tx_buffer_array;
  signal rx_reg      : std_logic_vector(31 downto 0);
  signal rx_rti      : std_logic_vector(4 downto 0);
  signal rx_buffer_t : t_rx_buffer_array;
  signal rx_buffer   : t_rx_buffer_array;
  signal cmd_reg     : std_logic_vector(31 downto 0);

  signal send_frame_req_p : std_logic;
  signal tx_send_frame_p  : std_logic;

  signal mil1553_tx_en      : std_logic;
  signal mil1553_rx_en      : std_logic;
  signal rx_word_cnt        : std_logic_vector(4 downto 0);
  signal rx_in_progress     : std_logic;
  signal rx_done_p          : std_logic;
  signal rx_parity_error_p  : std_logic;
  signal rx_manch_error_p   : std_logic;
  signal rx_nb_word_error_p : std_logic;

  signal resp_timeout_cnt    : unsigned(9 downto 0);
  signal resp_timeout_cnt_en : std_logic;
  signal resp_timeout_p      : std_logic;

  signal transaction_progress   : std_logic;
  signal transaction_progress_d : std_logic;
  signal transaction_end_p      : std_logic;

  signal sent_frame_cnt       : unsigned(31 downto 0);
  signal received_frame_cnt   : unsigned(31 downto 0);
  signal req_during_trans_cnt : unsigned(31 downto 0);

begin

  ------------------------------------------------------------------------------
  -- synchronous system reset (power-on and PCI command)
  ------------------------------------------------------------------------------
  rst_n <= pwr_reset_n_i and not(sw_rst_p);

  ------------------------------------------------------------------------------
  -- Memory interface
  ------------------------------------------------------------------------------
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

  ------------------------------------------------------------------------------
  -- Interrupt registers
  ------------------------------------------------------------------------------
  cmp_irq_regs : irq_regs
    generic map(
      g_REG_WIDTH <= 32
      )
    port map(
      clk_i          <= sys_clk_i,
      rst_n_i        <= pwr_reset_n_i,
      irq_src_i      <= irq_src,
      data_wr_i      <= to_regs.data,
      irq_en_wren_i  <= to_regs.wren(InterruptEnableP),
      irq_en_o       <= irq_en_msk_reg,
      irq_src_o      <= irq_src_reg,
      irq_src_rden_i <= to_regs.rden(InterruptSourceP),
      irq_req_o      <= irq_req_o
      );

  irq_src(31 downto 1) <= (others => '0');
  irq_src(0)           <= transaction_end_p;

  ------------------------------------------------------------------------------
  -- One-wire master for unique ID and temperature readout from DS1822
  ------------------------------------------------------------------------------
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

  ------------------------------------------------------------------------------
  -- Registers readback
  ------------------------------------------------------------------------------
  p_reg_rd : process(irq_en_msk_reg, irq_src_reg, cmd_reg, tx_reg, rx_reg,
                     rx_rti, rx_word_cnt, )
  begin

    l_rd_done : for I in from_regs'range loop
      from_regs(I).rddone <= '0';
    end loop;

    from_regs(MIDP).data             <= std_logic_vector(data_ready_cnt);  --id(63 downto 32);
    from_regs(LIDP).data             <= std_logic_vector(start_me_cnt);    --id(31 downto 0);
    from_regs(CommandP).data         <= cmd_reg;
    from_regs(RTIPRESENTP).data      <= DRTIReg;
    from_regs(CIRCADDP).data         <= std_logic_vector(write_during_tx_cnt) & std_logic_vector(write_req_during_tx_cnt);
    from_regs(FAULTADDLOW).data      <= x"12345678";                       -- in case of outside address range
    from_regs(FAULTADD).data         <= x"87654321";                       -- in case of outside address range
    from_regs(SOURCEREGP).data       <= StrBitNotBusy & StrBitBusy & '0' & DisablePoll & FrameType & M1553ActivRx & iM1553TX_RXN & RTIAddress & ErrorReg & HARDVERSION;
    from_regs(InterruptEnableP).data <= irq_en_msk_reg;
    from_regs(InterruptSourceP).data <= rx_rti & rx_word_cnt & irq_src_reg(21 downto 0);
    from_regs(TXREGP).data           <= tx_reg;
    from_regs(RXREGP).data           <= rx_reg;

    -- RX buffer read
    l_rx_buffer_rd : for I in 0 to 16 loop
      from_regs(W1 + I).data <= rx_buffer(I*2 + 1) & rx_buffer(I*2);
    end loop;
    -- TX buffer read
    l_tx_buffer_rd : for I in 0 to 15 loop
      from_regs(TXW1 + I).data <= tx_buffer(I*2+2) & tx_buffer(I*2+1);
    end loop;

  end process;

  ------------------------------------------------------------------------------
  -- TX register (TXREG)
  -- Contains the command word to be send to the RT
  ------------------------------------------------------------------------------
  p_tx_reg : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        tx_reg           <= (others => '0');
        send_frame_req_p <= '0';
      elsif to_regs.wren(TXREGP) then
        tx_reg           <= to_regs.data;
        send_frame_req_p <= '1';
      else
        send_frame_req_p <= '0';
      end if;
    end if;
  end process p_tx_reg;

  -- Copies command word to tx_buffer(0)
  tx_buffer(0) <= tx_reg(15 downto 0);

  -- Protection against transmission requests during a transaction
  tx_send_frame_p <= send_frame_req_p when transaction_progress = '1' else '0';

  ------------------------------------------------------------------------------
  -- TX buffer write
  -- Note: tx_buffer(0) corresponds to TX register (TXREG)
  ------------------------------------------------------------------------------
  l_tx_buffer_wr : for I in 0 to 15 generate
    process(sys_clk_i)
    begin
      if rising_edge(sys_clk_i) then
        if rst_n = '0' then
          tx_buffer(2*I+1) <= (others => '0');
          tx_buffer(2*I+2) <= (others => '0');
        elsif to_regs.wren(TXW1+I) = '1' then
          tx_buffer(2*I+1) <= to_regs.data(15 downto 0);
          tx_buffer(2*I+2) <= to_regs.data(31 downto 16);
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
        cmd_reg  <= (others => '0');
        sw_rst_p <= '0';
      elsif to_regs.wren(CommandP) = '1' then
        cmd_reg <= to_regs.data;
      else
        sw_rst_p               <= '0';
        cmd_reg(CommandRESETP) <= '0';
      end if;
    end if;
  end process p_cmd_reg;

  ------------------------------------------------------------------------------
  -- MIL1553 transmitter instantiation
  ------------------------------------------------------------------------------
  cmp_mil1553_tx : mil1553_tx
    port (
      sys_rst_n_i       => rst_n,
      sys_clk_i         => sys_clk_i,
      mil1553_txd_o     => mil1553_txd_o,
      mil1553_tx_en_o   => mil1553_tx_en,
      tx_buffer_i       => tx_buffer,
      tx_send_frame_p_i => tx_send_frame_p,
      tx_done_p_o       => tx_done_p
      );

  ------------------------------------------------------------------------------
  -- MIL1553 receiver enable generation
  ------------------------------------------------------------------------------
  mil1553_rx_en <= not(mil1553_tx_en);

  ------------------------------------------------------------------------------
  -- MIL1553 receiver instantiation
  ------------------------------------------------------------------------------
  cmp_mil1553_rx : mil1553_rx
    port map(
      sys_rst_n_i         => rst_n,
      sys_clk_i           => sys_clk_i,
      mil1553_rxd_i       => mil1553_rxd_a_i,
      mil1553_rx_en_i     => mil1553_rx_en,
      rx_buffer_o         => rx_buffer_t,
      rx_word_cnt_o       => rx_word_cnt,
      rx_in_progress_o    => rx_in_progress,
      rx_done_p_o         => rx_done_p,
      rx_parity_error_p_o => rx_parity_error_p,
      rx_manch_error_p_o  => rx_manch_error_p
      );

  ------------------------------------------------------------------------------
  -- RX buffer write
  ------------------------------------------------------------------------------
  p_rx_buffer_wr : process(sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        rx_buffer <= (others => (others => '0'));
      elsif rx_done_p = '1' then
        rx_buffer <= rx_buffer_t;
      end if;
    end if;
  end process p_rx_buffer_wr;

  ------------------------------------------------------------------------------
  -- RX register (RXREG)
  -- Contains the RT status word and the first received word, if any
  -- rx_buffer_t(0) = RT status
  ------------------------------------------------------------------------------
  rx_reg <= rx_buffer_t(1) & rx_buffer_t(0);

  ------------------------------------------------------------------------------
  -- Extract RT number from received status word
  ------------------------------------------------------------------------------
  rx_rti <= rx_reg(c_STA_RT4 downto c_STA_RT0);

  ------------------------------------------------------------------------------
  -- Check number of word(s) received against number of word(s) requested
  -- Only for reads from RTs
  ------------------------------------------------------------------------------
  p_check_nb_word : process(sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        rx_nb_word_error_p <= '0';
      elsif (rx_done_p = '1' and tx_reg(c_CMD_TR) = '1' and
             unsigned(rx_word_cnt) /= unsigned(tx_reg(c_CMD_WC4 downto c_CMD_WC0))) then
        rx_nb_word_error_p <= '1';
      else
        rx_nb_word_error_p <= '0';
      end if;
    end if;
  end process p_check_nb_word;

  ------------------------------------------------------------------------------
  -- Transaction in progess
  -- Starts when a command is written to the tx register (TXREG)
  -- Stops at the end of a frame reception OR if there is no reply from the RT,
  -- when the response timeout occurs
  ------------------------------------------------------------------------------
  p_transaction_progress : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        transaction_progress   <= '0';
        transaction_progress_d <= '0';
      else
        if start_message_p = '1' then
          transaction_progress <= '1';
        elsif resp_timeout_p = '1' or rx_done_p = '1' then
          transaction_progress <= '0';
        end if;
        transaction_progress_d <= transaction_progress;
      end if;
    end if;
  end process p_transaction_progress;

  transaction_end_p <= not(transaction_progress) and transaction_progress_d;

  ------------------------------------------------------------------------------
  -- RT response timout
  ------------------------------------------------------------------------------
  p_resp_timeout_cnt_en : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        resp_timeout_cnt_en <= '0';
      elsif tx_done_p = '1' then
        resp_timeout_cnt_en <= '1';
      elsif resp_timeout_cnt = 0 or rx_in_progress = '1' then
        resp_timeout_cnt_en <= '0';
      end if;
    end if;
  end process p_resp_timeout_cnt_en;

  p_resp_timeout_cnt : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        resp_timeout_cnt <= (others => '1');
      elsif tx_done_p = '1' or resp_timeout_cnt = 0 then
        resp_timeout_cnt <= (others => '1');
      elsif resp_timeout_cnt_en = '1' then
        resp_timeout_cnt <= resp_timeout_cnt - 1;
      end if;
    end if;
  end process p_resp_timeout_cnt;

  p_resp_timeout : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        resp_timeout_p <= '0';
      elsif resp_timeout_cnt = 0 then
        resp_timeout_p <= '1';
      else
        resp_timeout_p <= '0';
      end if;
    end if;
  end process p_resp_timeout;

  ------------------------------------------------------------------------------
  -- Sent frame counter
  ------------------------------------------------------------------------------
  p_sent_frame_cnt : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        sent_frame_cnt <= (others => '0');
      elsif tx_done_p = '1' then
        sent_frame_cnt <= sent_frame_cnt + 1;
      end if;
    end if;
  end process p_sent_frame_cnt;

  ------------------------------------------------------------------------------
  -- Received frame counter
  ------------------------------------------------------------------------------
  p_received_frame_cnt : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        received_frame_cnt <= (others => '0');
      elsif rx_done_p = '1' then
        received_frame_cnt <= received_frame_cnt + 1;
      end if;
    end if;
  end process p_received_frame_cnt;

  ------------------------------------------------------------------------------
  -- Send request while in transaction counter
  ------------------------------------------------------------------------------
  p_req_during_trans_cnt : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        req_during_trans_cnt <= (others => '0');
      elsif send_frame_req_p = '1' and transaction_progress = '1' then
        req_during_trans_cnt <= req_during_trans_cnt + 1;
      end if;
    end if;
  end process p_req_during_trans_cnt;

  ------------------------------------------------------------------------------
  -- LEDs
  -- Odd numbers  = orange LEDs
  -- Even numbers = green LEDs
  ------------------------------------------------------------------------------
  cmp_monostable_led0 : monostable
    generic map(
      g_INPUT_POLARITY  => '1',
      g_OUTPUT_POLARITY => '1',
      g_OUTPUT_RETRIG   => false,
      g_OUTPUT_LENGTH   => c_LED_MONOSTABLE_LENGTH
      )
    port map(
      rst_n_i   => rst_n,
      clk_i     => sys_clk_i,
      trigger_i => mil1553_tx_en,
      pulse_o   => led_o(0)
      );

  cmp_monostable_led1 : monostable
    generic map(
      g_INPUT_POLARITY  => '1',
      g_OUTPUT_POLARITY => '1',
      g_OUTPUT_RETRIG   => false,
      g_OUTPUT_LENGTH   => c_LED_MONOSTABLE_LENGTH
      )
    port map(
      rst_n_i   => rst_n,
      clk_i     => sys_clk_i,
      trigger_i => rx_parity_error_p,
      pulse_o   => led_o(1)
      );

  cmp_monostable_led2 : monostable
    generic map(
      g_INPUT_POLARITY  => '1',
      g_OUTPUT_POLARITY => '1',
      g_OUTPUT_RETRIG   => false,
      g_OUTPUT_LENGTH   => c_LED_MONOSTABLE_LENGTH
      )
    port map(
      rst_n_i   => rst_n,
      clk_i     => sys_clk_i,
      trigger_i => rx_in_progress,
      pulse_o   => led_o(2)
      );

  cmp_monostable_led3 : monostable
    generic map(
      g_INPUT_POLARITY  => '1',
      g_OUTPUT_POLARITY => '1',
      g_OUTPUT_RETRIG   => false,
      g_OUTPUT_LENGTH   => c_LED_MONOSTABLE_LENGTH
      )
    port map(
      rst_n_i   => rst_n,
      clk_i     => sys_clk_i,
      trigger_i => rx_manch_error_p,
      pulse_o   => led_o(3)
      );

  cmp_monostable_led4 : monostable
    generic map(
      g_INPUT_POLARITY  => '1',
      g_OUTPUT_POLARITY => '1',
      g_OUTPUT_RETRIG   => false,
      g_OUTPUT_LENGTH   => c_LED_MONOSTABLE_LENGTH
      )
    port map(
      rst_n_i   => rst_n,
      clk_i     => sys_clk_i,
      trigger_i => '0',
      pulse_o   => led_o(4)
      );

  cmp_monostable_led5 : monostable
    generic map(
      g_INPUT_POLARITY  => '1',
      g_OUTPUT_POLARITY => '1',
      g_OUTPUT_RETRIG   => false,
      g_OUTPUT_LENGTH   => c_LED_MONOSTABLE_LENGTH
      )
    port map(
      rst_n_i   => rst_n,
      clk_i     => sys_clk_i,
      trigger_i => rx_nb_word_error_p,
      pulse_o   => led_o(5)
      );

  cmp_monostable_led7 : monostable
    generic map(
      g_INPUT_POLARITY  => '1',
      g_OUTPUT_POLARITY => '1',
      g_OUTPUT_RETRIG   => false,
      g_OUTPUT_LENGTH   => c_LED_MONOSTABLE_LENGTH
      )
    port map(
      rst_n_i   => rst_n,
      clk_i     => sys_clk_i,
      trigger_i => '0',
      pulse_o   => led_o(6)
      );

  ------------------------------------------------------------------------------
  -- Test points
  ------------------------------------------------------------------------------
  test_point_o <= (others => '0');


end architecture rtl;
