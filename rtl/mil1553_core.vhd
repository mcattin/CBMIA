-------------------------------------------------------------------------------
-- Title      : MIL1553 core
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mil1553_core.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-03-12
-- Last update: 2012-04-16
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: MIL1553 bus controller core. In this design, the
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
use work.mem_interface_pkg.all;


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
    rd_to_mem_i     : in  std_logic;    -- Read strobe to memory interface
    wr_to_mem_i     : in  std_logic;    -- Write strobe to memory interface
    data_from_mem_o : out t_int_data;   -- Data from memory interface
    addr_to_mem_i   : in  t_int_addr;   -- Address to memory interface
    data_to_mem_i   : in  t_int_data;   -- Data to memory interface
    op_done_o       : out std_logic;    -- Operation done from memory interface

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

  signal to_regs   : t_cont_to_mem;     -- Data going from Control to the Registers
                                        -- This consists of Data + Enable + Read + Write
  signal from_regs : t_mem_to_cont(0 to c_NB_MEM_POS - 1);

  signal irq_src        : std_logic_vector(31 downto 0);
  signal irq_src_reg    : std_logic_vector(31 downto 0);
  signal irq_en_msk_reg : std_logic_vector(31 downto 0);

  signal pps_cnt   : unsigned(25 downto 0);
  signal pps_p     : std_logic;
  signal temp      : std_logic_vector(15 downto 0);
  signal unique_id : std_logic_vector(63 downto 0);

  signal tx_reg      : std_logic_vector(31 downto 0);
  signal tx_buffer   : t_tx_buffer_array;
  signal rx_reg      : std_logic_vector(31 downto 0);
  signal rx_rti      : std_logic_vector(4 downto 0);
  signal rx_buffer_t : t_rx_buffer_array;
  signal rx_buffer   : t_rx_buffer_array;
  signal cmd_reg     : std_logic_vector(31 downto 0);

  signal send_frame_req_p : std_logic;
  signal tx_send_frame_p  : std_logic;

  signal mil1553_txd           : std_logic;
  signal mil1553_tx_en         : std_logic;
  signal mil1553_rx_en         : std_logic;
  signal rx_word_cnt           : std_logic_vector(5 downto 0);
  signal rx_word_cnt_t         : std_logic_vector(5 downto 0);
  signal rx_in_progress        : std_logic;
  signal rx_done_p             : std_logic;
  signal rx_parity_error_p     : std_logic;
  signal rx_parity_error_flag  : std_logic;
  signal rx_parity_error_cnt   : unsigned(31 downto 0);
  signal rx_manch_error_p      : std_logic;
  signal rx_manch_error_flag   : std_logic;
  signal rx_manch_error_cnt    : unsigned(31 downto 0);
  signal rx_nb_word_error_p    : std_logic;
  signal rx_nb_word_error_flag : std_logic;
  signal tx_word_cnt           : std_logic_vector(5 downto 0);
  signal tx_tr_flag            : std_logic;
  signal rx_nb_word_error_cnt  : unsigned(31 downto 0);
  signal tx_done_p             : std_logic;
  signal expected_nb_word      : std_logic_vector(5 downto 0);  -- expected nb of word from RT (including the status word)
  signal rx_error_p            : std_logic;
  signal rx_error_cnt          : unsigned(31 downto 0);
  signal rx_error_flag         : std_logic;

  signal resp_time_cnt     : unsigned(9 downto 0);
  signal resp_time_cnt_en  : std_logic;
  signal resp_timeout_p    : std_logic;
  signal resp_timeout_flag : std_logic;
  signal resp_timeout_cnt  : unsigned(31 downto 0);

  signal transaction_progress   : std_logic;
  signal transaction_progress_d : std_logic;
  signal transaction_end_p      : std_logic;

  signal sent_frame_cnt       : unsigned(31 downto 0);
  signal received_frame_cnt   : unsigned(31 downto 0);
  signal req_during_trans_cnt : unsigned(31 downto 0);
  signal req_during_trans_p   : std_logic;

  signal irq_req : std_logic_vector(1 downto 0);


begin

  ------------------------------------------------------------------------------
  -- Synchronous system reset (power-on and PCI command)
  ------------------------------------------------------------------------------
  rst_n <= pwr_reset_n_i and not(sw_rst_p);

  ------------------------------------------------------------------------------
  -- Memory interface (address decoder, data mux)
  ------------------------------------------------------------------------------
  cmp_mem_interface : mem_interface
    port map(
      Clk         => sys_clk_i,
      RstN        => pwr_reset_n_i,
      IntRead     => rd_to_mem_i,
      IntWrite    => wr_to_mem_i,
      DataFromInt => data_to_mem_i,
      IntAdd      => addr_to_mem_i,
      OpDone      => op_done_o,
      DataToInt   => data_from_mem_o,
      ContToMem   => to_regs,
      MemToCont   => from_regs
      );

  ------------------------------------------------------------------------------
  -- Interrupt registers
  ------------------------------------------------------------------------------
  cmp_irq_regs : irq_regs
    generic map(
      g_REG_WIDTH => 32
      )
    port map(
      clk_i          => sys_clk_i,
      rst_n_i        => pwr_reset_n_i,
      irq_src_i      => irq_src,
      data_wr_i      => to_regs.data,
      irq_en_wren_i  => to_regs.wren(c_IRQ_EN_POS),
      irq_en_o       => irq_en_msk_reg,
      irq_src_o      => irq_src_reg,
      irq_src_rden_i => to_regs.rden(c_IRQ_SRC_POS),
      irq_req_o      => irq_req
      );

  irq_src(31 downto 1) <= (others => '0');
  irq_src(0)           <= transaction_end_p;
  irq_req_o            <= irq_req;

  ------------------------------------------------------------------------------
  -- PPS counter for temperature readout
  ------------------------------------------------------------------------------
  p_pps_cnt : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        pps_cnt <= (others => '0');
        pps_p   <= '0';
      elsif pps_cnt = 0 then
        pps_cnt <= to_unsigned(39999999, pps_cnt'length);
        pps_p   <= '1';
      else
        pps_cnt <= pps_cnt - 1;
        pps_p   <= '0';
      end if;
    end if;
  end process p_pps_cnt;

  ------------------------------------------------------------------------------
  -- One-wire master for unique ID and temperature readout from DS1822
  ------------------------------------------------------------------------------
  cmp_one_wire_ds1822 : one_wire_ds1822
    generic map(
      FREQ => 40                        -- in MHz
      )
    port map(
      Clk      => sys_clk_i,
      RstN     => rst_n,
      SerialId => onewire_b,
      Id       => unique_id,            -- 64-bit
      Temp     => temp,                 -- 16-bit
      IdRead   => open,
      Pps      => pps_p,                -- pulse per second for temp read
      IdOk     => open
      );

  ------------------------------------------------------------------------------
  -- Registers readback
  ------------------------------------------------------------------------------
  p_reg_rd : process(irq_en_msk_reg, irq_src_reg, cmd_reg, tx_reg, rx_reg,
                     rx_rti, tx_word_cnt, rx_word_cnt, transaction_progress,
                     sent_frame_cnt, received_frame_cnt, rx_nb_word_error_cnt,
                     req_during_trans_cnt, resp_time_cnt, resp_timeout_cnt,
                     rx_buffer, tx_buffer, temp, tx_tr_flag, unique_id,
                     rx_parity_error_cnt, rx_manch_error_cnt,
                     rx_parity_error_flag, rx_manch_error_flag,
                     rx_nb_word_error_flag, resp_timeout_flag,
                     rx_error_flag, rx_error_cnt, expected_nb_word)
  begin

    l_rd_done : for I in from_regs'range loop
      from_regs(I).rddone <= '0';
    end loop;

    -- Interrupts
    from_regs(c_IRQ_EN_POS).data  <= irq_en_msk_reg;
    from_regs(c_IRQ_SRC_POS).data <= rx_rti & rx_word_cnt & tx_tr_flag
                                     & rx_parity_error_flag & rx_manch_error_flag
                                     & rx_nb_word_error_flag & resp_timeout_flag
                                     & rx_error_flag
                                     & irq_src_reg(14 downto 0);

    -- Command and status
    from_regs(c_CMD_POS).data  <= cmd_reg;
    from_regs(c_STAT_POS).data <= transaction_progress & "000" & X"000" & g_HW_VERSION;

    -- TX and RX registers
    from_regs(c_TX_REG_POS).data <= tx_reg;
    from_regs(c_RX_REG_POS).data <= rx_reg;

    -- Unique ID
    from_regs(c_ID_MSB_POS).data <= unique_id(63 downto 32);
    from_regs(c_ID_LSB_POS).data <= unique_id(31 downto 0);

    -- Temperature
    from_regs(c_TEMP_POS).data <= X"0000" & temp;

    -- Statistics and debug
    from_regs(c_TX_FRAME_CNT_POS).data    <= std_logic_vector(sent_frame_cnt);
    from_regs(c_RX_FRAME_CNT_POS).data    <= std_logic_vector(received_frame_cnt);
    from_regs(c_PARITY_ERR_CNT_POS).data  <= std_logic_vector(rx_parity_error_cnt);
    from_regs(c_MANCH_ERR_CNT_POS).data   <= std_logic_vector(rx_manch_error_cnt);
    from_regs(c_NB_WORD_ERR_CNT_POS).data <= std_logic_vector(rx_nb_word_error_cnt);
    from_regs(c_TX_ERR_CNT_POS).data      <= std_logic_vector(req_during_trans_cnt);
    from_regs(c_NB_WORD_POS).data         <= tx_tr_flag & rx_parity_error_flag & rx_manch_error_flag
                                             & rx_nb_word_error_flag & resp_timeout_flag & rx_error_flag
                                             & X"00" & expected_nb_word & rx_word_cnt & tx_word_cnt;
    from_regs(c_RESP_TIME_POS).data        <= X"0000" & "000000" & std_logic_vector(resp_time_cnt);
    from_regs(c_RX_ERROR_CNT_POS).data     <= std_logic_vector(rx_error_cnt);
    from_regs(c_RESP_TIMEOUT_CNT_POS).data <= std_logic_vector(resp_timeout_cnt);

    -- Reserved for future use
    from_regs(c_RFU_POS).data <= X"00000000";

    -- Outside of range access
    from_regs(c_FAULT_ADDR_LOW_POS).data <= x"12345678";
    from_regs(c_FAULT_ADDR_POS).data     <= x"87654321";

    -- RX buffer read
    l_rx_buffer_rd : for I in 0 to 16 loop
      if I = 16 then
        from_regs(c_RX_BUF0_POS + I).data <= X"0000" & rx_buffer(I*2);
      else
        from_regs(c_RX_BUF0_POS + I).data <= rx_buffer(I*2 + 1) & rx_buffer(I*2);
      end if;
    end loop;
    -- TX buffer read
    l_tx_buffer_rd : for I in 0 to 15 loop
      from_regs(c_TX_BUF0_POS + I).data <= tx_buffer(I*2+2) & tx_buffer(I*2+1);
    end loop;

  end process;

  ------------------------------------------------------------------------------
  -- TX register (TX_REG)
  -- Contains the command word to be send to the RT
  -- A transaction is started every time this register is written and no
  -- transaction is in progress.
  ------------------------------------------------------------------------------
  p_tx_reg : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        tx_reg           <= (others => '0');
        send_frame_req_p <= '0';
      elsif to_regs.wren(c_TX_REG_POS) = '1' then
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
  tx_send_frame_p <= send_frame_req_p when transaction_progress = '0' else '0';

  ------------------------------------------------------------------------------
  -- Stores the number of word to send/receive, the expected number of word to
  -- reveive and the TR flag.
  ------------------------------------------------------------------------------
  p_tx_word_cnt : process(sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        expected_nb_word <= (others => '0');
        tx_word_cnt      <= (others => '0');
        tx_tr_flag       <= '0';
      elsif tx_send_frame_p = '1' then
        tx_tr_flag <= tx_reg(c_CMD_TR);
        if (tx_reg(c_CMD_SA4 downto c_CMD_SA0) = "00000" or
            tx_reg(c_CMD_SA4 downto c_CMD_SA0) = "11111") then  -- WC field countains a Mode Code
          tx_word_cnt      <= (others => '0');
          expected_nb_word <= std_logic_vector(to_unsigned(1, expected_nb_word'length));
        else                                                    -- WC field contains the number of word to send/receive
          if tx_reg(c_CMD_WC4 downto c_CMD_WC0) = "00000" then
            if tx_reg(c_CMD_TR) = c_BC2RT then
              tx_word_cnt      <= std_logic_vector(to_unsigned(32, tx_word_cnt'length));
              expected_nb_word <= std_logic_vector(to_unsigned(1, expected_nb_word'length));
            else                                                -- c_RT2BC
              tx_word_cnt      <= (others => '0');
              expected_nb_word <= std_logic_vector(to_unsigned(33, expected_nb_word'length));
            end if;
          else
            if tx_reg(c_CMD_TR) = c_BC2RT then
              tx_word_cnt      <= '0' & tx_reg(c_CMD_WC4 downto c_CMD_WC0);
              expected_nb_word <= std_logic_vector(to_unsigned(1, expected_nb_word'length));
            else                                                -- c_RT2BC
              tx_word_cnt      <= (others => '0');
              expected_nb_word <= std_logic_vector(unsigned('0' & tx_reg(c_CMD_WC4 downto c_CMD_WC0)) + 1);
            end if;
          end if;
        end if;
      end if;
    end if;
  end process p_tx_word_cnt;

  ------------------------------------------------------------------------------
  -- TX buffer write
  -- Note: tx_buffer(0) corresponds to TX register (TX_REG)
  ------------------------------------------------------------------------------
  l_tx_buffer_wr : for I in 0 to 15 generate
    process(sys_clk_i)
    begin
      if rising_edge(sys_clk_i) then
        if rst_n = '0' then
          tx_buffer(2*I+1) <= (others => '0');
          tx_buffer(2*I+2) <= (others => '0');
        elsif to_regs.wren(c_TX_BUF0_POS+I) = '1' then
          tx_buffer(2*I+1) <= to_regs.data(15 downto 0);
          tx_buffer(2*I+2) <= to_regs.data(31 downto 16);
        end if;
      end if;
    end process;
  end generate;

  ------------------------------------------------------------------------------
  -- Command register
  -- Software reset generation
  ------------------------------------------------------------------------------
  p_cmd_reg : process(sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        cmd_reg <= (others => '0');
      elsif to_regs.wren(c_CMD_POS) = '1' then
        cmd_reg <= to_regs.data;
      else
        cmd_reg(c_CMD_RST_BIT) <= '0';
      end if;
    end if;
  end process p_cmd_reg;

  sw_rst_p <= cmd_reg(c_CMD_RST_BIT);

  ------------------------------------------------------------------------------
  -- MIL1553 transmitter instantiation
  ------------------------------------------------------------------------------
  cmp_mil1553_tx : mil1553_tx
    port map(
      sys_rst_n_i       => rst_n,
      sys_clk_i         => sys_clk_i,
      mil1553_txd_o     => mil1553_txd,
      mil1553_tx_en_o   => mil1553_tx_en,
      tx_buffer_i       => tx_buffer,
      tx_send_frame_p_i => tx_send_frame_p,
      tx_done_p_o       => tx_done_p
      );

  ------------------------------------------------------------------------------
  -- MIL1553 interface outputs
  ------------------------------------------------------------------------------
  -- MIL1553 serial data output
  mil1553_txd_o     <= mil1553_txd;
  mil1553_txd_n_o   <= not(mil1553_txd);    -- for compatibility with CBMIA PCB
  -- MIL1553 TX/RX buffer enable
  mil1553_tx_rx_n_o <= mil1553_tx_en;
  mil1553_tx_n_o    <= not(mil1553_tx_en);  -- for compatibility with CBMIA PCB

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
      expected_nb_word_i  => expected_nb_word,
      rx_buffer_o         => rx_buffer_t,
      rx_word_cnt_o       => rx_word_cnt_t,
      rx_in_progress_o    => rx_in_progress,
      rx_done_p_o         => rx_done_p,
      rx_parity_error_p_o => rx_parity_error_p,
      rx_manch_error_p_o  => rx_manch_error_p,
      rx_error_p_o        => rx_error_p
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
  -- RX register (RX_REG)
  -- Contains the RT status word and the first received word, if any.
  -- rx_buffer_t(0) = RT status
  ------------------------------------------------------------------------------
  p_rx_reg_wr : process(sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        rx_reg <= (others => '0');
      elsif tx_send_frame_p = '1' then
        rx_reg <= (others => '0');
      elsif rx_done_p = '1' then
        rx_reg <= rx_buffer_t(1) & rx_buffer_t(0);
      end if;
    end if;
  end process p_rx_reg_wr;

  ------------------------------------------------------------------------------
  -- Extract RT number from received status word
  -- rx_rti will be 0 if there is no response from the RT (timeout)
  ------------------------------------------------------------------------------
  rx_rti <= rx_reg(c_STA_RT4 downto c_STA_RT0);

  ------------------------------------------------------------------------------
  -- Number of words received stored for IRQ register
  ------------------------------------------------------------------------------
  p_rx_word_cnt : process(sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        rx_word_cnt <= (others => '0');
      elsif tx_send_frame_p = '1' then
        rx_word_cnt <= (others => '0');
      elsif rx_done_p = '1' then
        rx_word_cnt <= rx_word_cnt_t;
      end if;
    end if;
  end process p_rx_word_cnt;

  ------------------------------------------------------------------------------
  -- Transaction in progess flag
  -- Starts when a command word is written to the tx register (TX_REG)
  -- Stops at the end of a frame reception OR in case of Manchester error OR
  -- if there is no reply from the RT, when a response timeout occurs.
  ------------------------------------------------------------------------------
  p_transaction_progress : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        transaction_progress   <= '0';
        transaction_progress_d <= '0';
      else
        if tx_send_frame_p = '1' then
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
  -- RT response timeout
  ------------------------------------------------------------------------------
  p_resp_time_cnt_en : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        resp_time_cnt_en <= '0';
      elsif tx_done_p = '1' then
        resp_time_cnt_en <= '1';
      elsif resp_time_cnt = 0 or rx_in_progress = '1' then
        resp_time_cnt_en <= '0';
      end if;
    end if;
  end process p_resp_time_cnt_en;

  p_resp_time_cnt : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        resp_time_cnt <= (others => '1');
      elsif tx_done_p = '1' or resp_time_cnt = 0 then
        resp_time_cnt <= (others => '1');
      elsif resp_time_cnt_en = '1' then
        resp_time_cnt <= resp_time_cnt - 1;
      end if;
    end if;
  end process p_resp_time_cnt;

  p_resp_timeout : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        resp_timeout_p <= '0';
      elsif resp_time_cnt = 0 then
        resp_timeout_p <= '1';
      else
        resp_timeout_p <= '0';
      end if;
    end if;
  end process p_resp_timeout;

  p_resp_timeout_flag : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        resp_timeout_flag <= '0';
      elsif tx_send_frame_p = '1' then
        resp_timeout_flag <= '0';
      elsif resp_timeout_p = '1' then
        resp_timeout_flag <= '1';
      end if;
    end if;
  end process p_resp_timeout_flag;

  p_resp_timeout_cnt : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        resp_timeout_cnt <= (others => '0');
      elsif resp_timeout_p = '1' then
        resp_timeout_cnt <= resp_timeout_cnt + 1;
      end if;
    end if;
  end process p_resp_timeout_cnt;

  ------------------------------------------------------------------------------
  -- Transmitted frame counter
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
  -- Check number of word(s) received against number of word(s) requested.
  -- Only for reads from RTs and when the mode code flag is cleared.
  ------------------------------------------------------------------------------
  p_check_nb_word : process(sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        rx_nb_word_error_p <= '0';
      elsif (rx_done_p = '1' and unsigned(rx_word_cnt_t) /= unsigned(expected_nb_word)) then
        rx_nb_word_error_p <= '1';
      else
        rx_nb_word_error_p <= '0';
      end if;
    end if;
  end process p_check_nb_word;

  ------------------------------------------------------------------------------
  -- Number of received word error counter
  ------------------------------------------------------------------------------
  p_nb_word_error_cnt : process(sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        rx_nb_word_error_cnt <= (others => '0');
      elsif rx_nb_word_error_p = '1' then
        rx_nb_word_error_cnt <= rx_nb_word_error_cnt + 1;
      end if;
    end if;
  end process p_nb_word_error_cnt;

  ------------------------------------------------------------------------------
  -- Number of received word error flag,
  -- different number of word than expected in the last received frame.
  ------------------------------------------------------------------------------
  p_nb_word_error_flag : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        rx_nb_word_error_flag <= '0';
      elsif tx_send_frame_p = '1' then
        rx_nb_word_error_flag <= '0';
      elsif rx_nb_word_error_p = '1' then
        rx_nb_word_error_flag <= '1';
      end if;
    end if;
  end process p_nb_word_error_flag;

  ------------------------------------------------------------------------------
  -- Transmit request while in transaction counter
  ------------------------------------------------------------------------------
  p_req_during_trans_cnt : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        req_during_trans_cnt <= (others => '0');
      elsif req_during_trans_p = '1' then
        req_during_trans_cnt <= req_during_trans_cnt + 1;
      end if;
    end if;
  end process p_req_during_trans_cnt;

  req_during_trans_p <= send_frame_req_p and transaction_progress;

  ------------------------------------------------------------------------------
  -- Parity errors counter
  ------------------------------------------------------------------------------
  p_parity_error_cnt : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        rx_parity_error_cnt <= (others => '0');
      elsif rx_parity_error_p = '1' then
        rx_parity_error_cnt <= rx_parity_error_cnt + 1;
      end if;
    end if;
  end process p_parity_error_cnt;

  ------------------------------------------------------------------------------
  -- Parity error flag, indicates a parity error in the last reveived frame
  ------------------------------------------------------------------------------
  p_parity_error_flag : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        rx_parity_error_flag <= '0';
      elsif tx_send_frame_p = '1' then
        rx_parity_error_flag <= '0';
      elsif rx_parity_error_p = '1' then
        rx_parity_error_flag <= '1';
      end if;
    end if;
  end process p_parity_error_flag;

  ------------------------------------------------------------------------------
  -- Manchester code errors counter
  ------------------------------------------------------------------------------
  p_manch_error_cnt : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        rx_manch_error_cnt <= (others => '0');
      elsif rx_manch_error_p = '1' then
        rx_manch_error_cnt <= rx_manch_error_cnt + 1;
      end if;
    end if;
  end process p_manch_error_cnt;

  ------------------------------------------------------------------------------
  -- Manchester error flag, indicates a Manchester error in the last reveived frame
  ------------------------------------------------------------------------------
  p_manch_error_flag : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        rx_manch_error_flag <= '0';
      elsif tx_send_frame_p = '1' then
        rx_manch_error_flag <= '0';
      elsif rx_manch_error_p = '1' then
        rx_manch_error_flag <= '1';
      end if;
    end if;
  end process p_manch_error_flag;

  ------------------------------------------------------------------------------
  -- Reception errors counter
  ------------------------------------------------------------------------------
  p_rx_error_cnt : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        rx_error_cnt <= (others => '0');
      elsif rx_error_p = '1' then
        rx_error_cnt <= rx_error_cnt + 1;
      end if;
    end if;
  end process p_rx_error_cnt;

  ------------------------------------------------------------------------------
  -- Reception error flag, indicates an error in the last reveived frame
  ------------------------------------------------------------------------------
  p_rx_error_flag : process (sys_clk_i)
  begin
    if rising_edge(sys_clk_i) then
      if rst_n = '0' then
        rx_error_flag <= '0';
      elsif tx_send_frame_p = '1' then
        rx_error_flag <= '0';
      elsif rx_error_p = '1' then
        rx_error_flag <= '1';
      end if;
    end if;
  end process p_rx_error_flag;

  ------------------------------------------------------------------------------
  -- LEDs
  -- Odd numbers  = orange LEDs
  -- Even numbers = green LEDs
  --
  -- LED 0 = Transmitting
  -- LED 2 = Receiving
  -- LED 4 = Response timeout
  -- LED 6 = PCI access (in cbmia_top.vhd)
  -- LED 1 = Parity error
  -- LED 3 = Manchester error
  -- LED 5 = Number of word error
  -- LED 7 = Transmission request during transaction
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
      trigger_i => tx_done_p,
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
      trigger_i => rx_done_p,
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
      trigger_i => resp_timeout_p,
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
      trigger_i => req_during_trans_p,
      pulse_o   => led_o(6)
      );

  ------------------------------------------------------------------------------
  -- Test points mux
  -- The signal to output on a test point is defined in the command register.
  ------------------------------------------------------------------------------
  p_tp0_mux : process (cmd_reg)
  begin
    case cmd_reg(19 downto 16) is
      when "0000" => test_point_o(0) <= transaction_progress;
      when "0001" => test_point_o(0) <= mil1553_tx_en;
      when "0010" => test_point_o(0) <= rx_in_progress;
      when "0011" => test_point_o(0) <= mil1553_rxd_a_i;
      when "0100" => test_point_o(0) <= tx_done_p;
      when "0101" => test_point_o(0) <= rx_done_p;
      when "0110" => test_point_o(0) <= rx_manch_error_p;
      when "0111" => test_point_o(0) <= rx_parity_error_p;
      when "1000" => test_point_o(0) <= rx_nb_word_error_p;
      when "1001" => test_point_o(0) <= resp_timeout_p;
      when "1010" => test_point_o(0) <= req_during_trans_p;
      when "1011" => test_point_o(0) <= tx_send_frame_p;
      when "1100" => test_point_o(0) <= send_frame_req_p;
      when "1101" => test_point_o(0) <= mil1553_txd;
      when "1110" => test_point_o(0) <= irq_req(0);
      when "1111" => test_point_o(0) <= rx_error_p;
      when others => test_point_o(0) <= transaction_progress;
    end case;
  end process p_tp0_mux;

  p_tp1_mux : process (cmd_reg)
  begin
    case cmd_reg(23 downto 20) is
      when "0000" => test_point_o(1) <= transaction_progress;
      when "0001" => test_point_o(1) <= mil1553_tx_en;
      when "0010" => test_point_o(1) <= rx_in_progress;
      when "0011" => test_point_o(1) <= mil1553_rxd_a_i;
      when "0100" => test_point_o(1) <= tx_done_p;
      when "0101" => test_point_o(1) <= rx_done_p;
      when "0110" => test_point_o(1) <= rx_manch_error_p;
      when "0111" => test_point_o(1) <= rx_parity_error_p;
      when "1000" => test_point_o(1) <= rx_nb_word_error_p;
      when "1001" => test_point_o(1) <= resp_timeout_p;
      when "1010" => test_point_o(1) <= req_during_trans_p;
      when "1011" => test_point_o(1) <= tx_send_frame_p;
      when "1100" => test_point_o(1) <= send_frame_req_p;
      when "1101" => test_point_o(1) <= mil1553_txd;
      when "1110" => test_point_o(1) <= irq_req(0);
      when "1111" => test_point_o(1) <= rx_error_p;
      when others => test_point_o(1) <= mil1553_tx_en;
    end case;
  end process p_tp1_mux;

  p_tp2_mux : process (cmd_reg)
  begin
    case cmd_reg(27 downto 24) is
      when "0000" => test_point_o(2) <= transaction_progress;
      when "0001" => test_point_o(2) <= mil1553_tx_en;
      when "0010" => test_point_o(2) <= rx_in_progress;
      when "0011" => test_point_o(2) <= mil1553_rxd_a_i;
      when "0100" => test_point_o(2) <= tx_done_p;
      when "0101" => test_point_o(2) <= rx_done_p;
      when "0110" => test_point_o(2) <= rx_manch_error_p;
      when "0111" => test_point_o(2) <= rx_parity_error_p;
      when "1000" => test_point_o(2) <= rx_nb_word_error_p;
      when "1001" => test_point_o(2) <= resp_timeout_p;
      when "1010" => test_point_o(2) <= req_during_trans_p;
      when "1011" => test_point_o(2) <= tx_send_frame_p;
      when "1100" => test_point_o(2) <= send_frame_req_p;
      when "1101" => test_point_o(2) <= mil1553_txd;
      when "1110" => test_point_o(2) <= irq_req(0);
      when "1111" => test_point_o(2) <= rx_error_p;
      when others => test_point_o(2) <= rx_in_progress;
    end case;
  end process p_tp2_mux;

  p_tp3_mux : process (cmd_reg)
  begin
    case cmd_reg(31 downto 28) is
      when "0000" => test_point_o(3) <= transaction_progress;
      when "0001" => test_point_o(3) <= mil1553_tx_en;
      when "0010" => test_point_o(3) <= rx_in_progress;
      when "0011" => test_point_o(3) <= mil1553_rxd_a_i;
      when "0100" => test_point_o(3) <= tx_done_p;
      when "0101" => test_point_o(3) <= rx_done_p;
      when "0110" => test_point_o(3) <= rx_manch_error_p;
      when "0111" => test_point_o(3) <= rx_parity_error_p;
      when "1000" => test_point_o(3) <= rx_nb_word_error_p;
      when "1001" => test_point_o(3) <= resp_timeout_p;
      when "1010" => test_point_o(3) <= req_during_trans_p;
      when "1011" => test_point_o(3) <= tx_send_frame_p;
      when "1100" => test_point_o(3) <= send_frame_req_p;
      when "1101" => test_point_o(3) <= mil1553_txd;
      when "1110" => test_point_o(3) <= irq_req(0);
      when "1111" => test_point_o(3) <= rx_error_p;
      when others => test_point_o(3) <= mil1553_rxd_a_i;
    end case;
  end process p_tp3_mux;


end architecture rtl;
