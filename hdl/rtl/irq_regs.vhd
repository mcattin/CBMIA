-------------------------------------------------------------------------------
-- Title      : Interrupt registers
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : irq_regs.vhd
-- Author     : Pablo Antonio Alvarez Sanchez
-- Company    : CERN (BE-CO-HT)
-- Created    : 2004-09-27
-- Last update: 2012-03-09
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: This entity contains a clear on read register that latches
--              any enabled irq request and activates the IRQ line.
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
-- 2004-09-27  1.0      paas            Created
-- 2012-03-09  1.1      mcattin         Clean-up, add license
-------------------------------------------------------------------------------
-- TODO: -
--       -
-------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.NUMERIC_STD.all;


entity irq_regs is

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

end irq_regs;


architecture rtl of irq_regs is

  ------------------------------------------------------------------------------
  -- Signals declaration
  ------------------------------------------------------------------------------
  signal irq_src_or  : std_logic;
  signal irq_src_reg : std_logic_vector(g_REG_WIDTH - 1 downto 0);
  signal irq_req_reg : std_logic;
  signal irq_en_reg  : std_logic_vector(g_REG_WIDTH - 1 downto 0);

begin

  ------------------------------------------------------------------------------
  -- ORing interrupt requests
  ------------------------------------------------------------------------------
  process(irq_src_i, irq_en_reg)
    variable vIrqSourceOr : std_logic;
  begin
    vIrqSourceOr := '0';

    for I in 0 to g_REG_WIDTH - 1 loop
      if irq_src_i(I) = '1' and irq_en_reg(I) = '1' then
        vIrqSourceOr := '1';
        exit;
      end if;
    end loop;
    irq_src_or <= vIrqSourceOr;
  end process;

  ------------------------------------------------------------------------------
  -- Interrupt source register
  ------------------------------------------------------------------------------
  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        irq_src_reg <= (others => '0');
      else
        if irq_src_rden_i = '1' then
          irq_src_reg <= irq_src_i and irq_en_reg;
        else
          irq_src_reg <= (irq_src_i or irq_src_reg) and irq_en_reg;
        end if;
      end if;
    end if;
  end process;

  irq_src_o <= irq_src_reg;

  ------------------------------------------------------------------------------
  -- Interrupt request to host
  ------------------------------------------------------------------------------
  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        irq_req_reg <= '0';
      else
        if irq_src_rden_i = '1' then
          irq_req_reg <= irq_src_or;
        elsif irq_src_or = '1' then
          irq_req_reg <= '1';
        end if;
      end if;

    end if;
  end process;

  irq_req_o(0) <= irq_req_reg;
  irq_req_o(1) <= '0';

  ------------------------------------------------------------------------------
  -- Interrupt enable register
  ------------------------------------------------------------------------------
  process (clk_i)
  begin
    if rising_edge (clk_i) then
      if rst_n_i = '0' then
        irq_en_reg <= (others => '0');
      else
        if irq_en_wren_i = '1' then
          irq_en_reg <= data_wr_i;
        end if;
      end if;
    end if;
  end process;

  irq_en_o  <= irq_en_reg;


end rtl;
