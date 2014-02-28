-------------------------------------------------------------------------------
-- Title      : Decrement counter
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : decr_cnt.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-03-02
-- Last update: 2012-03-23
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: Decrement counter
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
-- 2012-03-02  1.0      mcattin         Created, based on nanoFIP decr_counter
-------------------------------------------------------------------------------
-- TODO: - 
--       - 
-------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;


entity decr_cnt is

  generic(
    g_COUNTER_WIDTH : natural := 4      -- default counter width
    );

  port(

    sys_clk_i : in std_logic;

    sys_rst_n_i : in std_logic;         -- resets counter to all '1'

    counter_decr_i : in std_logic;                                       -- decrement enable
    counter_load_i : in std_logic;                                       -- load enable; loads counter to counter_top_i
    counter_top_i  : in std_logic_vector(g_COUNTER_WIDTH - 1 downto 0);  -- load value

    counter_o         : out std_logic_vector(g_COUNTER_WIDTH - 1 downto 0);  -- counter
    counter_is_zero_o : out std_logic                                        -- empty counter indication
    );

end entity decr_cnt;


architecture rtl of decr_cnt is

  signal s_counter : unsigned (g_COUNTER_WIDTH - 1 downto 0);

begin


  ------------------------------------------------------------------------------
  -- Synchronous process Decr_Counter
  ------------------------------------------------------------------------------
  p_decr_cnt : process (sys_clk_i)
  begin
    if rising_edge (sys_clk_i) then

      if sys_rst_n_i = '0' then
        s_counter <= (others => '1');
      else

        if counter_load_i = '1' then
          s_counter <= unsigned(counter_top_i);

        elsif counter_decr_i = '1' then
          s_counter <= s_counter - 1;

        end if;
      end if;
    end if;
  end process p_decr_cnt;

  counter_o         <= std_logic_vector(s_counter);
  counter_is_zero_o <= '1' when s_counter = to_unsigned(0, s_counter'length) else '0';


end architecture rtl;
