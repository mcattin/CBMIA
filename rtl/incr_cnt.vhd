-------------------------------------------------------------------------------
-- Title      : Increment counter
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : incr_cnt.vhd
-- Author     : Matthieu Cattin
-- Company    : CERN (BE-CO-HT)
-- Created    : 2012-03-02
-- Last update: 2012-03-14
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: Increment counter
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
-- 2012-03-02  1.0      mcattin         Created, based on nanoFIP incr_counter
-------------------------------------------------------------------------------
-- TODO: - 
--       - 
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;


entity incr_cnt is

  generic(
    g_COUNTER_WIDTH : natural := 4       -- default counter width
    );

  port(

    sys_clk_i : in std_logic;

    counter_incr_i   : in std_logic;    -- increment enable
    counter_reinit_i : in std_logic;    -- reinitializes counter to 0

    counter_o         : out unsigned (g_counter_lgth-1 downto 0);  -- counter
    counter_is_full_o : out std_logic                              -- counter full indication
                                                                   -- (all bits to '1')
    );

end entity incr_cnt;


architecture rtl of incr_cnt is

  constant c_COUNTER_FULL : unsigned (g_COUNTER_WIDTH-1 downto 0) := (others => '1');
  signal   s_counter      : unsigned (g_COUNTER_WIDTH-1 downto 0);

begin


  ------------------------------------------------------------------------------
  -- Synchronous process Incr_Counter
  ------------------------------------------------------------------------------
  p_incr_cnt : process (uclk_i)
  begin
    if rising_edge (uclk_i) then
      if counter_reinit_i = '1' then
        s_counter <= (others => '0');

      elsif counter_incr_i = '1' then
        s_counter <= s_counter + 1;

      end if;
    end if;
  end process p_incr_cnt;

  counter_o         <= s_counter;
  counter_is_full_o <= '1' when s_counter = c_COUNTER_FULL else '0';


end architecture rtl;
