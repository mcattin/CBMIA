-------------------------------------------------------------------------------
-- Title      : Memory interface mux
-- Project    : CBMIA, MIL1553 bus controller
-- Website    : http://
-------------------------------------------------------------------------------
-- File       : mem_interface_mux.vhd
-- Author     : Pablo Antonio Alvarez Sanchez
-- Company    : CERN (BE-CO-HT)
-- Created    : 2002-09-30
-- Last update: 2012-03-16
-- Platform   : FPGA-generic
-- Standard   : VHDL '87
-------------------------------------------------------------------------------
-- Description: Data multiplexor
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
-- 2002-09-30  1.0      paas            Created
-- 2012-03-09  1.1      mcattin         Clean-up, add license
-------------------------------------------------------------------------------
-- TODO: -
--       -
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;
library work;
use work.mem_interface_pkg.all;


entity mem_interface_mux is

  generic(
    INDH : integer := 10;
    INDL : integer := 0
    );

  port (
    IntAdd    : in  t_int_addr;
    DoingOpC  : in  std_logic;
    DoneRam   : in  t_selected_pos;
    DataArray : in  t_mux_data_array;
    Sel       : out t_selected_pos;
    AddL      : out t_int_addr;
    Done      : out std_logic;
    ThisIs    : out std_logic;
    DataOut   : out t_int_data
    );

end mem_interface_mux;


architecture rtl of mem_interface_mux is

begin

  process(IntAdd, DoingOpC, DataArray, DoneRam)
    variable vAdd : integer;
  begin
    vAdd    := Conv_Integer(IntAdd);
    AddL    <= IntAdd;
    Sel     <= (others => '0');
    ThisIs  <= '0';
    Done    <= '0';
    DataOut <= (others => '0');
    for I in INDL to INDH loop
      if ADDTABLE(I).Delay = 0 then
        if vAdd >= ADDTABLE(I).AddL and vAdd <= ADDTABLE(I).AddH then
          Sel(ADDTABLE(I).PosToSel) <= '1';
          Done                      <= DoingOpC;
          DataOut                   <= DataArray(ADDTABLE(I).PosToSel);
          AddL                      <= IntAdd;
          ThisIs                    <= '1';
          exit;
        end if;
      elsif vAdd >= ADDTABLE(I).AddL and vAdd <= ADDTABLE(I).AddH then
        Sel(ADDTABLE(I).PosToSel) <= '1';
        Done                      <= DoneRam(ADDTABLE(I).PosToSel);
        DataOut                   <= DataArray(ADDTABLE(I).PosToSel);
        if (ADDTABLE(I).PosToSel  <= INDH) or (ADDTABLE(I).PosToSel >= INDL) then
          ThisIs <= '1';
        end if;
        AddL <= conv_std_logic_vector(ADDTABLE(ADDTABLE(I).PosToSel).AddL, AddL'length);
        exit;
      end if;
    end loop;
  end process;


end rtl;

