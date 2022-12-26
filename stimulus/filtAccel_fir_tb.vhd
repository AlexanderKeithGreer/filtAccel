----------------------------------------------------------------------
-- Created by Microsemi SmartDesign Wed Oct 05 20:53:17 2022
-- Testbench Template
-- This is a basic testbench that instantiates your design with basic 
-- clock and reset pins connected.  If your design has special
-- clock/reset or testbench driver requirements then you should 
-- copy this file and modify it. 
----------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Company: Self
--
-- File: filtAccel_inClk_tb.vhd
-- File history:
--      0.1: <Date>: <Comments>
--
-- Description: 
--
--
-- Targeted device: <Family::SmartFusion2> <Die::M2S010> <Package::144 TQ>
-- Author: Alexander Greer
--
--------------------------------------------------------------------------------

library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;
use std.textio.all;

use work.typeAmba.all;

entity filtAccel_fir_tb is
end filtAccel_fir_tb;


architecture tb of filtAccel_fir_tb is
	type t_arrayWIntoS is array (natural range <>) of t_WriteIntoSubord;
    type t_arrayWFromS is array (natural range <>) of t_WriteFromSubord;

	constant c_width 	: integer:=16;
	constant c_tick	    : time := 1ns;
	constant c_rate		: integer := 12;

	constant c_rateReg	: std_logic_vector:=std_logic_vector(to_unsigned(c_rate-2,32));
	constant c_arrayWIntoS : t_arrayWIntoS := (
        ('0',x"00000000", "000",    '0', x"00000000", "1111",    '0'),
        ('1',x"00000001", "000",    '0', x"00000000", "1111",    '0'),
        ('0',x"00000000", "000",    '0', x"00000000", "1111",    '0'),
        ('0',x"00000000", "000",    '1',   c_rateReg, "1111",    '0'),
        ('0',x"00000000", "000",    '0', x"00000000", "1111",    '1'),
        ('0',x"00000000", "000",    '0', x"00000000", "1111",    '0'));

	file 	f_fir 		: text;
	signal 	s_clk 		: std_logic;
	signal	s_rst		: std_logic := '1';
	signal 	s_WIntoS 	: t_WriteIntoSubord;
    signal 	s_WFromS 	: t_WriteFromSubord;
	signal 	s_data		: std_logic := '0';
	signal 	s_clkMic	: std_logic;
	signal 	s_debug		: std_logic_vector(31 downto 0);
	signal	s_lineIdx	: integer := 0;

begin
	
	LOAD: process
		variable v_dataLine 	: line;
		variable v_comma 		: character;
		variable v_intDataRise 	: integer;
		variable v_intDataFall 	: integer;
		variable v_clkMicL		: std_logic := '0';

	begin

		--Initial Setup, write to the relevant Registers...
		for W in c_arrayWIntoS'range loop
			s_WIntoS <= c_arrayWIntoS(W);
			s_clk <= '0';
			wait for c_tick;
			s_rst <= '0';
			s_clk <= '1';
			wait for c_tick;
		end loop;

		--Time taken for writing to AXI can desync the input data
		--  from the micClk... which is in theory driven by the
		--  clock! So wait...
		v_clkMicL := s_clkMic;
		while ( not (v_clkMicL = '1' and s_clkMic = '0')) loop
			v_clkMicL := s_clkMic;
			s_clk <= '0';
			wait for c_tick;
			s_clk <= '1';
			wait for c_tick;
		end loop;

		--Actual Test, Run the FIR...
		file_open(f_fir, "D:/MicrosemiProject/filtAccel/stimulus/fir.csv", read_mode);
		while not endfile(f_fir) loop
			readline (f_fir, v_dataLine);
			if (v_dataLine'length /= 0) then
				s_lineIdx <= s_lineIdx + 1;

				--DATA
				read(v_dataLine, v_intDataRise);
				if (v_intDataRise = 0) then
					s_data <= '0';
				else
					s_data <= '1';
				end if;
				for C in 0 to c_rate/2-1 loop
					s_clk <= '0';
					wait for c_tick;
					s_clk <= '1';
					wait for c_tick;
				end loop;

				read(v_dataLine, v_comma);
				read(v_dataLine, v_intDataFall);
				if (v_intDataFall = 0) then
					s_data <= '0';
				else
					s_data <= '1';
				end if;
				for C in 0 to c_rate/2-1 loop
					s_clk <= '0';
					wait for c_tick;
					s_clk <= '1';
					wait for c_tick;
				end loop;
			
			end if;			
		end loop;
		wait;
	end process LOAD;

	UUT: entity work.filtAccel
	generic map (g_width=>16)
	port map (i_clk=>s_clk, i_rst=>s_rst,
				i_data=>s_data, o_clkMic=>s_clkMic,
				i_WIntoS=>s_WIntoS, o_WFromS=>s_WFromS,
				i_RIntoS=>c_zeroRIntoS, o_RFromS=>open,
				i_debug=>x"03", o_debug=>s_debug);
	
end tb;
