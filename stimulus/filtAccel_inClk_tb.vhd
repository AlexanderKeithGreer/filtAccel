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

use work.typeAxi.all;

use std.textio.all;

entity filtAccel_inClk_tb is
end filtAccel_inClk_tb;


architecture tb of filtAccel_inClk_tb is
	
	constant c_width 	: integer:=16;
	constant c_tick	    : time := 5ns;
	
	signal 	s_clk 		: std_logic;
	signal	s_rst		: std_logic := '1';
	file 	f_inClk 	: text;
	
	signal 	s_data		: std_logic := '0';
	signal 	s_clkMic	: std_logic;
	signal 	s_debug		: std_logic_vector(31 downto 0);
	
begin
	
	LOAD: process
		variable v_dataLine 	: line;
		variable v_comma 		: line;
		variable v_intData  	: integer; 
	begin

		file_open(f_inClk, "C:/Users/Alexander Greer/Documents/filterAccel/inClk.csv", read_mode);
		while not endfile(f_inClk) loop
			readline (f_inClk, v_dataLine);
			if (v_dataLine'length /= 0) then
				
				--DATA
				read(v_dataLine, v_intData);
				if (v_intData = 0) then 
					s_data <= '0';
				else
					s_data <= '1';
				end if;
			
				--CLOCK SECTION
				s_clk <= '0';
				wait for c_tick;
				s_rst <='0';
				s_clk <= '1';
				wait for c_tick;
			
			end if;			
		end loop;
		wait;
	end process LOAD;
	
	UUT: entity work.filtAccel
	generic map (g_width=>16)
	port map (i_clk=>s_clk, i_rst=>s_rst,
				i_data=>s_data, o_clkMic=>s_clkMic,
				i_WIntoS=>c_zeroWIntoS, o_WFromS=>open,
				i_RIntoS=>c_zeroRIntoS, o_RFromS=>open,
				i_debug=>x"01", o_debug=>s_debug);
	
end tb;
