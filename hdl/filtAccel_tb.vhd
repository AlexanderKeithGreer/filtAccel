
library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;
use work.typeAxi.all;
use std.textio.all;


entity filtAccel_tb is
end filtAccel_tb;


architecture tb of filtAccel_tb is
	
	component filtAccel is
		generic 	( 	g_width 	: integer:=16 );
		port	 	( 	i_clk		: in	std_logic;
						i_rst		: in 	std_logic;
						
						i_data	: in 	std_logic;
						o_clkMic : out std_logic;

						i_WIntoS  : in    t_WriteIntoSubord;
						o_WFromS  : out   t_WriteFromSubord;

						i_RIntoS  : in    t_ReadIntoSubord;
						i_RFromS  : out   t_ReadFromSubord;

						o_debug  : out	std_logic);	
	end component;
	
	constant c_width 	: integer:=16;
	constant c_tick	: time := 5ns;
	
	signal 	s_clk 		: std_logic;
	signal	s_rst			: std_logic := '1';
	file 	 	f_inClk 		: text;
	
	signal 	s_data		: std_logic := '0';
	signal 	s_clkMic		: std_logic;
	signal 	s_debugClk	: std_logic;
	
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
	
	
	UUT: filtAccel 
		generic map (g_width=>16)
		port map (i_clk=>s_clk, i_rst=>s_rst, 
						i_data=>s_data, 
						o_clkMic=>s_clkMic, o_debug=>s_debugClk);
	
end tb;
