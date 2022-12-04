--------------------------------------------------------------------------------
-- Company: Personal
--
-- File: filtAccel_axi_tb.vhd
-- File history:
--      0.0a0.00: 06/OCT/2022: Axi Lite Start
--
-- Description: 
--
--  This is being left alone for now...
--
--  This testbench should simultaneously test both the read and write operations
--  The things that need to be tested are...
--  WRITE_TEST:
--      -- Assert Send ADDR, PROT, AWVALID
--      -- Respond with AWREADY, save address into internal SM
--      -- Assert DATA, WSTRB, and WVALID 
--      -- Respond with WREADY, write to previously saved address, raise BVALID
--      -- Respond with BRESP
--      -- Assert BOKAY
-- 
--
-- Targeted device: <Family::SmartFusion2> <Die::M2S010> <Package::144 TQ>
-- Author: Alexander Greer
--
--------------------------------------------------------------------------------

library IEEE;

use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

use work.typeAxi.all;

entity filtAccel_axi_tb is
end filtAccel_axi_tb;


architecture architecture_filtAccel_axi_tb of filtAccel_axi_tb is
   -- signal, component etc. declarations

   component filtAccel
        generic ( g_width       : integer:=16 );
        port    (   i_clk       : in	std_logic;
                    i_rst	    : in 	std_logic;

                    i_data	    : in 	std_logic;
                    o_clkMic    : out   std_logic;

                    i_WIntoS    : in    t_WriteIntoSubord;
                    o_WFromS    : out   t_WriteFromSubord;

                    i_RIntoS    : in    t_ReadIntoSubord;
                    o_RFromS    : out   t_ReadFromSubord;

                    o_debug     : out	std_logic);
    end component filtAccel;
    
    constant c_clkT : time := 10ns;

    type t_arrayWIntoS is array (natural range <>) of t_WriteIntoSubord;
    type t_arrayWFromS is array (natural range <>) of t_WriteFromSubord;
    type t_arrayRIntoS is array (natural range <>) of t_ReadIntoSubord;
    type t_arrayRFromS is array (natural range <>) of t_ReadFromSubord;

    signal s_WIntoS : t_WriteIntoSubord;
    signal s_WFromS : t_WriteFromSubord;
    signal s_RIntoS : t_ReadIntoSubord;
    signal s_RFromS : t_ReadFromSubord;


    signal s_clk    : std_logic;
    signal s_rst    : std_logic := '1';
    signal s_data   : std_logic := '0';
    signal s_debug  : std_logic;


    constant c_arrayWIntoS : t_arrayWIntoS := (
        ('0',x"00000000", "000",    '0', x"0000000F", "1111",    '0'),
        ('1',x"00000001", "000",    '0', x"0000000F", "1111",    '0'),
        ('0',x"00000000", "000",    '0', x"0000000F", "1111",    '0'),
        ('0',x"00000000", "000",    '0', x"0000000F", "1111",    '0'),
        ('0',x"00000000", "000",    '1', x"0000002F", "1111",    '0'),
        ('0',x"00000000", "000",    '0', x"0000000F", "1111",    '0'),
        ('0',x"00000000", "000",    '0', x"0000000F", "1111",    '1'),
        ('0',x"00000000", "000",    '0', x"0000000F", "1111",    '0'),
        ('1',x"00000001", "000",    '0', x"0000003F", "1111",    '0'),
        ('0',x"00000000", "000",    '0', x"0000000F", "1111",    '0'),
        ('0',x"00000000", "000",    '0', x"0000000F", "1111",    '0'),
        ('0',x"00000000", "000",    '1', x"0000000F", "1111",    '0'),
        ('0',x"00000000", "000",    '0', x"0000000F", "1111",    '0'),
        ('0',x"00000000", "000",    '0', x"0000000F", "1111",    '1'),
        ('0',x"00000000", "000",    '0', x"0000000F", "1111",    '0')
    );

    constant c_arrayRIntoS : t_arrayRIntoS := (
        ('0',x"00000000", "000",    '0'),
        ('0',x"00000001", "000",    '0'),
        ('1',x"00000001", "000",    '0'),
        ('0',x"00000000", "000",    '1'),
        ('0',x"00000000", "000",    '0'),
        ('0',x"00000000", "000",    '0'),
        ('0',x"00000000", "000",    '1'),
        ('0',x"00000000", "000",    '0'),
        ('1',x"00000001", "000",    '0'),
        ('0',x"00000000", "000",    '0'),
        ('0',x"00000000", "000",    '1'),
        ('0',x"00000000", "000",    '0'),
        ('0',x"00000000", "000",    '0'),
        ('1',x"00000001", "000",    '1'),
        ('0',x"00000000", "000",    '0')
    );

begin

    LOAD: process 
    begin
        for S in c_arrayWIntoS'range loop
            s_clk <= '0';
            s_WIntoS <= c_arrayWIntoS(S);
            s_RIntoS <= c_arrayRIntoS(S);
            wait for c_clkT;

            s_clk <= '1';
            if (S /= 0) then
                s_rst <= '0';
            end if;

            wait for c_clkT;
        end loop;
        wait;
    end process LOAD;

    UUT: entity work.filtAccel
        generic map (g_width=>16)
        port map (i_clk=>s_clk, i_rst=>s_rst,
                    i_data=>s_data, o_clkMic=>open,
                    i_WIntoS=>S_WIntoS, o_WFromS=>s_WFromS,
                    i_RIntoS=>S_RIntoS, o_RFromS=>s_RFromS,
                    o_debug=>s_debug);

end architecture_filtAccel_axi_tb;

