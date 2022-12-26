--------------------------------------------------------------------------------
-- Company: <Name>
--
-- File: filtAccel_ahb_tb.vhd
-- File history:
--      <Revision number>: <Date>: <Comments>
--      <Revision number>: <Date>: <Comments>
--      <Revision number>: <Date>: <Comments>
--
-- Description:
--
-- <Description here>
--
-- Targeted device: <Family::SmartFusion2> <Die::M2S010> <Package::144 TQ>
-- Author: <Name>
--
--------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use work.typeAmba.all;

entity filtAccel_ahb_tb is
end filtAccel_ahb_tb;

architecture tb of filtAccel_ahb_tb is

    type t_arrayHIntoS is array (natural range <>) of t_HIntoSubord;
    type t_arrayHFromS is array (natural range <>) of t_HFromSubord;
    constant c_arrayHIntoS : t_arrayHIntoS := (
    --          ADDR  BURST   ML  PROT   SIZE  TRNS         DATA   WR  SEL   RDY
        (x"50000000", "000", '0', x"3", "000", "10", x"00000000", '1', '0', '1'),
        (x"50000000", "000", '0', x"3", "000", "10", x"00000000", '1', '1', '1'),
        (x"50000000", "000", '0', x"3", "000", "10", x"0000005A", '1', '0', '1'),
        (x"50000000", "000", '0', x"3", "000", "10", x"00000000", '0', '1', '1'),
        (x"50000000", "000", '0', x"3", "000", "10", x"00000000", '1', '0', '1'),
        (x"50000000", "000", '0', x"3", "000", "10", x"00000000", '1', '0', '1'),
        (x"50000000", "000", '0', x"3", "000", "10", x"00000000", '1', '0', '1'),
        (x"50000000", "000", '0', x"3", "000", "10", x"00000000", '1', '0', '1'),
        (x"50000000", "000", '0', x"3", "000", "10", x"00000000", '1', '0', '1'),
        (x"50000000", "000", '0', x"3", "000", "10", x"00000000", '1', '0', '1')
    );
    constant c_arrayHFromS : t_arrayHFromS := (
    --          DATA  RDY  RESP
        (x"00000000", '1', "00"),
        (x"00000000", '1', "00"),
        (x"00000000", '1', "10"),
        (x"00000000", '1', "10"),
        (x"0000005A", '1', "10"),
        (x"00000000", '1', "10"),
        (x"00000000", '1', "10"),
        (x"00000000", '1', "10"),
        (x"00000000", '1', "10"),
        (x"00000000", '1', "10")
    );

    constant c_sysClkPeriod : time := 10 ns; -- 100MHZ
    constant c_testVecLagClk : time := 500ps;
    signal s_clk : std_logic := '0';
    signal s_rst : std_logic := '1';

    signal s_HIntoS : t_HIntoSubord;
    signal s_HFromS : t_HFromSubord;

begin

    process
    begin

        assert (c_arrayHIntoS'length = c_arrayHFromS'length)
        report "Mismatch between input and output test vector lengths " &
                "( " & integer'image(c_arrayHIntoS'length) & " vs "
                     & integer'image(c_arrayHFromS'length) & " )"
                severity error;

        --Don't try to reset, I think?
        for T in c_arrayHIntoS'range loop
            --Change the inputs on the clock edge... master has same clock...
            s_clk <= '1';
            wait for c_testVecLagClk;
            s_HIntoS <= c_arrayHIntoS(T);
            wait for c_sysClkPeriod/2-c_testVecLagClk;

            s_clk <= '0';
            s_rst <= '0';
            assert (s_HFromS.oHRData = c_arrayHFromS(T).oHRData)
            report "Mismatch with HRDATA on cycle (" & integer'image(T) & ")"
                severity error;
            assert (s_HFromS.oHReadyOut = c_arrayHFromS(T).oHReadyOut)
            report "Mismatch with HREADYOUT on cycle (" & integer'image(T) & ")"
                severity error;
            assert (s_HFromS.oHResp = c_arrayHFromS(T).oHResp)
            report "Mismatch with HRESP on cycle (" & integer'image(T) & ")"
                severity error;
            wait for c_sysClkPeriod/2;
        end loop;
        wait;
    end process;

    -- Clock Driver

    -- Instantiate Unit Under Test:  EthView
    ETHV : entity work.FiltAccel
        generic map(g_bAddr=>x"50_00_00", g_bus=>AHB_LITE_BUS)
        port map(i_clk=>s_clk, i_rst=>s_rst,
                 i_data=>'1',               o_clkMic=>open,
                 i_WIntoS=>c_zeroWIntoS,    o_WFromS=>open,
                 i_RIntoS=>c_zeroRIntoS,    o_RFromS=>open,
                 i_HIntoS=>s_HIntoS,        o_HFromS=>s_HFromS,
                 i_debug=>x"00",            o_debug=>open);


end tb;

