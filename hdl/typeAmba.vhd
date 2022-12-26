-- typeAxi.vhd
-- Contains a set of records intended to hold IO for any AXI interface,
--  but I'm currently focusing on axi4-lite.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package typeAmba is

    type t_busType is (AXI_LITE_BUS, AHB_LITE_BUS);

    type t_WriteIntoSubord is record
        iAWValid   : std_logic;
        iAWAddr    : std_logic_vector(31 downto 0);
        iAWProt    : std_logic_vector(2 downto 0); --Insn & Secure & Priv
        iWValid    : std_logic;
        iWData     : std_logic_vector(31 downto 0);
        iWStrb     : std_logic_vector(3 downto 0); --Which Bytes valid?
        iBReady    : std_logic;
    end record;

    type t_WriteFromSubord is record
        oAWReady   : std_logic;
        oWReady    : std_logic;
        oBValid    : std_logic;
        oBResp     : std_logic_vector(1 downto 0); --OKAY, EXOKAY, SERR, IERR
    end record;

    type t_ReadIntoSubord  is record
        iARValid   : std_logic;
        iARAddr    : std_logic_vector(31 downto 0); --
        iARProt    : std_logic_vector(2 downto 0); --See above
        iRReady    : std_logic;
    end record;
    
    type t_ReadFromSubord is record
        oARReady   : std_logic;
        oRValid    : std_logic;
        oRData     : std_logic_vector(31 downto 0);
        oRResp     : std_logic_vector(1 downto 0); --See above
    end record;

    constant c_zeroWIntoS : t_WriteIntoSubord :=
        ('0', x"0000_0000", b"000", '0', x"0000_0000", b"0000",'0');
    constant c_zeroRIntoS : t_ReadIntoSubord :=
        ('0', x"0000_0000", b"000", '0');

    type t_HIntoSubord is record
        iHAddr     : std_logic_vector(31 downto 0);
        iHBurst    : std_logic_vector( 2 downto 0);
        iHMastLock : std_logic;                     --Not important, DNU
        iHProt     : std_logic_vector( 3 downto 0); --Not important, DNU
        iHSize     : std_logic_vector( 2 downto 0);
        iHTrans    : std_logic_vector( 1 downto 0);
        iHWData    : std_logic_vector(31 downto 0);
        iHWrite    : std_logic;

        iHSel      : std_logic;
        iHReady    : std_logic;
    end record;

    type t_HFromSubord is record
        oHRData    : std_logic_vector(31 downto 0);
        oHReadyOut : std_logic;
        oHResp     : std_logic_vector(1 downto 0);
    end record;

    constant c_zeroHIntoS : t_HIntoSubord :=
    --          ADDR  BURST   ML  PROT   SIZE  TRNS         DATA   WR  SEL   RDY
        (x"00000000", "000", '0', x"0", "000", "00", x"00000000", '0', '0', '0');

end package;

package body typeAmba is
    -- package body part
end typeAmba;
