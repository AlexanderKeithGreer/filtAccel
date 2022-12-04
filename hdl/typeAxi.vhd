-- typeAxi.vhd
-- Contains a set of records intended to hold IO for any AXI interface,
--  but I'm currently focusing on axi4-lite.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package typeAxi is

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

end package;

package body typeAxi is
    -- package body part
end typeAxi;
