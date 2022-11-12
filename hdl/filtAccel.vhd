-- --------------------------------
--   FILT ACCEL
-- --------------------------------
-- Take in FM radio or PDM from the i_data pin
-- (One bit quantised data!)
-- Filter it
-- Decimate it
-- Optional (Convert via hard limiter, count zero crossings, idk?)

-- --------------------------------
--   REGISTER MAP
-- --------------------------------
--	 CTRL  		-- ENTQ[17] 	ENable To Quadrature
--				-- ENRE[16] 	ENable Rising Edge
--				-- COUT[2] 		Clock OUT enable
-- 				-- ENTI[1]  	ENable To In-phase
--				-- ENFE[0]		ENable Falling Edge
--
--  IDWN	 	-- IDWN[9:0]	Input sampling relative to master clock (DoWN)
--  ODWN	 	-- ODWN[9:0]	Output decimation relative
--											to master clock (DoWN)
--	 FILT[63:0]	-- FILT[32:0]  FITLer Coefficients


library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;
use work.typeAxi.all;

entity filtAccel is
	generic ( g_width 	: integer:=16 );
	port    ( i_clk		: in	std_logic;
              i_rst		: in 	std_logic;

              i_data	: in 	std_logic;
			  o_clkMic  : out   std_logic;

              i_WIntoS  : in    t_WriteIntoSubord;
              o_WFromS  : out   t_WriteFromSubord;

              i_RIntoS  : in    t_ReadIntoSubord;
              o_RFromS  : out   t_ReadFromSubord;

			  o_debug  : out	std_logic);
end filtAccel;


architecture arch of filtAccel is
	
	type t_filtRegs is array (0 to 31) of std_logic_vector (31 downto 0);
	type t_shiftIn is array (0 to 31) of signed (g_width-1 downto 0);
	type t_statePDM is ( PDM_HIGH, PDM_LOW );
    type t_stateAxiW is ( TAKE_ADDR, TAKE_DATA, GIVE_RESP );
	type t_stateAxiR is ( TAKE_ADDR, GIVE_DATA);

	--See start of file for definitions
	signal r_ctrl 	: std_logic_vector (31 downto 0) := x"0000_0000";
	signal r_idwn   : std_logic_vector (31 downto 0) := x"0000_0008";
	signal r_odwn   : std_logic_vector (31 downto 0) := x"0000_0000";
	signal r_filt   : t_filtRegs;
	constant c_ctrlAddr : unsigned(5 downto 0) := to_unsigned(0, 6);
	constant c_idwnAddr : unsigned(5 downto 0) := to_unsigned(1, 6);
	constant c_odwnAddr : unsigned(5 downto 0) := to_unsigned(2, 6);
	constant c_filtAddr : unsigned(5 downto 0) := to_unsigned(3, 6);

	--assoc with clocking
	signal s_cntSamp	: unsigned (31 downto 0) := (others => '0');
	signal s_updateFIR: std_logic; --Do not hold high
	signal s_statePDM : t_statePDM;
	--assoc with the filter stages
	signal s_shiftInR	: t_shiftIn;
	signal s_shiftInF	: t_shiftIn;
    --assoc with the write part of the AXI-Lite interface
    signal s_stateAxiW  : t_stateAxiW;
	signal s_currAW 		: unsigned(5 downto 0); --Stored Write Address
	--assoc with the read part of the AXI-Lite interface
	signal s_stateAxiR 	: t_stateAxiR;
	signal s_currAR		: unsigned(5 downto 0);
	
begin
		
	IN_CLOCK: process (i_clk, i_rst)
	begin
		if (i_rst = '1') then
			s_cntSamp <= (others => '0');
			o_clkMic  <= '0'; 
			s_updateFIR <= '0';
			s_statePDM <= PDM_LOW;
			
		elsif (rising_edge(i_clk)) then 
			
			if (s_cntSamp = shift_right(unsigned(r_idwn),1)) then
				s_cntSamp <= (others => '0');
				if (s_statePDM = PDM_LOW) then
					o_clkMic <= '1';
					s_statePDM <= PDM_HIGH;
				else
					o_clkMic <= '0';
					s_statePDM <= PDM_LOW;
					s_updateFIR <= '1';      --Clk subord processes on fall edge
				end if;
			
			elsif (s_cntSamp = shift_right(unsigned(r_idwn),2)) then
				s_cntSamp <= s_cntSamp + to_unsigned(1,32);
				if (s_statePDM = PDM_HIGH) then
					if (i_data = '1') then
						s_shiftInR(0) <= to_signed(1,g_width);
					end if;
				else
					if (i_data = '1') then
						s_shiftInF(0) <= to_signed(1,g_width);
					end if;
				end if;
			
			elsif (unsigned(r_idwn) < to_unsigned(4,32)) then
                s_cntSamp <= (others => '0');
				s_updateFIR <= '0';
            else
				s_cntSamp <= s_cntSamp + to_unsigned(1,32);
				s_updateFIR <= '0';
			end if;
				
		end if;		
	end process IN_CLOCK;
	
	-- WRITE
    -- ADDR : _
    -- DATA :  _
    -- RESP :   _
    AXI_WRITE: process (i_clk, i_rst)
    begin
        -- Basically we set the default values for
        --  Our register map in here...
        if (i_rst = '1') then
            r_ctrl <= ( others => '0' );
            r_idwn <= ( others => '0' );
            r_odwn <= ( others => '0' );
            for C in 0 to r_filt'length-1 loop
                r_filt(C) <= (others => '0');
            end loop;
            s_currAW <= (others => '0');

        elsif (rising_edge(i_clk)) then
            case (s_stateAxiW) is
                when TAKE_ADDR =>
					o_WFromS.oAWReady <= '0';
					o_WFromS.oWReady <= '0';
					if (i_WIntoS.iAWValid = '1') then
						o_WFromS.oAWReady <= '1';
						s_currAW <= unsigned(i_WIntoS.iAWAddr(5 downto 0));
						s_stateAxiW <= TAKE_DATA;
					end if;
                when TAKE_DATA =>
					o_WFromS.oAWReady <= '0';
					if (i_WIntoS.iWValid = '1') then
						o_WFromS.oWReady <= '1';
						s_stateAxiW <= GIVE_RESP;

						case (s_currAW) is
							when c_ctrlAddr => r_ctrl <= i_WintoS.iWData;
							when c_idwnAddr => r_idwn <= i_WintoS.iWData;
							when c_odwnAddr => r_odwn <= i_WintoS.iWData;
							when others =>
								r_filt(to_integer(s_currAW)-2) <= i_WintoS.iWData;
						end case;
					end if;
                when GIVE_RESP =>
					o_WFromS.oAWReady <= '0';
					o_WFromS.oWReady <= '0';
					o_WFromS.oBValid <= '1';
					o_WFromS.oBResp <= "00";

					if (i_WIntoS.iBReady = '1') then
						s_stateAxiW <= TAKE_ADDR;
					end if;

            end case;
        end if;
    end process AXI_WRITE;


    AXI_READ: process (i_clk, i_rst)
    begin
		if (i_rst = '1') then
			o_RFromS.oARReady <= '0';
			o_RFromS.oRVAlid <= '0';
			o_RFromS.oRData <= (others=>'0');
			o_RFromS.oRResp <= "10"; --Indicate Slave Error...
			s_currAR <= (others => '0');
		elsif (rising_edge(i_clk)) then
			case (s_stateAxiR) is
				when TAKE_ADDR =>
					o_RFromS.oRVAlid <= '0';
					if (i_RIntoS.iARVAlid = '1') then
						o_RFromS.oARReady <= '1';
						s_currAR <= unsigned(i_RintoS.iARAddr(5 downto 0));
						s_stateAxiR <= GIVE_DATA;
					end if;
				when GIVE_DATA =>
					o_RFromS.oARReady <= '0';
					o_RFromS.oRVAlid <= '1';
					case (s_currAR) is
						when c_ctrlAddr => o_RfromS.oRData <= r_ctrl;
						when c_idwnAddr => o_RfromS.oRData <= r_idwn;
						when c_odwnAddr => o_RfromS.oRData <= r_odwn;
						when others =>
							o_RfromS.oRData <= r_filt(to_integer(s_currAR)-2);
					end case;

					o_RFromS.oRResp <= "00";

					if (i_RIntoS.iRReady = '1') then
						s_stateAxiR <= TAKE_ADDR;
					end if;
			end case;
		end if;
    end process AXI_READ;

    --FILTER: process (i_clk, i_rst)
    --begin
    --    if i_rst = '0')
    --end process FILTER;

end arch;
