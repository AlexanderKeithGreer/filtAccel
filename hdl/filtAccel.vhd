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
--	 CTRL		0x00  		-- ENTQ[17] 	ENable To Quadrature
--							-- ENRE[16] 	ENable Rising Edge
--							-- COUT[2] 		Clock OUT enable
-- 							-- ENTI[1]  	ENable To In-phase
--							-- ENFE[0]		ENable Falling Edge
-- 	 IDWN		0x04	 	-- IDWN[9:0]	Input sampling relative to master clock (DoWN)
-- 	 ODWN		0x08	 	-- ODWN[9:0]	Output decimation relative
--											to master clock (DoWN)
--	 FILT[0:31]	0x10		-- FLTI[15:0]	FiLTer In-phase Coefficients
--				0x8C		-- FLTQ[15:0]	FiLTer Quadrature Coefficients


library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;
use work.typeAmba.all;

entity filtAccel is
	generic ( g_bAddr	: std_logic_vector(23 downto 0);
			  g_bus		: t_busType:=AHB_LITE_BUS);
	port    ( i_clk		: in	std_logic;
              i_rst		: in 	std_logic;

              i_data	: in 	std_logic;
			  o_clkMic  : out   std_logic;

              i_WIntoS  : in    t_WriteIntoSubord;
              o_WFromS  : out   t_WriteFromSubord;

              i_RIntoS  : in    t_ReadIntoSubord;
              o_RFromS  : out   t_ReadFromSubord;

			  i_HIntoS	: in 	t_HIntoSubord;
			  o_HFromS	: out	t_HFromSubord;

              i_debug 	: in 	std_logic_vector(7 downto 0);
			  o_debug  	: out	std_logic_vector(31 downto 0));

end filtAccel;


architecture arch of filtAccel is
	--Note that the input data is one bit, and is multiplied by 32 bit coef
	--	then added 16 times... 2*2*2*2... extra four bits
	--G_width should probably be fixed to 32
	type t_shiftIn is array (0 to 15) of std_logic;
	type t_filtRegs is array (0 to 31) of std_logic_vector (31 downto 0);
	type t_filtData is array (0 to 16) of signed (31+4 downto 0);
	type t_addTree is array (0 to 4) of signed (31+4 downto 0); --Len 4+1

	type t_statePDM is ( PDM_HIGH, PDM_LOW );
    type t_stateAxiW is ( TAKE_ADDR, TAKE_DATA, GIVE_RESP );
	type t_stateAxiR is ( TAKE_ADDR, GIVE_DATA);
	type t_stateFilt is ( MULT, ADDL1, ADDL2, DONE);

	--See start of file for definitions
	signal r_ctrl 	: std_logic_vector (31 downto 0) := x"0000_0000";
	signal r_idwn   : std_logic_vector (31 downto 0) := x"0000_0008";
	signal r_odwn   : std_logic_vector (31 downto 0) := x"0000_0000";
	signal r_trun	: std_logic_vector (31 downto 0) := x"0000_0000";
	signal r_filt   : t_filtRegs;

	constant c_ctrlAddr : unsigned(5 downto 0) := to_unsigned(0, 6);
	constant c_idwnAddr : unsigned(5 downto 0) := to_unsigned(1, 6);
	constant c_odwnAddr : unsigned(5 downto 0) := to_unsigned(2, 6);
	constant c_filtAddr : unsigned(5 downto 0) := to_unsigned(3, 6);

	--assoc with the inClk
	signal s_dataR		: std_logic;
	signal s_dataF		: std_logic;
	signal s_cntSamp	: unsigned (31 downto 0) := (others => '0');
	signal s_updateFIR	: std_logic; --Do not hold high
	signal s_statePDM 	: t_statePDM;
	--assoc with the filter stages
	signal s_shiftInR	: t_shiftIn;
	signal s_shiftInF	: t_shiftIn;
	signal s_multCoefR 	: t_filtData;
	signal s_multCoefF 	: t_filtData;
	signal s_addTreeR	: t_addTree;
	signal s_addTreeF	: t_addTree;
	signal s_toOutR		: signed(31 downto 0);
	signal s_toOutF		: signed(31 downto 0);
	signal s_stateFilt	: t_stateFilt;
    --assoc with the write part of the AXI-Lite interface
    signal s_stateAxiW  : t_stateAxiW;
	signal s_currAW 	: unsigned(5 downto 0); --Stored Write Address
	signal s_currAH 	: unsigned(5 downto 0); --Stored AHB Address
	signal s_isReadAH	: std_logic;
	--assoc with the read part of the AXI-Lite interface
	signal s_stateAxiR 	: t_stateAxiR;
	signal s_currAR		: unsigned(5 downto 0);
	--assoc with the AHB interface
	signal s_readyWR    : std_logic;
    signal s_readyWA    : std_logic_vector(31 downto 0);

	
begin

	with i_debug select o_debug <=
		x"0000_0000" 							when x"00",
		(0=>s_updateFIR, others => '0') 		when x"01",
		(0=>s_shiftInR(0),1=>s_shiftInR(7),
		 2=>s_shiftInF(0),3=>s_shiftInF(7),
						   others=>'0') 		when x"02",
		std_logic_vector(s_toOutR(31 downto 0))	when x"03",
		std_logic_vector(s_toOutF(31 downto 0))	when x"04",
		x"0000_0000"							when others;

	IN_CLOCK: process (i_clk, i_rst)
	begin
		if (i_rst = '1') then
			s_cntSamp <= (others => '0');
			o_clkMic  <= '0'; 
			s_updateFIR <= '0';
			s_statePDM <= PDM_LOW;
			s_dataR <= '0';
			s_dataF <= '0';
			
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
					s_dataR <= i_data;
				else
					s_dataF <= i_data;
				end if;
			
			--elsif (unsigned(r_idwn) < to_unsigned(4,32)) then
            --    s_cntSamp <= (others => '0');
			--	s_updateFIR <= '0';
            else
				s_cntSamp <= s_cntSamp + to_unsigned(1,32);
				s_updateFIR <= '0';
			end if;
				
		end if;		
	end process IN_CLOCK;

	AXI_EN: if (g_bus = AXI_LITE_BUS) generate

    AXI_WRITE: process (i_clk, i_rst)
    begin
        -- Basically we set the default values for
        --  Our register map in here...
        if (i_rst = '1' and g_bus = AXI_LITE_BUS) then
            r_ctrl <= ( others => '0' );
            r_idwn <= x"0000_0008";	--( others => '0' ); TODO should init as 0, bad for test
            r_odwn <= ( others => '0' );

            for C in 3 to r_filt'length-1 loop
                r_filt(C) <= (others => '0');
            end loop;
            r_filt(00) <= x"0000_0001";
            r_filt(01) <= x"0000_0002";
            r_filt(02) <= x"0000_0003";
            r_filt(16) <= x"0000_0001";
            r_filt(17) <= x"0000_0001";
            r_filt(18) <= x"FFFF_FFFF";

            s_currAW <= (others => '0');

        elsif (rising_edge(i_clk) and g_bus = AXI_LITE_BUS) then
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
		if (i_rst = '1' and g_bus = AXI_LITE_BUS) then
			o_RFromS.oARReady <= '0';
			o_RFromS.oRVAlid <= '0';
			o_RFromS.oRData <= (others=>'0');
			o_RFromS.oRResp <= "10"; --Indicate Slave Error...
			s_currAR <= (others => '0');
		elsif (rising_edge(i_clk) and g_bus = AXI_LITE_BUS) then
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

    end generate AXI_EN;


    AHB_EN: if (g_bus = AHB_LITE_BUS) generate

    AHB_READ_WRITE: process(i_clk, i_rst)
    begin
        -- Basically we set the default values for
        --  Our register map in here...
        if (g_bus = AHB_LITE_BUS and i_rst = '1') then
            r_ctrl <= ( others => '0' );
            r_idwn <= x"0000_0008";	--( others => '0' ); TODO should init as 0, bad for test
            r_odwn <= ( others => '0' );

            for C in 3 to r_filt'length-1 loop
                r_filt(C) <= (others => '0');
            end loop;
            r_filt(00) <= x"0000_0001";
            r_filt(01) <= x"0000_0002";
            r_filt(02) <= x"0000_0003";
            r_filt(16) <= x"0000_0001";
            r_filt(17) <= x"0000_0001";
            r_filt(18) <= x"FFFF_FFFF";

            s_currAW <= (others => '0');

            o_HFromS.oHRData <= (others => '0');
            o_HFromS.oHResp <= "00";
            o_HFromS.oHReadyOut <= '1';

		elsif (g_bus = AHB_LITE_BUS
				and rising_edge(i_clk)) then
            if (i_HIntoS.iHReady = '1') then
                if (s_readyWR = '1') then
					case (s_readyWA) is
						when (g_bAddr & x"00") =>
							r_ctrl <=  i_HIntoS.iHWData;
						when (g_bAddr & x"04") =>
							r_idwn <=  i_HIntoS.iHWData;
						when others =>
					end case;
				else
					o_HFromS.oHRData <= x"0000_0000";
                end if;
            end if;

            if ( i_HIntoS.iHSel = '1') then
                if (i_HIntoS.iHReady = '1' and
                         i_HIntoS.iHWrite = '1' and
                         i_HIntoS.iHTrans(1) = '1' ) then
                    o_HFromS.oHResp <= "10";
                    o_HFromS.oHReadyOut <= '1';
                    s_readyWR <= '1';
                    s_readyWA <= i_HIntoS.iHAddr;

                elsif (i_HIntoS.iHReady = '1' and
                         i_HIntoS.iHWrite = '0' and
                         i_HIntoS.iHTrans(1) = '1' ) then
                    o_HFromS.oHResp <= "10";
                    s_readyWR <= '0';
                    o_HFromS.oHReadyOut <= '1';

                    case (i_HIntoS.iHAddr) is
                            when (g_bAddr & x"00") =>
                                o_HFromS.oHRData <= r_ctrl;
                            when (g_bAddr & x"04") =>
                                o_HFromS.oHRData <= r_idwn;
                            when others =>
                                o_HFromS.oHRData <= x"0000_0000";
                    end case;
                else
					o_HFromS.oHReadyOut <= '1';
                end if;
            else
                s_readyWR <= '0';
                s_readyWA <= x"0000_0000";
            end if;
		end if;
	end process AHB_READ_WRITE;

	end generate AHB_EN;


    s_toOutR <= s_addTreeR(4)(31 downto 0);
    s_toOutF <= s_addTreeF(4)(31 downto 0);

    --FILTER... primary constraint imposed is the speed of the MAC operations
	--				requires a tree in order to implement.
	--			We can pipeline all operations fine, only so long as the load
	--				is triggered at the right time, and we shift our output
	--				when we are "done"...
    FILTER: process (i_clk, i_rst)
    begin
        if (i_rst = '1') then
			s_multCoefR <= (others=>(others=>'0'));
			s_multCoefF <= (others=>(others=>'0'));
			s_addTreeR <= (others=>(others=>'0'));
			s_addTreeF <= (others=>(others=>'0'));
			for S in 0 to (s_shiftInR'length-1) loop
				s_shiftInR(S) <= '0';
				s_shiftInF(S) <= '0';
			end loop;
        elsif (rising_edge(i_clk)) then

			if (s_updateFIR = '1') then --Upddate... incl shift register
				s_stateFilt <= MULT;
				for S in 1 to s_shiftInR'length-1 loop
					s_shiftInR(S) <= s_shiftInR(S-1);
					s_shiftInF(S) <= s_shiftInF(S-1);
				end loop;
				s_shiftInR(0) <= s_dataR;
				s_shiftInF(0) <= s_dataF;
			end if;


			for S in s_shiftInR'range loop --Multiplication stage
				if (s_shiftInR(S) = '1') then
					s_multCoefR(S) <= resize(signed(r_filt(S)), s_multCoefR(S)'length);
				else
					s_multCoefR(S) <= (others=> '0');
				end if;

				if (s_shiftInF(S) = '1') then
					s_multCoefF(S) <= resize(signed(r_filt(S+15)), s_multCoefF(S)'length);
				else
					s_multCoefF(S) <= (others=> '0');
				end if;
			end loop;

			s_addTreeR(4) <= s_addTreeR(0) + s_addTreeR(1) +
								s_addTreeR(2) + s_addTreeR(3);
			s_addTreeF(4) <= s_addTreeF(0) + s_addTreeF(1) +
								s_addTreeF(2) + s_addTreeF(3);
			for S in 0 to (s_shiftInR'length)/4-1 loop --Addition stage
				s_addTreeR(S) <= s_multCoefR(4*S) + s_multCoefR(4*S+1) +
									s_multCoefR(4*S + 2) + s_multCoefR(4*S+3);
				s_addTreeF(S) <= s_multCoefF(4*S) + s_multCoefF(4*S+1) +
									s_multCoefF(4*S + 2) + s_multCoefF(4*S+3);
			end loop;


			case (s_stateFilt) is
				when MULT => s_stateFilt <= ADDL2;
				when ADDL2 => s_stateFilt <= ADDL1;
				when ADDL1 => s_stateFilt <= DONE;
				when DONE => s_stateFilt <= DONE;
			end case;

        end if;
    end process FILTER;

end arch;
