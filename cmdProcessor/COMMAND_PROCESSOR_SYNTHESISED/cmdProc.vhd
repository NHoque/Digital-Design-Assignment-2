
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.common_pack.all;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
use UNISIM.VPKG.ALL;


entity cmdProc is 
    port (
      clk:		in std_logic;
      reset:		in std_logic;
      rxnow:		in std_logic; --rxvalid
      rxData:			in std_logic_vector (7 downto 0);
      txData:			out std_logic_vector (7 downto 0);
      rxdone:		out std_logic;
      ovErr:		in std_logic;
      framErr:	in std_logic;
      txnow:		out std_logic;
      txdone:		in std_logic;
      start: out std_logic;
      numWords_bcd: out BCD_ARRAY_TYPE(2 downto 0);
      dataReady: in std_logic;
      byte: in std_logic_vector(7 downto 0);
      maxIndex: in BCD_ARRAY_TYPE(2 downto 0);
      dataResults: in CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
      seqDone: in std_logic
    );
end;

architecture arch of cmdProc is 
	type state is (S0, S1, S2, S3, S4, S5, S6, S7, S8);
	signal cur_state, next_state : state;
	signal received_cmd : std_logic_vector(31 downto 0);
	signal cmd_is_valid : std_logic;
	--signal hexout : std_logic_vector(7 downto 0);
	signal hex_1, hex_2 : std_logic_vector(7 downto 0);
	
	type lut is array (15 downto 0) of std_logic_vector(7 downto 0);
	constant hex_lut : lut := (
		"01000110", "01000101", "01000100", "01000011",
		"01000010", "01000001", "00111001", "00111000",
		"00110111", "00110110", "00110101", "00110100",
		"00110011", "00110010", "00110001", "00110000");
begin
    
	comb_byte : process(byte)
		variable lsb, msb : std_logic_vector(3 downto 0);
	begin
		msb := byte(7 downto 4);
		lsb := byte(3 downto 0);
		hex_1 <= hex_lut(to_integer(unsigned(msb)));
		hex_2 <= hex_lut(to_integer(unsigned(lsb)));
	end process;

	comb_set_numwords_and_checkValid : process(received_cmd, txdone, cur_state)
		variable b1 : std_logic_vector(7 downto 0);
		variable b2, b3, b4 : integer;
		variable b1valid, b2valid, b3valid, b4valid : std_logic;
	begin
		
		b1 := received_cmd(31 downto 24); --letter
		b2 := to_integer(unsigned(received_cmd(23 downto 16))) ; --msb
		b3 :=  to_integer(unsigned(received_cmd(15 downto 8)));
		b4 :=  to_integer(unsigned(received_cmd(7 downto 0)));
		
		
		
		if b1 = "01000001" or b1 = "01100001" then  --'A' or 'a'
			b1valid := '1';
		else
			b1valid := '0';
		end if;
		
		if b2 < 58 and b2 > 47 then    --0 to 9
		  b2valid := '1';
		else
		  b2valid := '0';
		end if;
    
		if b3 < 58 and b3 > 47 then
		  b3valid := '1';
		else
		  b3valid := '0';
		end if;
		
		if b4 < 58 and b4 > 47 then
		  b4valid := '1';
		else
		  b4valid := '0';
		end if;
		
		cmd_is_valid <= (b1valid and b2valid) and (b3valid and b4valid);   --detect the valid cmds
		
		if cur_state = S0 and txdone = '1' then 
		cmd_is_valid <= '0';
		end if;
		
	end process;
	
	proc_state : process(cur_state, txdone, rxData, rxnow, ovErr, framErr, cmd_is_valid, dataReady, seqDone, hex_1, hex_2)
	begin
		rxdone <= '0';
		start <= '0';
		txnow <= '0';
		txData <= "00000000";
		
		case cur_state is
			when S0 =>
				if rxnow = '1' then
					next_state <= S1;
					
				else
					next_state <= S0;
				end if;
			when S1 =>
				--Only assigned after this process is finished, but the change of value of cmd_is_valid will re-trigger this process
				 
				if cmd_is_valid = '1' then
					next_state <= S2;
				else
				    received_cmd <= received_cmd(23 downto 0) & rxData;--Shift contents of recieved_cmd
					rxdone <= '1'; --We have now read the new data
					next_state <= S0;
				end if;
				
				
			when S2 =>    --the state to insert an one cycle high start signal and insert numWords
				start <= '1';  
				numWords_bcd(2) <= received_cmd(19 downto 16);  --Take off the 4 most significant bytes gives the bcd of the ascii
		        numWords_bcd(1) <= received_cmd(11 downto 8);
		        numWords_bcd(0) <= received_cmd(3 downto 0);
				next_state <= S3; 

			
				
			when S3 =>
				if dataReady = '1' and txdone = '1' then
					txData <= hex_1;
					txnow <= '1';
					next_state <= S4;
				else
					next_state <= S3;
				end if;
			
			when S4 =>
			--Waiting for first bit to be transmitted
				if txdone = '1' and seqDone = '0' then
				  next_state <= S5;
				  txData <= hex_2;
				  txnow <= '1';
				elsif seqDone = '1' then
				  next_state <= S6;
				  txData <= hex_2;
				else
				  next_state <= S4;
				  txData <= hex_1;
				end if;
				
			when S5=>
				if txdone = '1' then --Waiting for second bit to be transmitted
					next_state <= S2;
				else
					txData <= hex_2;
					next_state <= S5;
				end if;
				
			when S6 =>
			--transmit second bit, sequence is done
				if txdone = '1' then --Still waiting for first bit to be transmitted
				  next_state <= S7;
				  txData <= hex_2;
				  txnow <= '1';
				else
					next_state <= S6;
					txData <= hex_2;
				end if;
				
			when S7 =>
				if txdone = '1' then --Waiting for second bit to be transmitted
					next_state <= S0;
				else
					next_state <= S7;
				end if;
		
			
			when others =>
				next_state <= S0;
				
			
		end case;
				
	end process;
	
	seq_nextstate : process(clk)
	begin
		if rising_edge(clk) then
			if reset = '1' then
				cur_state <= S0;
			else
				cur_state <= next_state;
			end if;
		end if;
	end process;


end;
  
  