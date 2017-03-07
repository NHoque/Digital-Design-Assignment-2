library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.common_pack.all;

entity dataConsume is
    port(
	clk          : in std_logic;
	reset        : in std_logic; -- synchronous reset
	start        : in std_logic; -- goes high to signal data transfer
	numWords_bcd : in BCD_ARRAY_TYPE(2 downto 0);
	ctrlIn       : in std_logic;
	ctrlOut      : out std_logic;
	data         : in std_logic_vector(7 downto 0);
	dataReady    : out std_logic;
	byte         : out std_logic_vector(7 downto 0);
	seqDone      : out std_logic;
	maxIndex     : out BCD_ARRAY_TYPE(2 downto 0);
	dataResults  : out CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1) -- index 3 holds the peak
  	);
end dataConsume;		

architecture Behavioral of dataConsume is

----------------------------------------------------------- ASSIGN SIGNALS
type state_type is (S0, S1, S2, S3, S4, S5);
signal curState, nextState: state_type;
signal ctrlIn_reg, ctrlIn_edge, ctrlOut_reg, seqDone_reg : STD_LOGIC := '0';
signal byte_reg, data_reg : UNSIGNED(7 downto 0) := X"00";
signal numWords_reg : BCD_ARRAY_TYPE(2 downto 0);
signal index, index_max, numWords_int : INTEGER := 0;
-----------------------------------------------------------
begin

  combi_nextState: process(curState, ctrlIn_edge, numWords_int, start)
  variable numWords_toMax : INTEGER; 
    begin
      numWords_toMax := 0;
      ctrlOut_reg <= '0';
      case curState is
        
        when S0 =>
          seqDone <= '0';
	  if numWords_toMax /= numWords_int AND start = '1' then 
	    ctrlOut_reg <= NOT ctrlOut_reg;
	    nextState <= S1;
	  elsif numWords_toMax /= numWords_int AND start = '0' then
            nextState <= S0;
	  end if;
	
	when S1 => 
	  ctrlOut_reg <= NOT ctrlOut_reg;
	  nextState <= S2;

	when S2 => 
	if ctrlIn_edge = '1' AND numWords_toMax /= numWords_Int then
          numWords_toMax := numWords_toMax + 1;
          ctrlOut_reg <= NOT ctrlOut_reg;
	  nextState <= S2;
	elsif(numWords_toMax = numWords_Int) then
          nextState <= S3;
	else
	  ctrlOut_reg <= NOT ctrlOut_reg; 
	  nextState <= S0;
	end if;
	
	when S3 =>
          seqDone_reg <= '1';
	  ctrlOut_reg <= NOT ctrlOut_reg;
	  nextState <= S0;
	  
        when others =>
          nextState <= S0;
    
      end case;
    end process; 

----------------------------------------------------------------------------
  combi_dataToReg : process(data, ctrlIn_edge)
  begin
    if ctrlIn_edge = '1' then
      data_reg <= UNSIGNED(data);
    else
      data_reg <= data_reg;
    end if;
  end process;
  
----------------------------------------------------------------------------
  -- Takes numWords_bcd and converts it to an integer
  comb_convertNumWordsBCDtoINT : process(numWords_reg)
  variable numWord_0, numWord_1, numWord_2 : UNSIGNED(3 downto 0);
  variable numWord_int0, numWord_int1, numWord_int2, numWords_full_int : INTEGER;
  begin
    numWord_0 := UNSIGNED(numWords_reg(0));
    numWord_1 := UNSIGNED(numWords_reg(1));
    numWord_2 := UNSIGNED(numWords_reg(2));
    numWord_int0 := TO_INTEGER(numWord_0);
    numWord_int1 := TO_INTEGER(numWord_1);
    numWord_int2 := TO_INTEGER(numWord_2);
    numWords_full_int := numWord_int2 * 100 + numWord_int1 * 10 + numWord_int0;
    numWords_int <= numWords_full_int;
  end process;
-----------------------------------------------------------------------------
--  combi_convertIndexINTtoBCD : process(index_max)
--  variable index_BCD_full : UNSIGNED(11 downto 0);
--  variable index_BCD_STD : STD_LOGIC_VECTOR(11 downto 0);
--  variable index_BCD_1, index_BCD_2, index_BCD_3 : STD_LOGIC_VECTOR(3 downto 0);
--  begin
--    index_BCD_full := TO_UNSIGNED(index, 12);
--    index_BCD_STD := STD_LOGIC_VECTOR(index_BCD_full);
--    index_BCD_1 := index_BCD_STD(11 downto 8);
--    index_BCD_2 := index_BCD_STD(7 downto 4);
--    index_BCD_3 := index_BCD_STD(3 downto 0);
--    maxIndex <= (index_BCD_1, index_BCD_2, index_BCD_3);
--  end process;
-----------------------------------------------------------------------------
  -- Main process, will try to find the peak byte.
  combi_detectPeak : process(data_reg)
  variable data_toUnsigned : UNSIGNED(7 downto 0) := X"00";
  variable data_max : UNSIGNED(7 downto 0) := X"00";
  variable shiftValue : INTEGER := -1;
  begin
    data_toUnsigned := UNSIGNED(data_reg);
    if (TO_INTEGER(data_toUnsigned) > TO_INTEGER(data_max)) then
      index <= index + 1;
      index_max <= index;
      data_max := data_reg;
      dataResults(3) <= STD_LOGIC_VECTOR(data_max);
      shiftValue := 0;
    else
      index <= index + 1;
      shiftValue := shiftValue + 1;
      data_max := data_max;
      dataResults((3 + shiftValue) mod 7) <= STD_LOGIC_VECTOR(data_reg);
    end if;
  end process;

--------------------------------------------------------------------------------
  -- Once start is asserted, output data to command processor.
  comb_outputData : process(start, data_reg)
  begin
    dataReady <= '0';
    if start = '1' then
      byte_reg <= data_reg;
      dataReady <= '1';      
    else
      byte_reg <= byte_reg;
      dataReady <= '0';      
    end if;
  end process;
--------------------------------------------------------------------------------

  seq_reg_ctrlIn : process(clk)
  begin
    if RISING_EDGE(clk) then
      ctrlIn_reg <= ctrlIn;
    end if;
  end process;

  ctrlIn_edge <= ctrlIn XOR ctrlIn_reg;

------------------------------------------------------------------------------
  -- On reset, all outputs are set to zero.
  -- Else, data sent to a signal, and index updated.
  seq_update: process(clk)
  begin
    if RISING_EDGE(clk) then
      if reset = '1' then
        curState <= S0;
        --index <= 0;
        --ctrlOut_reg <= '1';
        --dataReady <= '0';
        --byte <= X"00";
        --maxIndex <= (X"0", X"0", X"0");
        --dataResults <= (X"00", X"00", X"00", X"00", X"00", X"00", X"00"); 
        --seqDone <= '0';
      else
        numWords_reg <= numWords_bcd;
 	curState <= nextState;
 	--if ctrlIn_edge = '1' then
          --ctrlOut_reg <= NOT ctrlOut_reg;
          --data_reg <= UNSIGNED(data);
          --index <= index + 1;
        --else
          --ctrlOut_reg <= ctrlOut_reg;
          --data_reg <= data_reg;
          --index <= index;
        --end if;
      end if;
    end if;
  end process;
  
  seqDone <= seqDone_reg;
  ctrlOut <= ctrlOut_reg;
  byte <= STD_LOGIC_VECTOR(byte_reg);

end Behavioral;
