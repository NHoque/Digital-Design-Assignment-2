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
        dataResults  : out CHAR_ARRAY_TYPE(0 to 6) -- index 3 holds the peak
        );
end dataConsume;    

architecture Behavioral of dataConsume is

----------------------------------------------------------- ASSIGN SIGNALS
signal ctrlIn_reg, ctrlIn_edge, ctrlOut_reg, seqDone_reg, dataReady_reg : STD_LOGIC := '0';
signal byte_reg, data_reg : UNSIGNED(7 downto 0);
signal numWords_reg, maxIndex_reg : BCD_ARRAY_TYPE(2 downto 0);
signal dataResults_reg : CHAR_ARRAY_TYPE(0 to 6) := (X"00", X"00", X"00", X"00", X"00", X"00", X"00");
signal index, index_max, numWords_int : INTEGER := 0;
-----------------------------------------------------------
begin
----------------------------------------------------------------------------
-- by
-- https://en.wikipedia.org/wiki/Double_dabble
  convertIndexINTtoBCD: process(index_max)
  variable index_hold : STD_LOGIC_VECTOR (11 downto 0);
  variable index_bcd : UNSIGNED (15 downto 0) := (others => '0');
  variable index_ones, index_tens, index_hundreds : STD_LOGIC_VECTOR(3 downto 0) := X"0";
  begin
    index_bcd := X"0000";
    index_hold(11 downto 0) := STD_LOGIC_VECTOR(TO_UNSIGNED(index_max, 12));
    
    for i in 0 to 11 loop
      if index_bcd(3 downto 0) > 4 then 
        index_bcd(3 downto 0) := index_bcd(3 downto 0) + 3;
      end if;

      if index_bcd(7 downto 4) > 4 then 
        index_bcd(7 downto 4) := index_bcd(7 downto 4) + 3;
      end if;

      if index_bcd(11 downto 8) > 4 then  
        index_bcd(11 downto 8) := index_bcd(11 downto 8) + 3;
      end if;

      index_bcd := index_bcd(14 downto 0) & index_hold(11);
      index_hold := index_hold(10 downto 0) & '0';
    end loop;
    
    index_ones := STD_LOGIC_VECTOR(index_bcd(3 downto 0));
    index_tens := STD_LOGIC_VECTOR(index_bcd(7 downto 4));
    index_hundreds := STD_LOGIC_VECTOR(index_bcd(11 downto 8));
    maxIndex_reg <= (index_hundreds, index_tens, index_ones);
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
  -- Main process, will try to find the peak byte.
  combi_detectPeak : process(byte_reg, seqDone_reg, dataReady_reg)
  variable data_max : UNSIGNED(7 downto 0) := X"00";
  variable holdValues : CHAR_ARRAY_TYPE(0 to 6) := (others => X"00");
  variable shiftValue : INTEGER := 0;
  begin
    
    if seqDone_reg = '1' OR dataReady_reg = '0' then
      data_max := X"00";
      dataResults_reg <= (others => X"00");
      holdValues := (others => X"00");
      index <= 0;
    elsif dataReady_reg = '1' then
      index <= index + 1;
      shiftValue := shiftValue + 1;
      holdValues := holdValues(1 to 6) & STD_LOGIC_VECTOR(byte_reg); 
    end if;

    if (TO_INTEGER(byte_reg) >= TO_INTEGER(data_max)) then
      index_max <= index;
      data_max := byte_reg;
      dataResults_reg(3) <= STD_LOGIC_VECTOR(data_max);
      dataResults_reg(2) <= holdValues(5);
      dataResults_reg(1) <= holdValues(4);
      dataResults_reg(0) <= holdValues(3);
      shiftValue := 0;
    elsif (TO_INTEGER(byte_reg) < TO_INTEGER(data_max)) then
      data_max := data_max;
      if ((3 + shiftValue) mod 7 > 3) AND (shiftValue < 4) then
        dataResults_reg((3 + shiftValue) mod 7) <= STD_LOGIC_VECTOR(byte_reg);
      end if;
    end if;

  end process;
---------------------------------------------------------------------------------
  combi_reg_byte : process(start, data_reg)
  begin
    if start = '1' then
      byte_reg <= data_reg;
      dataReady_reg <= '1';      
    else
      byte_reg <= byte_reg;
      dataReady_reg <= '0';      
    end if;
  end process;
---------------------------------------------------------------------------------
  combi_reg_data : process(ctrlIn_edge)
  begin
    if ctrlIn_edge = '1' then
      data_reg <= UNSIGNED(data);
    else
      data_reg <= data_reg;
    end if;
  end process;
---------------------------------------------------------------------------------
  combi_seqDone : process(index, numWords_int)
  begin
    seqDone_reg <= '0';
    if index = numWords_int then
      seqDone_reg <= '1';
    elsif index /= numWords_int then
      seqDone_reg <= '0';
    end if;
  end process;
---------------------------------------------------------------------------------
  -- On reset, all outputs are set to zero except ctrlOut_reg 
  -- which is set to 1 to enable data acquisition.
  -- Else, run control lines and indexing.
  seq_runSystem: process(clk)
  begin
    if rising_edge(clk) then
      if reset = '1' then
        ctrlOut_reg <= '1';
      else
        ctrlIn_reg <= ctrlIn;
        if ctrlIn_edge = '1' then
          ctrlOut_reg <= NOT ctrlOut_reg;
        end if;  
      end if;
    end if;
  end process;
  
  ctrlIn_edge <= ctrlIn XOR ctrlIn_reg;
  
  ctrlOut <= ctrlOut_reg;
  
  dataReady <= dataReady_reg;
  maxIndex <= maxIndex_reg;
  numWords_reg <= numWords_bcd;
  byte <= STD_LOGIC_VECTOR(byte_reg);
  dataResults <= dataResults_reg;
  seqDone <= seqDone_reg;
  
end Behavioral;

