library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.common_pack.all;

entity dataConsume is
    Port ( clk          : in  STD_LOGIC;
           reset        : in  STD_LOGIC;
           ctrlIn       : in  STD_LOGIC;
           ctrlOut      : out  STD_LOGIC;
           data         : in  STD_LOGIC_VECTOR(7 downto 0);
           start        : in  STD_LOGIC;
           numWords_bcd : in  BCD_ARRAY_TYPE(2 downto 0);
           dataReady    : out  STD_LOGIC;
           byte         : out  STD_LOGIC_VECTOR(7 downto 0);
           maxIndex     : out  BCD_ARRAY_TYPE(2 downto 0);
           dataResults  : out  CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
           seqDone      : out  STD_LOGIC);
end dataConsume;

architecture Behavioral of dataConsume is

----------------------------------------------------------- ASSIGN SIGNALS
type state_type is (S0, S1, S2, S3, S4, S5);
signal curState, nextState: state_type;
signal ctrlIn_reg, ctrlIn_edge, ctrlOut_reg : STD_LOGIC;
signal byte_reg, data_reg, data_max : UNSIGNED(7 downto 0);
signal numWords_reg : BCD_ARRAY_TYPE(2 downto 0);
signal index, index_max, numWords_int : INTEGER;
-----------------------------------------------------------

begin

  ------------------------
  -- TODO: FINDING PEAK --
  ------------------------

--  combi_nextState: process(curState)
--  begin
--    case curState is
--      when S0 =>
--      
--      when others =>
--        nextState <= S0;
--    
--    end case;
--  end process; 


  -- Takes numWords_bcd and converts it to an integer
  convertNumWordsBCDtoINT : process(numWords_reg)
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


  -- Main process, will try to find the peak byte.
  checkMaxByte : process(data_reg, data_max)
  variable data_int, data_max_int : INTEGER;
  variable toUnsigned : UNSIGNED(11 downto 0);
  begin
    data_int := TO_INTEGER(data_reg);
    data_max_int := TO_INTEGER(data_max);
    if (data_int > data_max_int) then
      data_max <= data_reg;
      index_max <= index;
      toUnsigned := TO_UNSIGNED(index_max, 12);
      maxIndex <= (STD_LOGIC_VECTOR(toUnsigned(11 downto 8)), STD_LOGIC_VECTOR(toUnsigned(7 downto 4)), STD_LOGIC_VECTOR(toUnsigned(3 downto 0)));
    else
      data_max <= data_max;
      index_max <= index_max;
    end if;
  end process;


  -- Once start is asserted, output data to command processor.
  outputData : process(start)
  begin
    if start = '1' then
      byte_reg <= data_reg;
      dataReady <= '1';      
    else
      byte_reg <= byte_reg;
      dataReady <= '0';      
    end if;
  end process;

  reg_ctrlIn : process(clk)
  begin
    if RISING_EDGE(clk) then
      ctrlIn_reg <= ctrlIn;
    end if;
  end process;

  ctrlIn_edge <= ctrlIn XOR ctrlIn_reg;

  -- On reset, all outputs are set to zero.
  -- Else, data sent to a signal, and index updated.
  reset_system: process(clk, reset)
  begin
    if RISING_EDGE(clk) then
      if reset = '1' then
        index <= 0;
        ctrlOut_reg <= '0';
        dataReady <= '0';
        byte <= X"00";
        maxIndex <= (X"0", X"0", X"0");
        seqDone <= '0';
      else
        numWords_reg <= numWords_bcd;
        if ctrlIn_edge = '1' then
          ctrlOut_reg <= NOT ctrlOut_reg;
          data_reg <= UNSIGNED(data);
          index <= index + 1;
        end if;
      end if;
    end if;
  end process;

  
  ctrlOut <= ctrlOut_reg;
  byte <= STD_LOGIC_VECTOR(byte_reg);

end Behavioral;

