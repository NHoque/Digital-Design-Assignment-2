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
type STATE_TYPE is (S0, S1, S2, S3);
signal curState, nextState : STATE_TYPE;
signal ctrlIn_reg, ctrlIn_edge, ctrlOut_reg, seqDone_reg, dataReady_reg : STD_LOGIC := '0';
signal data_reg, byte_reg : UNSIGNED(7 downto 0);
signal numWords_reg, maxIndex_reg : BCD_ARRAY_TYPE(2 downto 0) := (others => X"0");
signal dataResults_reg : CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1) := (others => X"00");
signal index, index_max, numWords_int : INTEGER;
-----------------------------------------------------------
begin
----------------------------------------------------------------------------
-- by
-- https://en.wikipedia.org/wiki/Double_dabble
  combi_convertIndexINTtoBCD: process(index_max)
  variable index_hold : STD_LOGIC_VECTOR (11 downto 0);
  variable index_bcd : UNSIGNED (15 downto 0);
  variable index_ones, index_tens, index_hundreds : STD_LOGIC_VECTOR(3 downto 0);
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
    maxIndex <= (index_hundreds, index_tens, index_ones);
  end process;
----------------------------------------------------------------------------
  -- Takes numWords_bcd and converts it to an integer
  combi_convertNumWordsBCDtoINT : process(numWords_reg)
  variable numWord_0, numWord_1, numWord_2, numWords_full_int : INTEGER;
  begin
    numWord_0 := TO_INTEGER(UNSIGNED(numWords_reg(0)));
    numWord_1 := TO_INTEGER(UNSIGNED(numWords_reg(1)));
    numWord_2 := TO_INTEGER(UNSIGNED(numWords_reg(2)));
    numWords_int <= numWord_2 * 100 + numWord_1 * 10 + numWord_0;
  end process;
-----------------------------------------------------------------------------
  nextStateLogic: process(curState, start, numWords_int, index)
  begin
    case curState is
    when S0 =>
      nextState <= S1;

    when S1 =>
      if start = '1' then
        nextState <= S2;
      else
        nextState <= S1;
      end if;

    when S2 =>
      if index = numWords_int then
        nextState <= S3;
      elsif index < numWords_int then
        nextState <= S1;
      else
        nextState <= S2;
      end if;

    when S3 =>
      nextState <= S0;
    end case;
  end process;
-----------------------------------------------------------------------------
  seq_setSeqDone : process(clk, curState)
    begin
      if rising_edge(clk) then
        if curState = S0 then
          seqDone_reg <= '0';
        elsif curState = S3 then
          seqDone_reg <= '1';
        end if;
      end if;
    end process;
-----------------------------------------------------------------------------
  seq_setDataReady : process(clk)
    begin
      if rising_edge(clk) then
        if start = '1' then
          dataReady_reg <= '1';
        else
				  dataReady_reg <= '0';
        end if;
      end if;
    end process;
-----------------------------------------------------------------------------
  seq_detectPeak : process(clk, curState)
  variable data_max : UNSIGNED(7 downto 0) := X"00";
  variable holdValues : CHAR_ARRAY_TYPE(0 to 6) := (others => X"00");
  variable shiftValue : INTEGER := 0;
    begin
      if rising_edge(clk) then
        if curState = S0 then
          shiftValue := 0;
          holdValues := (others => X"00");
          dataResults_reg <= (others => X"00");
          index_max <= 0;
          data_max := X"00";
          byte_reg <= X"00";
        elsif curState = S2 then
          shiftValue := shiftValue + 1;
          holdValues := holdValues(1 to 6) & STD_LOGIC_VECTOR(data_reg);
          if (data_reg >= data_max) then
            index_max <= index;
            data_max := data_reg;
            --byte_reg <= data_max;
            dataResults_reg(0 to 2) <= holdValues(3 to 5);
            dataResults_reg(3) <= STD_LOGIC_VECTOR(data_reg);
            dataResults_reg(4 to 6) <= (others => X"00");
            shiftValue := 0;
          else
            index_max <= index_max;
            data_max := data_max;
            dataResults_reg(0 to 3) <= dataResults_reg(0 to 3);
            if(shiftValue < 4) then
              dataResults_reg(4 to 6) <= dataResults_reg(5 to 6) & STD_LOGIC_VECTOR(data_reg);
            else
              dataResults_reg <= dataResults_reg;
            end if;
          end if;
					byte_reg <= data_reg;
        end if;
      end if;
    end process;
-----------------------------------------------------------------------------
  -- On reset, all outputs are set to zero except ctrlOut_reg
  -- which is set to 1 to enable data acquisition.
  -- Else, run control lines and indexing.
  seq_runSystem: process(clk, reset)
  begin
    if rising_edge(clk) then
      if reset = '1' then
        ctrlOut_reg <= '1';
        index <= 0;
        curState <= S0;
      else
        curState <= nextState;
        ctrlIn_reg <= ctrlIn;
        if ctrlIn_edge = '1' then
          if index = numWords_int then
            index <= 0;
          elsif start = '1' then
            index <= index + 1;
          end if;
          data_reg <= UNSIGNED(data);
          ctrlOut_reg <= NOT ctrlOut_reg;
        end if;

      end if;
    end if;
  end process;

  ctrlIn_edge <= ctrlIn XOR ctrlIn_reg;

  ctrlOut <= ctrlOut_reg;
  numWords_reg <= numWords_bcd;
  dataReady <= dataReady_reg;
  byte <= STD_LOGIC_VECTOR(byte_reg);
  seqDone <= seqDone_reg;
  dataResults <= dataResults_reg;

end Behavioral;
