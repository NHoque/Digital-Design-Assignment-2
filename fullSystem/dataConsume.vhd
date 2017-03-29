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
signal index_bcd, maxIndex_reg : BCD_ARRAY_TYPE(2 downto 0) := (others => X"0");
signal dataResults_reg : CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1) := (others => X"00");
-----------------------------------------------------------
begin
-----------------------------------------------------------------------------
  nextStateLogic: process(curState, start, numWords_bcd, index_bcd)
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
      if index_bcd(0) = numWords_bcd(0) AND index_bcd(1) = numWords_bcd(1) AND index_bcd(2) = numWords_bcd(2) then
        nextState <= S3;
      elsif index_bcd(0) /= numWords_bcd(0) AND index_bcd(1) /= numWords_bcd(1) AND index_bcd(2) /= numWords_bcd(2) then
        nextState <= S1;
			else
			  nextState <= S1;
      end if;

    when S3 =>
      nextState <= S0;
    end case;
  end process;
-----------------------------------------------------------------------------
  seq_setSeqDone : process(clk, curState)
    begin
      if rising_edge(clk) then
        if curState = S3 then
          seqDone_reg <= '1';
        else
          seqDone_reg <= '0';
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
  variable holdValues : CHAR_ARRAY_TYPE(0 to 3) := (others => X"00");
  variable shiftValue : UNSIGNED(2 downto 0) := "000";
    begin
      if rising_edge(clk) then
        if curState = S0 then
          shiftValue := "000";
          holdValues := (others => X"00");
          dataResults_reg <= (others => X"00");
          maxIndex_reg <= (others => X"0");
          data_max := X"00";
          byte_reg <= X"00";
        elsif curState = S2 then
          shiftValue := shiftValue + 1;
          holdValues := holdValues(1 to 3) & STD_LOGIC_VECTOR(data_reg);
          if (data_reg >= data_max) then
            maxIndex_reg <= index_bcd;
            data_max := data_reg;
            dataResults_reg(0 to 2) <= holdValues(0 to 2);
            dataResults_reg(3) <= STD_LOGIC_VECTOR(data_reg);
            dataResults_reg(4 to 6) <= (others => X"00");
            shiftValue := "000";
          else
            maxIndex_reg <= maxIndex_reg;
            data_max := data_max;
            dataResults_reg(0 to 3) <= dataResults_reg(0 to 3);
            if (shiftValue < "100") then
              dataResults_reg(4 to 6) <= dataResults_reg(5 to 6) & STD_LOGIC_VECTOR(data_reg);
            else
							shiftValue := "100";
              dataResults_reg <= dataResults_reg;
            end if;
          end if;
					byte_reg <= data_reg;
        end if;
      end if;
    end process;
-----------------------------------------------------------------------------
  seq_indexCountBCD: process(clk)
  variable index_hundreds, index_tens, index_ones : UNSIGNED(3 downto 0);
  begin
	  if rising_edge(clk) then
		  if reset = '1' OR seqDone_reg = '1' then
				index_hundreds := X"0";
				index_tens := X"0";
				index_ones := X"0";
			elsif start = '1' AND ctrlIn_edge = '1' then
			  index_ones := index_ones + 1;
			  if index_ones = "1010" then
				  index_ones := "0000";
					index_tens := index_tens + 1;
				  if index_tens = "1010" then
					  index_tens := "0000";
						index_hundreds := index_hundreds + 1;
					end if;
				end if;
			end if;
			index_bcd <= (STD_LOGIC_VECTOR(index_hundreds), STD_LOGIC_VECTOR(index_tens), STD_LOGIC_VECTOR(index_ones));
		end if;
  end process;
----------------------------------------------------------------------------
  seq_runSystem: process(clk)
  begin
    if rising_edge(clk) then
      if reset = '1' then
				ctrlIn_reg <= '0';
				data_reg <= X"00";
        ctrlOut_reg <= '1';
        curState <= S0;
      else
        curState <= nextState;
        ctrlIn_reg <= ctrlIn;
        if ctrlIn_edge = '1' then
          ctrlOut_reg <= NOT ctrlOut_reg;
					if start = '1' then
						data_reg <= UNSIGNED(data);
					end if;
        end if;
      end if;
    end if;
  end process;

  ctrlIn_edge <= ctrlIn XOR ctrlIn_reg;
  ctrlOut <= ctrlOut_reg;
	maxIndex <= maxIndex_reg;
  dataReady <= dataReady_reg;
  byte <= STD_LOGIC_VECTOR(byte_reg);
  seqDone <= seqDone_reg;
  dataResults <= dataResults_reg;

end Behavioral;
