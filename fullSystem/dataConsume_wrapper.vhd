----------------------------------------------------------------------------
--	dataConsume_wrapper.vhd -- Wrapper for cmd processor that instantiates the 
-- the synethesised structural version of the comd processor
----------------------------------------------------------------------------
-- Author:  Dinesh Pamunuwa
----------------------------------------------------------------------------
--
----------------------------------------------------------------------------
-- This is a wrapper that instantiates the Xilinx synthesised structural 
-- version of the data processor that has a slightly different entity
-- port description than the original specifications. 
-- It has some conversion functions that translate different array types 
-- and instantiates the synthesised version with the correct entity port
-- definitions to implement the command processor funtionaliy.
----------------------------------------------------------------------------
--
----------------------------------------------------------------------------
-- Version:			1.0
-- Revision History:
--  09/02/2014 (Dinesh): Created using Modelsim
----------------------------------------------------------------------------
library ieee;
use work.common_pack.all;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned."-";

library UNISIM;
use UNISIM.VPKG.ALL;

entity dataConsume is
  port (
    clk:		in std_logic;
    reset:		in std_logic; -- synchronous reset
    start: in std_logic; -- goes high to signal data transfer
    numWords_bcd: in BCD_ARRAY_TYPE(2 downto 0);
    ctrlIn: in std_logic;
    ctrlOut: out std_logic;
    data: in std_logic_vector(7 downto 0);
    dataReady: out std_logic;
    byte: out std_logic_vector(7 downto 0);
    seqDone: out std_logic;
    maxIndex: out BCD_ARRAY_TYPE(2 downto 0);
    dataResults: out CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1) -- index 3 holds the peak
  );
end dataConsume;
	
architecture structural of dataConsume is
	
  component dataConsume_synthesised is
    port (
      clk : in STD_LOGIC := 'X'; 
      reset : in STD_LOGIC := 'X'; 
      start : in STD_LOGIC := 'X'; 
      ctrlIn : in STD_LOGIC := 'X'; 
      ctrlOut : out STD_LOGIC; 
      dataReady : out STD_LOGIC; 
      seqDone : out STD_LOGIC; 
      numWords_bcd : in STD_LOGIC_VECTOR2 ( 2 downto 0 , 3 downto 0 ); 
      data : in STD_LOGIC_VECTOR ( 7 downto 0 ); 
      byte : out STD_LOGIC_VECTOR ( 7 downto 0 ); 
      maxIndex : out STD_LOGIC_VECTOR2 ( 2 downto 0 , 3 downto 0 ); 
      dataResults : out STD_LOGIC_VECTOR2 ( 6 downto 0 , 7 downto 0 ) 
    );
  end component;
	
	function conv_2d_BCD_ARRAY (a_2d: in STD_LOGIC_VECTOR2) return BCD_ARRAY_TYPE is
    variable result: BCD_ARRAY_TYPE(BCD_INDICES-1 downto 0);
  begin
    for i in result'range loop
      for j in a_2d'range(2) loop
        result(i)(j) := a_2d(i, j);
      end loop;
    end loop;
    return result;
  end;  
  
 	function conv_2d_CHAR_ARRAY (a_2d: in STD_LOGIC_VECTOR2) return CHAR_ARRAY_TYPE is
    variable result: CHAR_ARRAY_TYPE(RESULT_BYTE_NUM-1 downto 0);
  begin
    for i in result'range loop
      for j in a_2d'range(2) loop
        result(i)(j) := a_2d(i, j);
      end loop;
    end loop;
    return result;
  end;   
  
  function conv_BCD_ARRAY_2d (a_BCD_ARRAY: in BCD_ARRAY_TYPE) return std_logic_vector2 is
      variable result: std_logic_vector2(a_BCD_ARRAY'range, BCD_WORD_LENGTH-1 downto 0);
  begin
    for i in result'range(1) loop
      for j in result'range(2) loop
        result(i,j) := a_BCD_ARRAY(i)(j);
      end loop;
    end loop;
    return result;
  end;
  
  function conv_CHAR_ARRAY_2d (a_CHAR_ARRAY: in CHAR_ARRAY_TYPE) return std_logic_vector2 is
      variable result: std_logic_vector2(a_CHAR_ARRAY'range, WORD_LENGTH-1 downto 0);
  begin
    for i in result'range(1) loop
      for j in result'range(2) loop
        result(i,j) := a_CHAR_ARRAY(i)(j);
      end loop;
    end loop;
    return result;
  end;
	
	signal sig_maxIndex_xilinx: STD_LOGIC_VECTOR2 ( 2 downto 0 , 3 downto 0 ); 
	signal sig_dataResults_xilinx : STD_LOGIC_VECTOR2 ( 6 downto 0 , 7 downto 0 ); 
  signal sig_numWords_bcd_xilinx : STD_LOGIC_VECTOR2 ( 2 downto 0 , 3 downto 0 ); 
	

begin 
  
  process(sig_maxIndex_xilinx,sig_dataResults_xilinx,numWords_bcd)
  begin
    maxIndex <= conv_2d_BCD_ARRAY(sig_maxIndex_xilinx);
    dataResults <= conv_2d_CHAR_ARRAY(sig_dataResults_xilinx);
    sig_numWords_bcd_xilinx <= conv_BCD_ARRAY_2d(numWords_bcd);
  end process;

  dataConsume_s1: dataConsume_synthesised
      port map (
        clk => clk,
        reset => reset,
        start => start,
        numWords_bcd => sig_numWords_bcd_xilinx,
        ctrlIn => ctrlIn,
        ctrlOut => ctrlOut,
        dataReady => dataReady,
        byte => byte,
        data => data,
        seqDone => seqDone,
        maxIndex => sig_maxIndex_xilinx,
        dataResults => sig_dataResults_xilinx
     );

end structural;
