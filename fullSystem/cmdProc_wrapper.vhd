----------------------------------------------------------------------------
--	cmdProc_wrapper.vhd -- Wrapper for cmd processor that instantiates the 
-- the synethesised structural version of the comd processor
----------------------------------------------------------------------------
-- Author:  Dinesh Pamunuwa
----------------------------------------------------------------------------
--
----------------------------------------------------------------------------
-- This is a wrapper that instantiates the Xilinx synthesised structural 
-- version of the command processor that has a slightly different entity
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

entity cmdProc is
	port (
		clk:		in std_logic;
		reset:		in std_logic;
		rxnow:		in std_logic;
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
end cmdProc;
	
architecture structural of cmdProc is
	
  component cmdProc_synthesised is
      port (
        clk : in STD_LOGIC := 'X'; 
        reset : in STD_LOGIC := 'X'; 
        rxnow : in STD_LOGIC := 'X'; 
        ovErr : in STD_LOGIC := 'X'; 
        framErr : in STD_LOGIC := 'X'; 
        txdone : in STD_LOGIC := 'X'; 
        dataReady : in STD_LOGIC := 'X'; 
        seqDone : in STD_LOGIC := 'X'; 
        rxdone : out STD_LOGIC; 
        txnow : out STD_LOGIC; 
        start : out STD_LOGIC; 
        rxData : in STD_LOGIC_VECTOR ( 7 downto 0 ); 
        byte : in STD_LOGIC_VECTOR ( 7 downto 0 ); 
        maxIndex : in STD_LOGIC_VECTOR2 ( 2 downto 0 , 3 downto 0 ); 
        dataResults : in STD_LOGIC_VECTOR2 ( 6 downto 0 , 7 downto 0 ); 
        txData : out STD_LOGIC_VECTOR ( 7 downto 0 ); 
        numWords_bcd : out STD_LOGIC_VECTOR2 ( 2 downto 0 , 3 downto 0 ) 
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
  
  process(maxIndex,dataResults,sig_numWords_bcd_xilinx)
  begin
    sig_maxIndex_xilinx <= conv_BCD_ARRAY_2d(maxIndex);
    sig_dataResults_xilinx <= conv_CHAR_ARRAY_2d(dataResults);
    numWords_bcd <= conv_2d_BCD_ARRAY(sig_numWords_bcd_xilinx);
  end process;
 
	
	cmdProc_s1: cmdProc_synthesised
    port map (
      clk => clk,
      reset => reset,
      rxNow => rxNow,
      rxData => rxData,
      txData => txData,
      rxDone => rxDone,
      ovErr => ovErr,
      framErr => framErr,
      txNow => txNow,
      txDone => txDone,
      start => start,
      numWords_bcd => sig_numWords_bcd_xilinx,
      dataReady => dataReady,
      byte => byte,
      maxIndex => sig_maxIndex_xilinx,
      seqDone => seqDone,
      dataResults => sig_dataResults_xilinx
    );

end structural;
