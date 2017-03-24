--------------------------------------------------------------------------------
-- Copyright (c) 1995-2012 Xilinx, Inc.  All rights reserved.
--------------------------------------------------------------------------------
--   ____  ____
--  /   /\/   /
-- /___/  \  /    Vendor: Xilinx
-- \   \   \/     Version: P.15xf
--  \   \         Application: netgen
--  /   /         Filename: cmdProc_synthesis.vhd
-- /___/   /\     Timestamp: Mon Feb 03 15:39:59 2014
-- \   \  /  \ 
--  \___\/\___\
--             
-- Command	: -intstyle ise -tb -ti UUT -ar Structure -tm cmdProc -w -dir netgen/synthesis -ofmt vhdl -sim cmdProc.ngc cmdProc_synthesis.vhd 
-- Device	: xc6slx16-3-csg324
-- Input file	: cmdProc.ngc
-- Output file	: \\ads.bris.ac.uk\filestore\myfiles\staff\eezgz\Documents\EENG28010\Source Code\peakdetector\Projects\cmdProc1\netgen\synthesis\cmdProc_synthesis.vhd
-- # of Entities	: 1
-- Design Name	: cmdProc
-- Xilinx	: C:\Xilinx\14.1\ISE_DS\ISE\
--             
-- Purpose:    
--     This VHDL netlist is a verification model and uses simulation 
--     primitives which may not represent the true implementation of the 
--     device, however the netlist is functionally correct and should not 
--     be modified. This file cannot be synthesized and should only be used 
--     with supported simulation tools.
--             
-- Reference:  
--     Command Line Tools User Guide, Chapter 23
--     Synthesis and Simulation Design Guide, Chapter 6
--             
--------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
use UNISIM.VPKG.ALL;

entity cmdProc_synthesised is
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
end cmdProc_synthesised;

architecture Structure of cmdProc_synthesised is
  signal dataComplete_158 : STD_LOGIC; 
  signal byteNum_3_7_Q : STD_LOGIC; 
  signal byteNum_3_6_Q : STD_LOGIC; 
  signal byteNum_3_4_Q : STD_LOGIC; 
  signal byteNum_3_3_Q : STD_LOGIC; 
  signal byteNum_3_2_Q : STD_LOGIC; 
  signal byteNum_3_1_Q : STD_LOGIC; 
  signal byteNum_3_0_Q : STD_LOGIC; 
  signal dataStarted_194 : STD_LOGIC; 
  signal lastByte_195 : STD_LOGIC; 
  signal NlwRenamedSig_OI_curState_FSM_FFd9 : STD_LOGIC; 
  signal curState_FSM_FFd6_201 : STD_LOGIC; 
  signal curState_FSM_FFd3_202 : STD_LOGIC; 
  signal curState_FSM_FFd13_203 : STD_LOGIC; 
  signal curState_FSM_FFd11_204 : STD_LOGIC; 
  signal curState_FSM_FFd8_205 : STD_LOGIC; 
  signal curState_FSM_FFd2_206 : STD_LOGIC; 
  signal NlwRenamedSig_OI_curState_FSM_FFd17 : STD_LOGIC; 
  signal reset_clearCmd_OR_20_o : STD_LOGIC; 
  signal GND_8_o_PWR_8_o_OR_16_o : STD_LOGIC; 
  signal Q_n0472_inv : STD_LOGIC; 
  signal Mcount_nibbleCount_val_220 : STD_LOGIC; 
  signal curState_FSM_FFd14_In2 : STD_LOGIC; 
  signal curState_FSM_FFd3_In1_225 : STD_LOGIC; 
  signal curState_FSM_FFd9_In1 : STD_LOGIC; 
  signal curState_FSM_FFd6_In1_227 : STD_LOGIC; 
  signal curState_FSM_FFd18_In : STD_LOGIC; 
  signal curState_FSM_FFd15_In : STD_LOGIC; 
  signal curState_FSM_FFd13_In : STD_LOGIC; 
  signal curState_FSM_FFd12_In : STD_LOGIC; 
  signal curState_FSM_FFd11_In : STD_LOGIC; 
  signal curState_FSM_FFd10_In : STD_LOGIC; 
  signal curState_FSM_FFd8_In : STD_LOGIC; 
  signal curState_FSM_FFd7_In : STD_LOGIC; 
  signal curState_FSM_FFd4_In : STD_LOGIC; 
  signal curState_FSM_FFd2_In : STD_LOGIC; 
  signal curState_FSM_FFd1_In : STD_LOGIC; 
  signal curState_FSM_FFd18_239 : STD_LOGIC; 
  signal curState_FSM_FFd16_240 : STD_LOGIC; 
  signal curState_FSM_FFd15_241 : STD_LOGIC; 
  signal curState_FSM_FFd14_242 : STD_LOGIC; 
  signal curState_FSM_FFd12_243 : STD_LOGIC; 
  signal curState_FSM_FFd10_244 : STD_LOGIC; 
  signal curState_FSM_FFd7_245 : STD_LOGIC; 
  signal curState_FSM_FFd5_246 : STD_LOGIC; 
  signal curState_FSM_FFd4_247 : STD_LOGIC; 
  signal curState_FSM_FFd1_248 : STD_LOGIC; 
  signal Mcount_peakByteCount1 : STD_LOGIC; 
  signal Mcount_peakByteCount2 : STD_LOGIC; 
  signal Mcount_byteCount1 : STD_LOGIC; 
  signal Mcount_byteCount2 : STD_LOGIC; 
  signal Mcount_ctrlByteCount_val : STD_LOGIC; 
  signal Result_1_1 : STD_LOGIC; 
  signal Result_2_1 : STD_LOGIC; 
  signal txdone_0 : STD_LOGIC; 
  signal reg_dataResults_3_7_PWR_8_o_LessThan_203_o : STD_LOGIC; 
  signal reg_dataResults_3_3_PWR_8_o_LessThan_207_o : STD_LOGIC; 
  signal Msub_GND_8_o_GND_8_o_sub_187_OUT_3_0_lut_3_Q : STD_LOGIC; 
  signal n0366_5_Q : STD_LOGIC; 
  signal Msub_GND_8_o_GND_8_o_sub_187_OUT_3_0_cy_0_Q : STD_LOGIC; 
  signal n0366_6_Q : STD_LOGIC; 
  signal n0366_2_Q : STD_LOGIC; 
  signal Msub_GND_8_o_GND_8_o_sub_197_OUT_3_0_lut_3_Q : STD_LOGIC; 
  signal n0366_1_Q : STD_LOGIC; 
  signal Msub_GND_8_o_GND_8_o_sub_197_OUT_3_0_cy_0_Q : STD_LOGIC; 
  signal GND_8_o_PWR_8_o_LessThan_182_o : STD_LOGIC; 
  signal GND_8_o_PWR_8_o_LessThan_192_o : STD_LOGIC; 
  signal byte_7_PWR_8_o_LessThan_170_o : STD_LOGIC; 
  signal byte_3_PWR_8_o_LessThan_174_o : STD_LOGIC; 
  signal Mmux_byteNum_0_7_GND_8_o_mux_218_OUT311 : STD_LOGIC; 
  signal Q_n0435 : STD_LOGIC; 
  signal printResults_INV_61_o : STD_LOGIC; 
  signal printPeak_INV_62_o : STD_LOGIC; 
  signal Mmux_byteNum_0_7_GND_8_o_mux_218_OUT312 : STD_LOGIC; 
  signal Mmux_cmdValid_startSeq14_300 : STD_LOGIC; 
  signal n0023 : STD_LOGIC; 
  signal cmdValid_startSeq : STD_LOGIC; 
  signal GND_8_o_peakByteCount_2_equal_201_o : STD_LOGIC; 
  signal n0015 : STD_LOGIC; 
  signal GND_8_o_GND_8_o_OR_71_o : STD_LOGIC; 
  signal GND_8_o_GND_8_o_OR_70_o_306 : STD_LOGIC; 
  signal curState_FSM_FFd3_In11 : STD_LOGIC; 
  signal Mmux_txData511_308 : STD_LOGIC; 
  signal N01 : STD_LOGIC; 
  signal Mmux_n03666 : STD_LOGIC; 
  signal Mmux_n036661_311 : STD_LOGIC; 
  signal Mmux_n03667 : STD_LOGIC; 
  signal Mmux_n036671_313 : STD_LOGIC; 
  signal Mmux_n03668 : STD_LOGIC; 
  signal Mmux_n036681_315 : STD_LOGIC; 
  signal Mmux_n03661 : STD_LOGIC; 
  signal Mmux_n036611_317 : STD_LOGIC; 
  signal Mmux_n03662 : STD_LOGIC; 
  signal Mmux_n036621_319 : STD_LOGIC; 
  signal Mmux_n03663 : STD_LOGIC; 
  signal Mmux_n036631_321 : STD_LOGIC; 
  signal Mmux_n03664 : STD_LOGIC; 
  signal Mmux_n036641_323 : STD_LOGIC; 
  signal Mmux_txData22 : STD_LOGIC; 
  signal Mmux_txData23_325 : STD_LOGIC; 
  signal Mmux_txData24_326 : STD_LOGIC; 
  signal Mmux_txData26 : STD_LOGIC; 
  signal Mmux_txData28 : STD_LOGIC; 
  signal Mmux_txData29_329 : STD_LOGIC; 
  signal Mmux_txData210_330 : STD_LOGIC; 
  signal Mmux_txData211_331 : STD_LOGIC; 
  signal Mmux_txData212_332 : STD_LOGIC; 
  signal Mmux_txData1 : STD_LOGIC; 
  signal Mmux_txData11_334 : STD_LOGIC; 
  signal Mmux_txData14 : STD_LOGIC; 
  signal Mmux_txData15_336 : STD_LOGIC; 
  signal Mmux_txData17 : STD_LOGIC; 
  signal Mmux_txData18_338 : STD_LOGIC; 
  signal Mmux_txData32 : STD_LOGIC; 
  signal Mmux_txData33_340 : STD_LOGIC; 
  signal Mmux_txData35 : STD_LOGIC; 
  signal Mmux_txData37 : STD_LOGIC; 
  signal Mmux_txData38_343 : STD_LOGIC; 
  signal Mmux_txData39_344 : STD_LOGIC; 
  signal Mmux_txData311 : STD_LOGIC; 
  signal Mmux_txData312_346 : STD_LOGIC; 
  signal Mmux_txData7 : STD_LOGIC; 
  signal N2 : STD_LOGIC; 
  signal Mmux_cmdValid_startSeq1 : STD_LOGIC; 
  signal Mmux_cmdValid_startSeq11_350 : STD_LOGIC; 
  signal Mmux_cmdValid_startSeq12_351 : STD_LOGIC; 
  signal Mmux_cmdValid_startSeq13_352 : STD_LOGIC; 
  signal Mmux_cmdValid_startSeq17_353 : STD_LOGIC; 
  signal Mmux_cmdValid_startSeq18_354 : STD_LOGIC; 
  signal Mmux_txData42 : STD_LOGIC; 
  signal Mmux_txData43_356 : STD_LOGIC; 
  signal Mmux_txData46 : STD_LOGIC; 
  signal Mmux_txData47_358 : STD_LOGIC; 
  signal Mmux_txData49 : STD_LOGIC; 
  signal Mmux_txData410_360 : STD_LOGIC; 
  signal N4 : STD_LOGIC; 
  signal curState_FSM_FFd13_In1_362 : STD_LOGIC; 
  signal curState_FSM_FFd13_In2_363 : STD_LOGIC; 
  signal curState_FSM_FFd13_In3_364 : STD_LOGIC; 
  signal curState_FSM_FFd13_In4_365 : STD_LOGIC; 
  signal N6 : STD_LOGIC; 
  signal N8 : STD_LOGIC; 
  signal Mmux_txData5 : STD_LOGIC; 
  signal Mmux_txData51_369 : STD_LOGIC; 
  signal dataStarted_glue_rst_370 : STD_LOGIC; 
  signal dataComplete_glue_rst_371 : STD_LOGIC; 
  signal peakByteCount_0_glue_set_372 : STD_LOGIC; 
  signal byteCount_0_glue_set_373 : STD_LOGIC; 
  signal ctrlByteCount_0_rstpot_374 : STD_LOGIC; 
  signal curState_FSM_FFd17_rstpot_375 : STD_LOGIC; 
  signal lastByte_rstpot_376 : STD_LOGIC; 
  signal N10 : STD_LOGIC; 
  signal N12 : STD_LOGIC; 
  signal N14 : STD_LOGIC; 
  signal N16 : STD_LOGIC; 
  signal reg_numWords_bcd_0_0_rstpot_381 : STD_LOGIC; 
  signal reg_numWords_bcd_0_1_rstpot_382 : STD_LOGIC; 
  signal reg_numWords_bcd_0_2_rstpot_383 : STD_LOGIC; 
  signal reg_numWords_bcd_0_3_rstpot_384 : STD_LOGIC; 
  signal reg_numWords_bcd_1_0_rstpot_385 : STD_LOGIC; 
  signal reg_numWords_bcd_1_1_rstpot_386 : STD_LOGIC; 
  signal reg_numWords_bcd_1_2_rstpot_387 : STD_LOGIC; 
  signal reg_numWords_bcd_1_3_rstpot_388 : STD_LOGIC; 
  signal reg_numWords_bcd_2_0_rstpot_389 : STD_LOGIC; 
  signal reg_numWords_bcd_2_1_rstpot_390 : STD_LOGIC; 
  signal reg_numWords_bcd_2_2_rstpot_391 : STD_LOGIC; 
  signal reg_numWords_bcd_2_3_rstpot_392 : STD_LOGIC; 
  signal N18 : STD_LOGIC; 
  signal N22 : STD_LOGIC; 
  signal N24 : STD_LOGIC; 
  signal N26 : STD_LOGIC; 
  signal N28 : STD_LOGIC; 
  signal N30 : STD_LOGIC; 
  signal N32 : STD_LOGIC; 
  signal N48 : STD_LOGIC; 
  signal N49 : STD_LOGIC; 
  signal N50 : STD_LOGIC; 
  signal N52 : STD_LOGIC; 
  signal N53 : STD_LOGIC; 
  signal N54 : STD_LOGIC; 
  signal N56 : STD_LOGIC; 
  signal N58 : STD_LOGIC; 
  signal N60 : STD_LOGIC; 
  signal N62 : STD_LOGIC; 
  signal N64 : STD_LOGIC; 
  signal N65 : STD_LOGIC; 
  signal N66 : STD_LOGIC; 
  signal N67 : STD_LOGIC; 
  signal N68 : STD_LOGIC; 
  signal N69 : STD_LOGIC; 
  signal N70 : STD_LOGIC; 
  signal N71 : STD_LOGIC; 
  signal N72 : STD_LOGIC; 
  signal N73 : STD_LOGIC; 
  signal N74 : STD_LOGIC; 
  signal byteNum_0 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal reg_maxIndex_2 : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal reg_maxIndex_1 : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal reg_maxIndex_0 : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal reg_dataResults_0 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal reg_dataResults_1 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal reg_dataResults_2 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal reg_dataResults_3 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal reg_dataResults_4 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal reg_dataResults_5 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal reg_dataResults_6 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal byteNum_1 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal byteNum_2 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal NlwRenamedSig_OI_reg_numWords_bcd_2 : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal NlwRenamedSig_OI_reg_numWords_bcd_1 : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal NlwRenamedSig_OI_reg_numWords_bcd_0 : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal byteCount : STD_LOGIC_VECTOR ( 2 downto 0 ); 
  signal peakByteCount : STD_LOGIC_VECTOR ( 2 downto 0 ); 
  signal Result : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal Mcount_byteCount_lut : STD_LOGIC_VECTOR ( 3 downto 3 ); 
  signal nibbleCount : STD_LOGIC_VECTOR ( 2 downto 0 ); 
  signal ctrlByteCount : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal GND_8_o_X_8_o_wide_mux_215_OUT : STD_LOGIC_VECTOR ( 2 downto 2 ); 
  signal Q_n0440 : STD_LOGIC_VECTOR ( 6 downto 6 ); 
  signal Q_n0437 : STD_LOGIC_VECTOR ( 6 downto 6 ); 
  signal Q_n0434 : STD_LOGIC_VECTOR ( 6 downto 6 ); 
  signal GND_8_o_GND_8_o_sub_215_OUT : STD_LOGIC_VECTOR ( 3 downto 3 ); 
begin
  numWords_bcd(2, 3) <= NlwRenamedSig_OI_reg_numWords_bcd_2(3);
  numWords_bcd(2, 2) <= NlwRenamedSig_OI_reg_numWords_bcd_2(2);
  numWords_bcd(2, 1) <= NlwRenamedSig_OI_reg_numWords_bcd_2(1);
  numWords_bcd(2, 0) <= NlwRenamedSig_OI_reg_numWords_bcd_2(0);
  numWords_bcd(1, 3) <= NlwRenamedSig_OI_reg_numWords_bcd_1(3);
  numWords_bcd(1, 2) <= NlwRenamedSig_OI_reg_numWords_bcd_1(2);
  numWords_bcd(1, 1) <= NlwRenamedSig_OI_reg_numWords_bcd_1(1);
  numWords_bcd(1, 0) <= NlwRenamedSig_OI_reg_numWords_bcd_1(0);
  numWords_bcd(0, 3) <= NlwRenamedSig_OI_reg_numWords_bcd_0(3);
  numWords_bcd(0, 2) <= NlwRenamedSig_OI_reg_numWords_bcd_0(2);
  numWords_bcd(0, 1) <= NlwRenamedSig_OI_reg_numWords_bcd_0(1);
  numWords_bcd(0, 0) <= NlwRenamedSig_OI_reg_numWords_bcd_0(0);
  rxdone <= NlwRenamedSig_OI_curState_FSM_FFd17;
  start <= NlwRenamedSig_OI_curState_FSM_FFd9;
  XST_GND : GND
    port map (
      G => Mcount_byteCount_lut(3)
    );
  reg_maxIndex_2_0 : FDRE
    port map (
      C => clk,
      CE => seqDone,
      D => maxIndex(2, 0),
      R => reset,
      Q => reg_maxIndex_2(0)
    );
  reg_maxIndex_2_1 : FDRE
    port map (
      C => clk,
      CE => seqDone,
      D => maxIndex(2, 1),
      R => reset,
      Q => reg_maxIndex_2(1)
    );
  reg_maxIndex_2_2 : FDRE
    port map (
      C => clk,
      CE => seqDone,
      D => maxIndex(2, 2),
      R => reset,
      Q => reg_maxIndex_2(2)
    );
  reg_maxIndex_2_3 : FDRE
    port map (
      C => clk,
      CE => seqDone,
      D => maxIndex(2, 3),
      R => reset,
      Q => reg_maxIndex_2(3)
    );
  reg_maxIndex_1_0 : FDRE
    port map (
      C => clk,
      CE => seqDone,
      D => maxIndex(1, 0),
      R => reset,
      Q => reg_maxIndex_1(0)
    );
  reg_maxIndex_1_1 : FDRE
    port map (
      C => clk,
      CE => seqDone,
      D => maxIndex(1, 1),
      R => reset,
      Q => reg_maxIndex_1(1)
    );
  reg_maxIndex_1_2 : FDRE
    port map (
      C => clk,
      CE => seqDone,
      D => maxIndex(1, 2),
      R => reset,
      Q => reg_maxIndex_1(2)
    );
  reg_maxIndex_1_3 : FDRE
    port map (
      C => clk,
      CE => seqDone,
      D => maxIndex(1, 3),
      R => reset,
      Q => reg_maxIndex_1(3)
    );
  reg_maxIndex_0_0 : FDRE
    port map (
      C => clk,
      CE => seqDone,
      D => maxIndex(0, 0),
      R => reset,
      Q => reg_maxIndex_0(0)
    );
  reg_maxIndex_0_1 : FDRE
    port map (
      C => clk,
      CE => seqDone,
      D => maxIndex(0, 1),
      R => reset,
      Q => reg_maxIndex_0(1)
    );
  reg_maxIndex_0_2 : FDRE
    port map (
      C => clk,
      CE => seqDone,
      D => maxIndex(0, 2),
      R => reset,
      Q => reg_maxIndex_0(2)
    );
  reg_maxIndex_0_3 : FDRE
    port map (
      C => clk,
      CE => seqDone,
      D => maxIndex(0, 3),
      R => reset,
      Q => reg_maxIndex_0(3)
    );
  reg_dataResults_0_0 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(0, 0),
      S => reset,
      Q => reg_dataResults_0(0)
    );
  reg_dataResults_0_1 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(0, 1),
      S => reset,
      Q => reg_dataResults_0(1)
    );
  reg_dataResults_0_2 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(0, 2),
      S => reset,
      Q => reg_dataResults_0(2)
    );
  reg_dataResults_0_3 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(0, 3),
      S => reset,
      Q => reg_dataResults_0(3)
    );
  reg_dataResults_0_4 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(0, 4),
      S => reset,
      Q => reg_dataResults_0(4)
    );
  reg_dataResults_0_5 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(0, 5),
      S => reset,
      Q => reg_dataResults_0(5)
    );
  reg_dataResults_0_6 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(0, 6),
      S => reset,
      Q => reg_dataResults_0(6)
    );
  reg_dataResults_0_7 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(0, 7),
      S => reset,
      Q => reg_dataResults_0(7)
    );
  reg_dataResults_1_0 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(1, 0),
      S => reset,
      Q => reg_dataResults_1(0)
    );
  reg_dataResults_1_1 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(1, 1),
      S => reset,
      Q => reg_dataResults_1(1)
    );
  reg_dataResults_1_2 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(1, 2),
      S => reset,
      Q => reg_dataResults_1(2)
    );
  reg_dataResults_1_3 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(1, 3),
      S => reset,
      Q => reg_dataResults_1(3)
    );
  reg_dataResults_1_4 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(1, 4),
      S => reset,
      Q => reg_dataResults_1(4)
    );
  reg_dataResults_1_5 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(1, 5),
      S => reset,
      Q => reg_dataResults_1(5)
    );
  reg_dataResults_1_6 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(1, 6),
      S => reset,
      Q => reg_dataResults_1(6)
    );
  reg_dataResults_1_7 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(1, 7),
      S => reset,
      Q => reg_dataResults_1(7)
    );
  reg_dataResults_2_0 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(2, 0),
      S => reset,
      Q => reg_dataResults_2(0)
    );
  reg_dataResults_2_1 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(2, 1),
      S => reset,
      Q => reg_dataResults_2(1)
    );
  reg_dataResults_2_2 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(2, 2),
      S => reset,
      Q => reg_dataResults_2(2)
    );
  reg_dataResults_2_3 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(2, 3),
      S => reset,
      Q => reg_dataResults_2(3)
    );
  reg_dataResults_2_4 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(2, 4),
      S => reset,
      Q => reg_dataResults_2(4)
    );
  reg_dataResults_2_5 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(2, 5),
      S => reset,
      Q => reg_dataResults_2(5)
    );
  reg_dataResults_2_6 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(2, 6),
      S => reset,
      Q => reg_dataResults_2(6)
    );
  reg_dataResults_2_7 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(2, 7),
      S => reset,
      Q => reg_dataResults_2(7)
    );
  reg_dataResults_3_0 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(3, 0),
      S => reset,
      Q => reg_dataResults_3(0)
    );
  reg_dataResults_3_1 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(3, 1),
      S => reset,
      Q => reg_dataResults_3(1)
    );
  reg_dataResults_3_2 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(3, 2),
      S => reset,
      Q => reg_dataResults_3(2)
    );
  reg_dataResults_3_3 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(3, 3),
      S => reset,
      Q => reg_dataResults_3(3)
    );
  reg_dataResults_3_4 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(3, 4),
      S => reset,
      Q => reg_dataResults_3(4)
    );
  reg_dataResults_3_5 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(3, 5),
      S => reset,
      Q => reg_dataResults_3(5)
    );
  reg_dataResults_3_6 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(3, 6),
      S => reset,
      Q => reg_dataResults_3(6)
    );
  reg_dataResults_3_7 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(3, 7),
      S => reset,
      Q => reg_dataResults_3(7)
    );
  reg_dataResults_4_0 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(4, 0),
      S => reset,
      Q => reg_dataResults_4(0)
    );
  reg_dataResults_4_1 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(4, 1),
      S => reset,
      Q => reg_dataResults_4(1)
    );
  reg_dataResults_4_2 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(4, 2),
      S => reset,
      Q => reg_dataResults_4(2)
    );
  reg_dataResults_4_3 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(4, 3),
      S => reset,
      Q => reg_dataResults_4(3)
    );
  reg_dataResults_4_4 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(4, 4),
      S => reset,
      Q => reg_dataResults_4(4)
    );
  reg_dataResults_4_5 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(4, 5),
      S => reset,
      Q => reg_dataResults_4(5)
    );
  reg_dataResults_4_6 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(4, 6),
      S => reset,
      Q => reg_dataResults_4(6)
    );
  reg_dataResults_4_7 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(4, 7),
      S => reset,
      Q => reg_dataResults_4(7)
    );
  reg_dataResults_5_0 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(5, 0),
      S => reset,
      Q => reg_dataResults_5(0)
    );
  reg_dataResults_5_1 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(5, 1),
      S => reset,
      Q => reg_dataResults_5(1)
    );
  reg_dataResults_5_2 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(5, 2),
      S => reset,
      Q => reg_dataResults_5(2)
    );
  reg_dataResults_5_3 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(5, 3),
      S => reset,
      Q => reg_dataResults_5(3)
    );
  reg_dataResults_5_4 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(5, 4),
      S => reset,
      Q => reg_dataResults_5(4)
    );
  reg_dataResults_5_5 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(5, 5),
      S => reset,
      Q => reg_dataResults_5(5)
    );
  reg_dataResults_5_6 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(5, 6),
      S => reset,
      Q => reg_dataResults_5(6)
    );
  reg_dataResults_5_7 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(5, 7),
      S => reset,
      Q => reg_dataResults_5(7)
    );
  reg_dataResults_6_0 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(6, 0),
      S => reset,
      Q => reg_dataResults_6(0)
    );
  reg_dataResults_6_1 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(6, 1),
      S => reset,
      Q => reg_dataResults_6(1)
    );
  reg_dataResults_6_2 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(6, 2),
      S => reset,
      Q => reg_dataResults_6(2)
    );
  reg_dataResults_6_3 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(6, 3),
      S => reset,
      Q => reg_dataResults_6(3)
    );
  reg_dataResults_6_4 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(6, 4),
      S => reset,
      Q => reg_dataResults_6(4)
    );
  reg_dataResults_6_5 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(6, 5),
      S => reset,
      Q => reg_dataResults_6(5)
    );
  reg_dataResults_6_6 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(6, 6),
      S => reset,
      Q => reg_dataResults_6(6)
    );
  reg_dataResults_6_7 : FDSE
    port map (
      C => clk,
      CE => seqDone,
      D => dataResults(6, 7),
      S => reset,
      Q => reg_dataResults_6(7)
    );
  byteNum_0_0 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => rxData(0),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_0(0)
    );
  byteNum_0_1 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => rxData(1),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_0(1)
    );
  byteNum_0_2 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => rxData(2),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_0(2)
    );
  byteNum_0_3 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => rxData(3),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_0(3)
    );
  byteNum_0_4 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => rxData(4),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_0(4)
    );
  byteNum_0_5 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => rxData(5),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_0(5)
    );
  byteNum_0_6 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => rxData(6),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_0(6)
    );
  byteNum_0_7 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => rxData(7),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_0(7)
    );
  byteNum_1_0 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_0(0),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_1(0)
    );
  byteNum_1_1 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_0(1),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_1(1)
    );
  byteNum_1_2 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_0(2),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_1(2)
    );
  byteNum_1_3 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_0(3),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_1(3)
    );
  byteNum_1_4 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_0(4),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_1(4)
    );
  byteNum_1_5 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_0(5),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_1(5)
    );
  byteNum_1_6 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_0(6),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_1(6)
    );
  byteNum_1_7 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_0(7),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_1(7)
    );
  byteNum_2_0 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_1(0),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_2(0)
    );
  byteNum_2_1 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_1(1),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_2(1)
    );
  byteNum_2_2 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_1(2),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_2(2)
    );
  byteNum_2_3 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_1(3),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_2(3)
    );
  byteNum_2_4 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_1(4),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_2(4)
    );
  byteNum_2_5 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_1(5),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_2(5)
    );
  byteNum_2_6 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_1(6),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_2(6)
    );
  byteNum_2_7 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_1(7),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_2(7)
    );
  byteNum_3_0 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_2(0),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_3_0_Q
    );
  byteNum_3_1 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_2(1),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_3_1_Q
    );
  byteNum_3_2 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_2(2),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_3_2_Q
    );
  byteNum_3_3 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_2(3),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_3_3_Q
    );
  byteNum_3_4 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_2(4),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_3_4_Q
    );
  byteNum_3_6 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_2(6),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_3_6_Q
    );
  byteNum_3_7 : FDSE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_curState_FSM_FFd17,
      D => byteNum_2(7),
      S => reset_clearCmd_OR_20_o,
      Q => byteNum_3_7_Q
    );
  nibbleCount_2 : FDRE
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      CE => GND_8_o_PWR_8_o_OR_16_o,
      D => Result(2),
      R => Mcount_nibbleCount_val_220,
      Q => nibbleCount(2)
    );
  nibbleCount_1 : FDRE
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      CE => GND_8_o_PWR_8_o_OR_16_o,
      D => Result(1),
      R => Mcount_nibbleCount_val_220,
      Q => nibbleCount(1)
    );
  curState_FSM_FFd18 : FDS
    generic map(
      INIT => '1'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd18_In,
      S => reset,
      Q => curState_FSM_FFd18_239
    );
  curState_FSM_FFd15 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd15_In,
      R => reset,
      Q => curState_FSM_FFd15_241
    );
  curState_FSM_FFd14 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd14_In2,
      R => txdone_0,
      Q => curState_FSM_FFd14_242
    );
  curState_FSM_FFd13 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd13_In,
      R => reset,
      Q => curState_FSM_FFd13_203
    );
  curState_FSM_FFd12 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd12_In,
      R => reset,
      Q => curState_FSM_FFd12_243
    );
  curState_FSM_FFd11 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd11_In,
      R => reset,
      Q => curState_FSM_FFd11_204
    );
  curState_FSM_FFd10 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd10_In,
      R => reset,
      Q => curState_FSM_FFd10_244
    );
  curState_FSM_FFd9 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd9_In1,
      R => txdone_0,
      Q => NlwRenamedSig_OI_curState_FSM_FFd9
    );
  curState_FSM_FFd8 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd8_In,
      R => reset,
      Q => curState_FSM_FFd8_205
    );
  curState_FSM_FFd7 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd7_In,
      R => reset,
      Q => curState_FSM_FFd7_245
    );
  curState_FSM_FFd6 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd6_In1_227,
      R => txdone_0,
      Q => curState_FSM_FFd6_201
    );
  curState_FSM_FFd4 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd4_In,
      R => reset,
      Q => curState_FSM_FFd4_247
    );
  curState_FSM_FFd3 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd3_In1_225,
      R => txdone_0,
      Q => curState_FSM_FFd3_202
    );
  curState_FSM_FFd2 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd2_In,
      R => reset,
      Q => curState_FSM_FFd2_206
    );
  curState_FSM_FFd1 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd1_In,
      R => reset,
      Q => curState_FSM_FFd1_248
    );
  curState_FSM_FFd16 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => NlwRenamedSig_OI_curState_FSM_FFd17,
      R => reset,
      Q => curState_FSM_FFd16_240
    );
  curState_FSM_FFd5 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd13_203,
      R => reset,
      Q => curState_FSM_FFd5_246
    );
  peakByteCount_1 : FDRE
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      CE => curState_FSM_FFd3_202,
      D => Mcount_peakByteCount1,
      R => reset,
      Q => peakByteCount(1)
    );
  peakByteCount_2 : FDRE
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      CE => curState_FSM_FFd3_202,
      D => Mcount_peakByteCount2,
      R => reset,
      Q => peakByteCount(2)
    );
  byteCount_1 : FDRE
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      CE => curState_FSM_FFd6_201,
      D => Mcount_byteCount1,
      R => reset,
      Q => byteCount(1)
    );
  byteCount_2 : FDRE
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      CE => curState_FSM_FFd6_201,
      D => Mcount_byteCount2,
      R => reset,
      Q => byteCount(2)
    );
  ctrlByteCount_1 : FDRE
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      CE => curState_FSM_FFd13_203,
      D => Result_1_1,
      R => Mcount_ctrlByteCount_val,
      Q => ctrlByteCount(1)
    );
  ctrlByteCount_2 : FDRE
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      CE => curState_FSM_FFd13_203,
      D => Result_2_1,
      R => Mcount_ctrlByteCount_val,
      Q => ctrlByteCount(2)
    );
  ctrlByteCount_3 : FDRE
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      CE => curState_FSM_FFd13_203,
      D => Result(3),
      R => Mcount_ctrlByteCount_val,
      Q => ctrlByteCount(3)
    );
  nibbleCount_0 : FDRE
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      CE => GND_8_o_PWR_8_o_OR_16_o,
      D => Result(0),
      R => Mcount_nibbleCount_val_220,
      Q => nibbleCount(0)
    );
  reset_clearCmd_OR_20_o1 : LUT4
    generic map(
      INIT => X"FFFE"
    )
    port map (
      I0 => curState_FSM_FFd6_201,
      I1 => curState_FSM_FFd3_202,
      I2 => reset,
      I3 => NlwRenamedSig_OI_curState_FSM_FFd9,
      O => reset_clearCmd_OR_20_o
    );
  curState_GND_8_o_PWR_8_o_OR_16_o1 : LUT3
    generic map(
      INIT => X"FE"
    )
    port map (
      I0 => curState_FSM_FFd8_205,
      I1 => curState_FSM_FFd11_204,
      I2 => curState_FSM_FFd2_206,
      O => GND_8_o_PWR_8_o_OR_16_o
    );
  curState_n0515_1_1 : LUT5
    generic map(
      INIT => X"FFFFFFFE"
    )
    port map (
      I0 => curState_FSM_FFd8_205,
      I1 => curState_FSM_FFd5_246,
      I2 => curState_FSM_FFd2_206,
      I3 => curState_FSM_FFd16_240,
      I4 => curState_FSM_FFd11_204,
      O => txnow
    );
  curState_FSM_FFd15_In1 : LUT3
    generic map(
      INIT => X"BA"
    )
    port map (
      I0 => curState_FSM_FFd16_240,
      I1 => txdone,
      I2 => curState_FSM_FFd15_241,
      O => curState_FSM_FFd15_In
    );
  curState_FSM_FFd12_In1 : LUT3
    generic map(
      INIT => X"BA"
    )
    port map (
      I0 => NlwRenamedSig_OI_curState_FSM_FFd9,
      I1 => dataReady,
      I2 => curState_FSM_FFd12_243,
      O => curState_FSM_FFd12_In
    );
  curState_FSM_FFd10_In1 : LUT3
    generic map(
      INIT => X"BA"
    )
    port map (
      I0 => curState_FSM_FFd11_204,
      I1 => txdone,
      I2 => curState_FSM_FFd10_244,
      O => curState_FSM_FFd10_In
    );
  curState_FSM_FFd7_In1 : LUT3
    generic map(
      INIT => X"BA"
    )
    port map (
      I0 => curState_FSM_FFd8_205,
      I1 => txdone,
      I2 => curState_FSM_FFd7_245,
      O => curState_FSM_FFd7_In
    );
  curState_FSM_FFd4_In1 : LUT3
    generic map(
      INIT => X"BA"
    )
    port map (
      I0 => curState_FSM_FFd5_246,
      I1 => txdone,
      I2 => curState_FSM_FFd4_247,
      O => curState_FSM_FFd4_In
    );
  curState_FSM_FFd1_In1 : LUT3
    generic map(
      INIT => X"BA"
    )
    port map (
      I0 => curState_FSM_FFd2_206,
      I1 => txdone,
      I2 => curState_FSM_FFd1_248,
      O => curState_FSM_FFd1_In
    );
  txdone_01 : LUT2
    generic map(
      INIT => X"B"
    )
    port map (
      I0 => reset,
      I1 => txdone,
      O => txdone_0
    );
  Mcount_nibbleCount_xor_1_11 : LUT2
    generic map(
      INIT => X"6"
    )
    port map (
      I0 => nibbleCount(1),
      I1 => nibbleCount(0),
      O => Result(1)
    );
  Mcount_ctrlByteCount_xor_1_11 : LUT2
    generic map(
      INIT => X"6"
    )
    port map (
      I0 => ctrlByteCount(1),
      I1 => ctrlByteCount(0),
      O => Result_1_1
    );
  Mcount_nibbleCount_xor_2_11 : LUT3
    generic map(
      INIT => X"6A"
    )
    port map (
      I0 => nibbleCount(2),
      I1 => nibbleCount(0),
      I2 => nibbleCount(1),
      O => Result(2)
    );
  Q_n0472_inv11 : LUT4
    generic map(
      INIT => X"0010"
    )
    port map (
      I0 => lastByte_195,
      I1 => nibbleCount(0),
      I2 => nibbleCount(1),
      I3 => nibbleCount(2),
      O => Q_n0472_inv
    );
  Mmux_cmdValid_startSeq141 : LUT4
    generic map(
      INIT => X"0001"
    )
    port map (
      I0 => byteNum_0(1),
      I1 => byteNum_0(2),
      I2 => byteNum_0(3),
      I3 => byteNum_0(0),
      O => Mmux_cmdValid_startSeq14_300
    );
  GND_8_o_GND_8_o_OR_71_o1 : LUT4
    generic map(
      INIT => X"0800"
    )
    port map (
      I0 => byteNum_0(4),
      I1 => byteNum_0(6),
      I2 => byteNum_0(7),
      I3 => Mmux_cmdValid_startSeq14_300,
      O => GND_8_o_GND_8_o_OR_71_o
    );
  curState_printPeak_INV_62_o1 : LUT3
    generic map(
      INIT => X"FE"
    )
    port map (
      I0 => curState_FSM_FFd2_206,
      I1 => curState_FSM_FFd3_202,
      I2 => curState_FSM_FFd1_248,
      O => printPeak_INV_62_o
    );
  Mcount_peakByteCount11 : LUT2
    generic map(
      INIT => X"6"
    )
    port map (
      I0 => peakByteCount(1),
      I1 => peakByteCount(0),
      O => Mcount_peakByteCount1
    );
  reg_dataResults_3_7_PWR_8_o_LessThan_203_o1 : LUT3
    generic map(
      INIT => X"57"
    )
    port map (
      I0 => reg_dataResults_3(7),
      I1 => reg_dataResults_3(5),
      I2 => reg_dataResults_3(6),
      O => reg_dataResults_3_7_PWR_8_o_LessThan_203_o
    );
  reg_dataResults_3_3_PWR_8_o_LessThan_207_o1 : LUT3
    generic map(
      INIT => X"57"
    )
    port map (
      I0 => reg_dataResults_3(3),
      I1 => reg_dataResults_3(1),
      I2 => reg_dataResults_3(2),
      O => reg_dataResults_3_3_PWR_8_o_LessThan_207_o
    );
  curState_printResults_INV_61_o1 : LUT3
    generic map(
      INIT => X"FE"
    )
    port map (
      I0 => curState_FSM_FFd7_245,
      I1 => curState_FSM_FFd8_205,
      I2 => curState_FSM_FFd6_201,
      O => printResults_INV_61_o
    );
  byte_7_PWR_8_o_LessThan_170_o1 : LUT3
    generic map(
      INIT => X"57"
    )
    port map (
      I0 => byte(7),
      I1 => byte(5),
      I2 => byte(6),
      O => byte_7_PWR_8_o_LessThan_170_o
    );
  byte_3_PWR_8_o_LessThan_174_o1 : LUT3
    generic map(
      INIT => X"57"
    )
    port map (
      I0 => byte(3),
      I1 => byte(1),
      I2 => byte(2),
      O => byte_3_PWR_8_o_LessThan_174_o
    );
  Q_n0435_2_1 : LUT2
    generic map(
      INIT => X"E"
    )
    port map (
      I0 => nibbleCount(2),
      I1 => nibbleCount(1),
      O => Q_n0435
    );
  GND_8_o_peakByteCount_2_equal_201_o_2_1 : LUT3
    generic map(
      INIT => X"10"
    )
    port map (
      I0 => peakByteCount(2),
      I1 => peakByteCount(1),
      I2 => peakByteCount(0),
      O => GND_8_o_peakByteCount_2_equal_201_o
    );
  curState_FSM_FFd2_In1 : LUT5
    generic map(
      INIT => X"AAEAAAAA"
    )
    port map (
      I0 => curState_FSM_FFd3_202,
      I1 => curState_FSM_FFd1_248,
      I2 => txdone,
      I3 => n0015,
      I4 => GND_8_o_peakByteCount_2_equal_201_o,
      O => curState_FSM_FFd2_In
    );
  curState_FSM_FFd11_In1 : LUT6
    generic map(
      INIT => X"888888888888F888"
    )
    port map (
      I0 => dataReady,
      I1 => curState_FSM_FFd12_243,
      I2 => curState_FSM_FFd10_244,
      I3 => txdone,
      I4 => n0015,
      I5 => dataStarted_194,
      O => curState_FSM_FFd11_In
    );
  n00151 : LUT3
    generic map(
      INIT => X"EA"
    )
    port map (
      I0 => nibbleCount(2),
      I1 => nibbleCount(1),
      I2 => nibbleCount(0),
      O => n0015
    );
  n0023_3_1 : LUT4
    generic map(
      INIT => X"0200"
    )
    port map (
      I0 => ctrlByteCount(3),
      I1 => ctrlByteCount(2),
      I2 => ctrlByteCount(1),
      I3 => ctrlByteCount(0),
      O => n0023
    );
  Result_3_1 : LUT4
    generic map(
      INIT => X"6CCC"
    )
    port map (
      I0 => ctrlByteCount(2),
      I1 => ctrlByteCount(3),
      I2 => ctrlByteCount(0),
      I3 => ctrlByteCount(1),
      O => Result(3)
    );
  Result_2_11 : LUT3
    generic map(
      INIT => X"6A"
    )
    port map (
      I0 => ctrlByteCount(2),
      I1 => ctrlByteCount(0),
      I2 => ctrlByteCount(1),
      O => Result_2_1
    );
  Mcount_peakByteCount_xor_2_11 : LUT3
    generic map(
      INIT => X"68"
    )
    port map (
      I0 => peakByteCount(0),
      I1 => peakByteCount(1),
      I2 => peakByteCount(2),
      O => Mcount_peakByteCount2
    );
  curState_FSM_FFd14_In21 : LUT5
    generic map(
      INIT => X"EEEFAAAA"
    )
    port map (
      I0 => curState_FSM_FFd15_241,
      I1 => dataComplete_158,
      I2 => GND_8_o_GND_8_o_OR_70_o_306,
      I3 => GND_8_o_GND_8_o_OR_71_o,
      I4 => curState_FSM_FFd3_In11,
      O => curState_FSM_FFd14_In2
    );
  Mcount_byteCount_xor_1_11 : LUT2
    generic map(
      INIT => X"6"
    )
    port map (
      I0 => byteCount(0),
      I1 => byteCount(1),
      O => Mcount_byteCount1
    );
  Mcount_byteCount_xor_2_11 : LUT3
    generic map(
      INIT => X"6A"
    )
    port map (
      I0 => byteCount(2),
      I1 => byteCount(0),
      I2 => byteCount(1),
      O => Mcount_byteCount2
    );
  Msub_GND_8_o_GND_8_o_sub_215_OUT_3_0_xor_3_11 : LUT4
    generic map(
      INIT => X"AAA9"
    )
    port map (
      I0 => ctrlByteCount(3),
      I1 => ctrlByteCount(0),
      I2 => ctrlByteCount(1),
      I3 => ctrlByteCount(2),
      O => GND_8_o_GND_8_o_sub_215_OUT(3)
    );
  Mcount_nibbleCount_val_SW0 : LUT2
    generic map(
      INIT => X"E"
    )
    port map (
      I0 => curState_FSM_FFd13_203,
      I1 => reset,
      O => N01
    );
  Mcount_nibbleCount_val : LUT6
    generic map(
      INIT => X"FF54FFFFFF54FF54"
    )
    port map (
      I0 => dataComplete_158,
      I1 => curState_FSM_FFd6_201,
      I2 => curState_FSM_FFd3_202,
      I3 => N01,
      I4 => dataStarted_194,
      I5 => NlwRenamedSig_OI_curState_FSM_FFd9,
      O => Mcount_nibbleCount_val_220
    );
  Mmux_n036661 : LUT6
    generic map(
      INIT => X"FD75B931EC64A820"
    )
    port map (
      I0 => byteCount(0),
      I1 => byteCount(2),
      I2 => reg_dataResults_4(5),
      I3 => reg_dataResults_0(5),
      I4 => reg_dataResults_1(5),
      I5 => reg_dataResults_5(5),
      O => Mmux_n03666
    );
  Mmux_n036662 : LUT5
    generic map(
      INIT => X"FD75A820"
    )
    port map (
      I0 => byteCount(2),
      I1 => byteCount(0),
      I2 => reg_dataResults_3(5),
      I3 => reg_dataResults_2(5),
      I4 => reg_dataResults_6(5),
      O => Mmux_n036661_311
    );
  Mmux_n036663 : LUT3
    generic map(
      INIT => X"E4"
    )
    port map (
      I0 => byteCount(1),
      I1 => Mmux_n036661_311,
      I2 => Mmux_n03666,
      O => n0366_5_Q
    );
  Mmux_n036671 : LUT6
    generic map(
      INIT => X"FD75B931EC64A820"
    )
    port map (
      I0 => byteCount(0),
      I1 => byteCount(2),
      I2 => reg_dataResults_4(6),
      I3 => reg_dataResults_0(6),
      I4 => reg_dataResults_1(6),
      I5 => reg_dataResults_5(6),
      O => Mmux_n03667
    );
  Mmux_n036672 : LUT5
    generic map(
      INIT => X"FD75A820"
    )
    port map (
      I0 => byteCount(2),
      I1 => byteCount(0),
      I2 => reg_dataResults_3(6),
      I3 => reg_dataResults_2(6),
      I4 => reg_dataResults_6(6),
      O => Mmux_n036671_313
    );
  Mmux_n036673 : LUT3
    generic map(
      INIT => X"E4"
    )
    port map (
      I0 => byteCount(1),
      I1 => Mmux_n036671_313,
      I2 => Mmux_n03667,
      O => n0366_6_Q
    );
  Mmux_n036681 : LUT6
    generic map(
      INIT => X"FD75B931EC64A820"
    )
    port map (
      I0 => byteCount(0),
      I1 => byteCount(2),
      I2 => reg_dataResults_4(7),
      I3 => reg_dataResults_0(7),
      I4 => reg_dataResults_1(7),
      I5 => reg_dataResults_5(7),
      O => Mmux_n03668
    );
  Mmux_n036682 : LUT5
    generic map(
      INIT => X"FD75A820"
    )
    port map (
      I0 => byteCount(2),
      I1 => byteCount(0),
      I2 => reg_dataResults_3(7),
      I3 => reg_dataResults_2(7),
      I4 => reg_dataResults_6(7),
      O => Mmux_n036681_315
    );
  Mmux_n036683 : LUT3
    generic map(
      INIT => X"E4"
    )
    port map (
      I0 => byteCount(1),
      I1 => Mmux_n036681_315,
      I2 => Mmux_n03668,
      O => Msub_GND_8_o_GND_8_o_sub_187_OUT_3_0_lut_3_Q
    );
  Mmux_n036611 : LUT6
    generic map(
      INIT => X"FD75B931EC64A820"
    )
    port map (
      I0 => byteCount(0),
      I1 => byteCount(2),
      I2 => reg_dataResults_4(0),
      I3 => reg_dataResults_0(0),
      I4 => reg_dataResults_1(0),
      I5 => reg_dataResults_5(0),
      O => Mmux_n03661
    );
  Mmux_n036612 : LUT5
    generic map(
      INIT => X"FD75A820"
    )
    port map (
      I0 => byteCount(2),
      I1 => byteCount(0),
      I2 => reg_dataResults_3(0),
      I3 => reg_dataResults_2(0),
      I4 => reg_dataResults_6(0),
      O => Mmux_n036611_317
    );
  Mmux_n036613 : LUT3
    generic map(
      INIT => X"E4"
    )
    port map (
      I0 => byteCount(1),
      I1 => Mmux_n036611_317,
      I2 => Mmux_n03661,
      O => Msub_GND_8_o_GND_8_o_sub_197_OUT_3_0_cy_0_Q
    );
  Mmux_n036621 : LUT6
    generic map(
      INIT => X"FD75B931EC64A820"
    )
    port map (
      I0 => byteCount(0),
      I1 => byteCount(2),
      I2 => reg_dataResults_4(1),
      I3 => reg_dataResults_0(1),
      I4 => reg_dataResults_1(1),
      I5 => reg_dataResults_5(1),
      O => Mmux_n03662
    );
  Mmux_n036622 : LUT5
    generic map(
      INIT => X"FD75A820"
    )
    port map (
      I0 => byteCount(2),
      I1 => byteCount(0),
      I2 => reg_dataResults_3(1),
      I3 => reg_dataResults_2(1),
      I4 => reg_dataResults_6(1),
      O => Mmux_n036621_319
    );
  Mmux_n036623 : LUT3
    generic map(
      INIT => X"E4"
    )
    port map (
      I0 => byteCount(1),
      I1 => Mmux_n036621_319,
      I2 => Mmux_n03662,
      O => n0366_1_Q
    );
  Mmux_n036631 : LUT6
    generic map(
      INIT => X"FD75B931EC64A820"
    )
    port map (
      I0 => byteCount(0),
      I1 => byteCount(2),
      I2 => reg_dataResults_4(2),
      I3 => reg_dataResults_0(2),
      I4 => reg_dataResults_1(2),
      I5 => reg_dataResults_5(2),
      O => Mmux_n03663
    );
  Mmux_n036632 : LUT5
    generic map(
      INIT => X"FD75A820"
    )
    port map (
      I0 => byteCount(2),
      I1 => byteCount(0),
      I2 => reg_dataResults_3(2),
      I3 => reg_dataResults_2(2),
      I4 => reg_dataResults_6(2),
      O => Mmux_n036631_321
    );
  Mmux_n036633 : LUT3
    generic map(
      INIT => X"E4"
    )
    port map (
      I0 => byteCount(1),
      I1 => Mmux_n036631_321,
      I2 => Mmux_n03663,
      O => n0366_2_Q
    );
  Mmux_n036641 : LUT6
    generic map(
      INIT => X"FD75B931EC64A820"
    )
    port map (
      I0 => byteCount(0),
      I1 => byteCount(2),
      I2 => reg_dataResults_4(3),
      I3 => reg_dataResults_0(3),
      I4 => reg_dataResults_1(3),
      I5 => reg_dataResults_5(3),
      O => Mmux_n03664
    );
  Mmux_n036642 : LUT5
    generic map(
      INIT => X"FD75A820"
    )
    port map (
      I0 => byteCount(2),
      I1 => byteCount(0),
      I2 => reg_dataResults_3(3),
      I3 => reg_dataResults_2(3),
      I4 => reg_dataResults_6(3),
      O => Mmux_n036641_323
    );
  Mmux_n036643 : LUT3
    generic map(
      INIT => X"E4"
    )
    port map (
      I0 => byteCount(1),
      I1 => Mmux_n036641_323,
      I2 => Mmux_n03664,
      O => Msub_GND_8_o_GND_8_o_sub_197_OUT_3_0_lut_3_Q
    );
  Mmux_txData210 : LUT6
    generic map(
      INIT => X"F8F8F8F8FFF8F8F8"
    )
    port map (
      I0 => byteNum_0(1),
      I1 => Mmux_byteNum_0_7_GND_8_o_mux_218_OUT311,
      I2 => Mmux_txData23_325,
      I3 => Mmux_txData28,
      I4 => printPeak_INV_62_o,
      I5 => printResults_INV_61_o,
      O => Mmux_txData29_329
    );
  Mmux_txData214 : LUT6
    generic map(
      INIT => X"FFFFFFFFAAAA8880"
    )
    port map (
      I0 => dataStarted_194,
      I1 => Mmux_txData210_330,
      I2 => Mmux_txData211_331,
      I3 => Mmux_txData212_332,
      I4 => Mmux_txData29_329,
      I5 => Mmux_txData22,
      O => txData(1)
    );
  Mmux_txData12 : LUT6
    generic map(
      INIT => X"1101011110000010"
    )
    port map (
      I0 => dataStarted_194,
      I1 => Q_n0435,
      I2 => nibbleCount(0),
      I3 => byte_3_PWR_8_o_LessThan_174_o,
      I4 => byte(0),
      I5 => Mmux_txData1,
      O => Mmux_txData11_334
    );
  Mmux_txData16 : LUT5
    generic map(
      INIT => X"D78282D7"
    )
    port map (
      I0 => nibbleCount(0),
      I1 => reg_dataResults_3(0),
      I2 => reg_dataResults_3_3_PWR_8_o_LessThan_207_o,
      I3 => reg_dataResults_3(4),
      I4 => reg_dataResults_3_7_PWR_8_o_LessThan_203_o,
      O => Mmux_txData15_336
    );
  Mmux_txData19 : LUT5
    generic map(
      INIT => X"D78282D7"
    )
    port map (
      I0 => nibbleCount(0),
      I1 => GND_8_o_PWR_8_o_LessThan_192_o,
      I2 => Msub_GND_8_o_GND_8_o_sub_197_OUT_3_0_cy_0_Q,
      I3 => GND_8_o_PWR_8_o_LessThan_182_o,
      I4 => Msub_GND_8_o_GND_8_o_sub_187_OUT_3_0_cy_0_Q,
      O => Mmux_txData18_338
    );
  Mmux_txData110 : LUT6
    generic map(
      INIT => X"FFFFFFFFAAAA0080"
    )
    port map (
      I0 => dataStarted_194,
      I1 => Mmux_txData18_338,
      I2 => printResults_INV_61_o,
      I3 => Q_n0435,
      I4 => Mmux_txData17,
      I5 => Mmux_txData11_334,
      O => txData(0)
    );
  Mmux_txData34 : LUT5
    generic map(
      INIT => X"88808888"
    )
    port map (
      I0 => reg_dataResults_3(2),
      I1 => nibbleCount(0),
      I2 => reg_dataResults_3(0),
      I3 => reg_dataResults_3(1),
      I4 => reg_dataResults_3(3),
      O => Mmux_txData33_340
    );
  Mmux_txData310 : LUT6
    generic map(
      INIT => X"FFFFF888F888F888"
    )
    port map (
      I0 => byteNum_0(2),
      I1 => Mmux_byteNum_0_7_GND_8_o_mux_218_OUT311,
      I2 => GND_8_o_X_8_o_wide_mux_215_OUT(2),
      I3 => Mmux_byteNum_0_7_GND_8_o_mux_218_OUT312,
      I4 => Mmux_txData37,
      I5 => Mmux_txData38_343,
      O => Mmux_txData39_344
    );
  Mmux_txData312 : LUT5
    generic map(
      INIT => X"88808888"
    )
    port map (
      I0 => nibbleCount(0),
      I1 => n0366_2_Q,
      I2 => Msub_GND_8_o_GND_8_o_sub_197_OUT_3_0_cy_0_Q,
      I3 => n0366_1_Q,
      I4 => Msub_GND_8_o_GND_8_o_sub_197_OUT_3_0_lut_3_Q,
      O => Mmux_txData311
    );
  Mmux_txData313 : LUT5
    generic map(
      INIT => X"22222022"
    )
    port map (
      I0 => n0366_6_Q,
      I1 => nibbleCount(0),
      I2 => Msub_GND_8_o_GND_8_o_sub_187_OUT_3_0_cy_0_Q,
      I3 => Msub_GND_8_o_GND_8_o_sub_187_OUT_3_0_lut_3_Q,
      I4 => n0366_5_Q,
      O => Mmux_txData312_346
    );
  Mmux_txData314 : LUT6
    generic map(
      INIT => X"FFFFFFFFAAAA8880"
    )
    port map (
      I0 => dataStarted_194,
      I1 => Mmux_txData210_330,
      I2 => Mmux_txData311,
      I3 => Mmux_txData312_346,
      I4 => Mmux_txData39_344,
      I5 => Mmux_txData32,
      O => txData(2)
    );
  Mmux_txData71 : LUT5
    generic map(
      INIT => X"EAAA4000"
    )
    port map (
      I0 => printResults_INV_61_o,
      I1 => Q_n0440(6),
      I2 => GND_8_o_peakByteCount_2_equal_201_o,
      I3 => printPeak_INV_62_o,
      I4 => Q_n0437(6),
      O => Mmux_txData7
    );
  Mmux_txData72 : LUT6
    generic map(
      INIT => X"AF0D0D0DAA080808"
    )
    port map (
      I0 => dataStarted_194,
      I1 => Mmux_txData7,
      I2 => Q_n0435,
      I3 => byteNum_0(6),
      I4 => Mmux_byteNum_0_7_GND_8_o_mux_218_OUT311,
      I5 => Q_n0434(6),
      O => txData(6)
    );
  GND_8_o_GND_8_o_OR_70_o_SW0 : LUT2
    generic map(
      INIT => X"E"
    )
    port map (
      I0 => byteNum_0(0),
      I1 => byteNum_0(4),
      O => N2
    );
  GND_8_o_GND_8_o_OR_70_o : LUT6
    generic map(
      INIT => X"0000000000080000"
    )
    port map (
      I0 => byteNum_0(3),
      I1 => byteNum_0(2),
      I2 => byteNum_0(7),
      I3 => byteNum_0(1),
      I4 => byteNum_0(6),
      I5 => N2,
      O => GND_8_o_GND_8_o_OR_70_o_306
    );
  Mmux_cmdValid_startSeq11 : LUT6
    generic map(
      INIT => X"0000000000000001"
    )
    port map (
      I0 => byteNum_2(0),
      I1 => byteNum_1(1),
      I2 => byteNum_1(2),
      I3 => byteNum_1(3),
      I4 => byteNum_2(2),
      I5 => byteNum_2(1),
      O => Mmux_cmdValid_startSeq1
    );
  Mmux_cmdValid_startSeq12 : LUT4
    generic map(
      INIT => X"1000"
    )
    port map (
      I0 => byteNum_1(0),
      I1 => byteNum_2(3),
      I2 => Mmux_cmdValid_startSeq1,
      I3 => Mmux_cmdValid_startSeq14_300,
      O => Mmux_cmdValid_startSeq11_350
    );
  Mmux_cmdValid_startSeq13 : LUT5
    generic map(
      INIT => X"FFFFFFFE"
    )
    port map (
      I0 => byteNum_1(6),
      I1 => byteNum_2(6),
      I2 => byteNum_1(7),
      I3 => byteNum_0(7),
      I4 => byteNum_0(6),
      O => Mmux_cmdValid_startSeq12_351
    );
  Mmux_cmdValid_startSeq14 : LUT6
    generic map(
      INIT => X"FFFFFFE0E0E0E0E0"
    )
    port map (
      I0 => byteNum_1(2),
      I1 => byteNum_1(1),
      I2 => byteNum_1(3),
      I3 => byteNum_0(2),
      I4 => byteNum_0(1),
      I5 => byteNum_0(3),
      O => Mmux_cmdValid_startSeq13_352
    );
  Mmux_cmdValid_startSeq18 : LUT6
    generic map(
      INIT => X"7FFFFFFFFFFFFFFF"
    )
    port map (
      I0 => byteNum_3_0_Q,
      I1 => byteNum_3_6_Q,
      I2 => byteNum_2(5),
      I3 => byteNum_2(4),
      I4 => byteNum_1(5),
      I5 => byteNum_1(4),
      O => Mmux_cmdValid_startSeq18_354
    );
  Mmux_cmdValid_startSeq19 : LUT5
    generic map(
      INIT => X"FFFFFFF7"
    )
    port map (
      I0 => byteNum_0(5),
      I1 => byteNum_0(4),
      I2 => Mmux_cmdValid_startSeq18_354,
      I3 => Mmux_cmdValid_startSeq11_350,
      I4 => Mmux_cmdValid_startSeq17_353,
      O => cmdValid_startSeq
    );
  Mmux_txData48 : LUT6
    generic map(
      INIT => X"FFFFFFFF55514440"
    )
    port map (
      I0 => printResults_INV_61_o,
      I1 => printPeak_INV_62_o,
      I2 => Mmux_txData46,
      I3 => Mmux_txData43_356,
      I4 => byteNum_0(3),
      I5 => Mmux_byteNum_0_7_GND_8_o_mux_218_OUT312,
      O => Mmux_txData47_358
    );
  Mmux_txData412 : LUT6
    generic map(
      INIT => X"FFFFFFFFAAAA8880"
    )
    port map (
      I0 => dataStarted_194,
      I1 => Mmux_txData210_330,
      I2 => Mmux_txData49,
      I3 => Mmux_txData410_360,
      I4 => Mmux_txData47_358,
      I5 => Mmux_txData42,
      O => txData(3)
    );
  curState_FSM_FFd3_In1 : LUT6
    generic map(
      INIT => X"C0D5C0C0C0C0C0C0"
    )
    port map (
      I0 => dataComplete_158,
      I1 => curState_FSM_FFd1_248,
      I2 => N4,
      I3 => GND_8_o_GND_8_o_OR_70_o_306,
      I4 => GND_8_o_GND_8_o_OR_71_o,
      I5 => curState_FSM_FFd3_In11,
      O => curState_FSM_FFd3_In1_225
    );
  curState_FSM_FFd13_In2 : LUT2
    generic map(
      INIT => X"8"
    )
    port map (
      I0 => curState_FSM_FFd10_244,
      I1 => dataStarted_194,
      O => curState_FSM_FFd13_In2_363
    );
  curState_FSM_FFd13_In3 : LUT5
    generic map(
      INIT => X"80000000"
    )
    port map (
      I0 => curState_FSM_FFd7_245,
      I1 => byteCount(1),
      I2 => byteCount(0),
      I3 => byteCount(2),
      I4 => n0015,
      O => curState_FSM_FFd13_In3_364
    );
  curState_FSM_FFd13_In4 : LUT6
    generic map(
      INIT => X"AAAAAAAAAAA0AAA8"
    )
    port map (
      I0 => txdone,
      I1 => curState_FSM_FFd4_247,
      I2 => curState_FSM_FFd13_In1_362,
      I3 => curState_FSM_FFd13_In2_363,
      I4 => n0023,
      I5 => curState_FSM_FFd13_In3_364,
      O => curState_FSM_FFd13_In4_365
    );
  curState_FSM_FFd13_In5 : LUT6
    generic map(
      INIT => X"FFFF2220FFFFAAAA"
    )
    port map (
      I0 => curState_FSM_FFd14_242,
      I1 => dataComplete_158,
      I2 => GND_8_o_GND_8_o_OR_70_o_306,
      I3 => GND_8_o_GND_8_o_OR_71_o,
      I4 => curState_FSM_FFd13_In4_365,
      I5 => cmdValid_startSeq,
      O => curState_FSM_FFd13_In
    );
  curState_FSM_FFd6_In1_SW0 : LUT3
    generic map(
      INIT => X"80"
    )
    port map (
      I0 => byteCount(2),
      I1 => byteCount(0),
      I2 => byteCount(1),
      O => N6
    );
  curState_FSM_FFd6_In1 : LUT6
    generic map(
      INIT => X"40FF404040404040"
    )
    port map (
      I0 => N6,
      I1 => curState_FSM_FFd7_245,
      I2 => n0015,
      I3 => dataComplete_158,
      I4 => GND_8_o_GND_8_o_OR_70_o_306,
      I5 => curState_FSM_FFd3_In11,
      O => curState_FSM_FFd6_In1_227
    );
  Mmux_txData511_SW0 : LUT3
    generic map(
      INIT => X"EA"
    )
    port map (
      I0 => ctrlByteCount(2),
      I1 => ctrlByteCount(0),
      I2 => ctrlByteCount(1),
      O => N8
    );
  Mmux_txData511 : LUT6
    generic map(
      INIT => X"2020202020FF2020"
    )
    port map (
      I0 => Mmux_byteNum_0_7_GND_8_o_mux_218_OUT312,
      I1 => GND_8_o_GND_8_o_sub_215_OUT(3),
      I2 => N8,
      I3 => GND_8_o_peakByteCount_2_equal_201_o,
      I4 => printPeak_INV_62_o,
      I5 => printResults_INV_61_o,
      O => Mmux_txData511_308
    );
  dataStarted : FDS
    generic map(
      INIT => '1'
    )
    port map (
      C => clk,
      D => dataStarted_glue_rst_370,
      S => reset,
      Q => dataStarted_194
    );
  dataComplete : FDS
    port map (
      C => clk,
      D => dataComplete_glue_rst_371,
      S => reset,
      Q => dataComplete_158
    );
  peakByteCount_0 : FD
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => peakByteCount_0_glue_set_372,
      Q => peakByteCount(0)
    );
  byteCount_0 : FD
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => byteCount_0_glue_set_373,
      Q => byteCount(0)
    );
  ctrlByteCount_0 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => ctrlByteCount_0_rstpot_374,
      R => reset,
      Q => ctrlByteCount(0)
    );
  curState_FSM_FFd17 : FD
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd17_rstpot_375,
      Q => NlwRenamedSig_OI_curState_FSM_FFd17
    );
  lastByte : FD
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => lastByte_rstpot_376,
      Q => lastByte_195
    );
  Mmux_cmdValid_startSeq17_SW0 : LUT6
    generic map(
      INIT => X"FFFFFFFEFEFEFEFE"
    )
    port map (
      I0 => byteNum_3_1_Q,
      I1 => byteNum_3_2_Q,
      I2 => byteNum_2(7),
      I3 => byteNum_2(1),
      I4 => byteNum_2(2),
      I5 => byteNum_2(3),
      O => N10
    );
  Mmux_cmdValid_startSeq17 : LUT6
    generic map(
      INIT => X"FFFFFFFFFFFFFFFE"
    )
    port map (
      I0 => byteNum_3_3_Q,
      I1 => byteNum_3_4_Q,
      I2 => byteNum_3_7_Q,
      I3 => Mmux_cmdValid_startSeq12_351,
      I4 => N10,
      I5 => Mmux_cmdValid_startSeq13_352,
      O => Mmux_cmdValid_startSeq17_353
    );
  curState_FSM_FFd18_In1 : LUT6
    generic map(
      INIT => X"5444545444444444"
    )
    port map (
      I0 => rxnow,
      I1 => curState_FSM_FFd18_239,
      I2 => curState_FSM_FFd14_242,
      I3 => dataComplete_158,
      I4 => N12,
      I5 => cmdValid_startSeq,
      O => curState_FSM_FFd18_In
    );
  Mmux_cmdValid_startSeq19_SW0 : LUT2
    generic map(
      INIT => X"8"
    )
    port map (
      I0 => byteNum_0(5),
      I1 => byteNum_0(4),
      O => N14
    );
  curState_FSM_FFd3_In111 : LUT6
    generic map(
      INIT => X"8888888888888808"
    )
    port map (
      I0 => curState_FSM_FFd4_247,
      I1 => n0023,
      I2 => N14,
      I3 => Mmux_cmdValid_startSeq18_354,
      I4 => Mmux_cmdValid_startSeq11_350,
      I5 => Mmux_cmdValid_startSeq17_353,
      O => curState_FSM_FFd3_In11
    );
  curState_FSM_FFd13_In1 : LUT4
    generic map(
      INIT => X"0200"
    )
    port map (
      I0 => curState_FSM_FFd1_248,
      I1 => peakByteCount(1),
      I2 => peakByteCount(0),
      I3 => peakByteCount(2),
      O => curState_FSM_FFd13_In1_362
    );
  reg_numWords_bcd_0_0 : FDS
    port map (
      C => clk,
      D => reg_numWords_bcd_0_0_rstpot_381,
      S => reset,
      Q => NlwRenamedSig_OI_reg_numWords_bcd_0(0)
    );
  reg_numWords_bcd_0_1 : FDS
    port map (
      C => clk,
      D => reg_numWords_bcd_0_1_rstpot_382,
      S => reset,
      Q => NlwRenamedSig_OI_reg_numWords_bcd_0(1)
    );
  reg_numWords_bcd_0_2 : FDS
    port map (
      C => clk,
      D => reg_numWords_bcd_0_2_rstpot_383,
      S => reset,
      Q => NlwRenamedSig_OI_reg_numWords_bcd_0(2)
    );
  reg_numWords_bcd_0_3 : FDS
    port map (
      C => clk,
      D => reg_numWords_bcd_0_3_rstpot_384,
      S => reset,
      Q => NlwRenamedSig_OI_reg_numWords_bcd_0(3)
    );
  reg_numWords_bcd_1_0 : FDS
    port map (
      C => clk,
      D => reg_numWords_bcd_1_0_rstpot_385,
      S => reset,
      Q => NlwRenamedSig_OI_reg_numWords_bcd_1(0)
    );
  reg_numWords_bcd_1_1 : FDS
    port map (
      C => clk,
      D => reg_numWords_bcd_1_1_rstpot_386,
      S => reset,
      Q => NlwRenamedSig_OI_reg_numWords_bcd_1(1)
    );
  reg_numWords_bcd_1_2 : FDS
    port map (
      C => clk,
      D => reg_numWords_bcd_1_2_rstpot_387,
      S => reset,
      Q => NlwRenamedSig_OI_reg_numWords_bcd_1(2)
    );
  reg_numWords_bcd_1_3 : FDS
    port map (
      C => clk,
      D => reg_numWords_bcd_1_3_rstpot_388,
      S => reset,
      Q => NlwRenamedSig_OI_reg_numWords_bcd_1(3)
    );
  reg_numWords_bcd_2_0 : FDS
    port map (
      C => clk,
      D => reg_numWords_bcd_2_0_rstpot_389,
      S => reset,
      Q => NlwRenamedSig_OI_reg_numWords_bcd_2(0)
    );
  reg_numWords_bcd_2_1 : FDS
    port map (
      C => clk,
      D => reg_numWords_bcd_2_1_rstpot_390,
      S => reset,
      Q => NlwRenamedSig_OI_reg_numWords_bcd_2(1)
    );
  reg_numWords_bcd_2_2 : FDS
    port map (
      C => clk,
      D => reg_numWords_bcd_2_2_rstpot_391,
      S => reset,
      Q => NlwRenamedSig_OI_reg_numWords_bcd_2(2)
    );
  reg_numWords_bcd_2_3 : FDS
    port map (
      C => clk,
      D => reg_numWords_bcd_2_3_rstpot_392,
      S => reset,
      Q => NlwRenamedSig_OI_reg_numWords_bcd_2(3)
    );
  Mmux_cmdValid_startSeq19_SW1 : LUT5
    generic map(
      INIT => X"FFFF7FFF"
    )
    port map (
      I0 => curState_FSM_FFd4_247,
      I1 => byteNum_0(5),
      I2 => byteNum_0(4),
      I3 => n0023,
      I4 => Mmux_cmdValid_startSeq18_354,
      O => N18
    );
  curState_FSM_FFd9_In11 : LUT6
    generic map(
      INIT => X"20202020202020FF"
    )
    port map (
      I0 => curState_FSM_FFd10_244,
      I1 => dataStarted_194,
      I2 => n0015,
      I3 => N18,
      I4 => Mmux_cmdValid_startSeq11_350,
      I5 => Mmux_cmdValid_startSeq17_353,
      O => curState_FSM_FFd9_In1
    );
  curState_FSM_FFd17_In11_SW1_SW0 : LUT4
    generic map(
      INIT => X"FFFD"
    )
    port map (
      I0 => byteNum_0(6),
      I1 => byteNum_0(7),
      I2 => byteNum_0(1),
      I3 => dataComplete_158,
      O => N22
    );
  cmdValid_startSeq_inv1_SW0 : LUT3
    generic map(
      INIT => X"E4"
    )
    port map (
      I0 => byteNum_0(5),
      I1 => NlwRenamedSig_OI_reg_numWords_bcd_0(0),
      I2 => byteNum_0(0),
      O => N24
    );
  reg_numWords_bcd_0_0_rstpot : LUT6
    generic map(
      INIT => X"AAAAAAAAAAAAAAE2"
    )
    port map (
      I0 => NlwRenamedSig_OI_reg_numWords_bcd_0(0),
      I1 => byteNum_0(4),
      I2 => N24,
      I3 => Mmux_cmdValid_startSeq18_354,
      I4 => Mmux_cmdValid_startSeq11_350,
      I5 => Mmux_cmdValid_startSeq17_353,
      O => reg_numWords_bcd_0_0_rstpot_381
    );
  cmdValid_startSeq_inv1_SW1 : LUT3
    generic map(
      INIT => X"E4"
    )
    port map (
      I0 => byteNum_0(5),
      I1 => NlwRenamedSig_OI_reg_numWords_bcd_0(1),
      I2 => byteNum_0(1),
      O => N26
    );
  reg_numWords_bcd_0_1_rstpot : LUT6
    generic map(
      INIT => X"AAAAAAAAAAAAAAE2"
    )
    port map (
      I0 => NlwRenamedSig_OI_reg_numWords_bcd_0(1),
      I1 => byteNum_0(4),
      I2 => N26,
      I3 => Mmux_cmdValid_startSeq18_354,
      I4 => Mmux_cmdValid_startSeq11_350,
      I5 => Mmux_cmdValid_startSeq17_353,
      O => reg_numWords_bcd_0_1_rstpot_382
    );
  cmdValid_startSeq_inv1_SW2 : LUT3
    generic map(
      INIT => X"E4"
    )
    port map (
      I0 => byteNum_0(5),
      I1 => NlwRenamedSig_OI_reg_numWords_bcd_0(2),
      I2 => byteNum_0(2),
      O => N28
    );
  reg_numWords_bcd_0_2_rstpot : LUT6
    generic map(
      INIT => X"AAAAAAAAAAAAAAE2"
    )
    port map (
      I0 => NlwRenamedSig_OI_reg_numWords_bcd_0(2),
      I1 => byteNum_0(4),
      I2 => N28,
      I3 => Mmux_cmdValid_startSeq18_354,
      I4 => Mmux_cmdValid_startSeq11_350,
      I5 => Mmux_cmdValid_startSeq17_353,
      O => reg_numWords_bcd_0_2_rstpot_383
    );
  cmdValid_startSeq_inv1_SW3 : LUT3
    generic map(
      INIT => X"E4"
    )
    port map (
      I0 => byteNum_0(5),
      I1 => NlwRenamedSig_OI_reg_numWords_bcd_0(3),
      I2 => byteNum_0(3),
      O => N30
    );
  reg_numWords_bcd_0_3_rstpot : LUT6
    generic map(
      INIT => X"AAAAAAAAAAAAAAE2"
    )
    port map (
      I0 => NlwRenamedSig_OI_reg_numWords_bcd_0(3),
      I1 => byteNum_0(4),
      I2 => N30,
      I3 => Mmux_cmdValid_startSeq18_354,
      I4 => Mmux_cmdValid_startSeq11_350,
      I5 => Mmux_cmdValid_startSeq17_353,
      O => reg_numWords_bcd_0_3_rstpot_384
    );
  cmdValid_startSeq_inv1_SW4 : LUT3
    generic map(
      INIT => X"E4"
    )
    port map (
      I0 => byteNum_0(5),
      I1 => NlwRenamedSig_OI_reg_numWords_bcd_1(0),
      I2 => byteNum_1(0),
      O => N32
    );
  reg_numWords_bcd_1_0_rstpot : LUT6
    generic map(
      INIT => X"AAAAAAAAAAAAAAE2"
    )
    port map (
      I0 => NlwRenamedSig_OI_reg_numWords_bcd_1(0),
      I1 => byteNum_0(4),
      I2 => N32,
      I3 => Mmux_cmdValid_startSeq18_354,
      I4 => Mmux_cmdValid_startSeq11_350,
      I5 => Mmux_cmdValid_startSeq17_353,
      O => reg_numWords_bcd_1_0_rstpot_385
    );
  reg_numWords_bcd_1_1_rstpot : LUT6
    generic map(
      INIT => X"AAAAAAAAAAAAAACA"
    )
    port map (
      I0 => NlwRenamedSig_OI_reg_numWords_bcd_1(1),
      I1 => byteNum_1(1),
      I2 => N14,
      I3 => Mmux_cmdValid_startSeq18_354,
      I4 => Mmux_cmdValid_startSeq11_350,
      I5 => Mmux_cmdValid_startSeq17_353,
      O => reg_numWords_bcd_1_1_rstpot_386
    );
  reg_numWords_bcd_1_2_rstpot : LUT6
    generic map(
      INIT => X"AAAAAAAAAAAAAACA"
    )
    port map (
      I0 => NlwRenamedSig_OI_reg_numWords_bcd_1(2),
      I1 => byteNum_1(2),
      I2 => N14,
      I3 => Mmux_cmdValid_startSeq18_354,
      I4 => Mmux_cmdValid_startSeq11_350,
      I5 => Mmux_cmdValid_startSeq17_353,
      O => reg_numWords_bcd_1_2_rstpot_387
    );
  reg_numWords_bcd_1_3_rstpot : LUT6
    generic map(
      INIT => X"AAAAAAAAAAAAAACA"
    )
    port map (
      I0 => NlwRenamedSig_OI_reg_numWords_bcd_1(3),
      I1 => byteNum_1(3),
      I2 => N14,
      I3 => Mmux_cmdValid_startSeq18_354,
      I4 => Mmux_cmdValid_startSeq11_350,
      I5 => Mmux_cmdValid_startSeq17_353,
      O => reg_numWords_bcd_1_3_rstpot_388
    );
  reg_numWords_bcd_2_0_rstpot : LUT6
    generic map(
      INIT => X"AAAAAAAAAAAAAACA"
    )
    port map (
      I0 => NlwRenamedSig_OI_reg_numWords_bcd_2(0),
      I1 => byteNum_2(0),
      I2 => N14,
      I3 => Mmux_cmdValid_startSeq18_354,
      I4 => Mmux_cmdValid_startSeq11_350,
      I5 => Mmux_cmdValid_startSeq17_353,
      O => reg_numWords_bcd_2_0_rstpot_389
    );
  reg_numWords_bcd_2_1_rstpot : LUT6
    generic map(
      INIT => X"AAAAAAAAAAAAAACA"
    )
    port map (
      I0 => NlwRenamedSig_OI_reg_numWords_bcd_2(1),
      I1 => byteNum_2(1),
      I2 => N14,
      I3 => Mmux_cmdValid_startSeq18_354,
      I4 => Mmux_cmdValid_startSeq11_350,
      I5 => Mmux_cmdValid_startSeq17_353,
      O => reg_numWords_bcd_2_1_rstpot_390
    );
  reg_numWords_bcd_2_2_rstpot : LUT6
    generic map(
      INIT => X"AAAAAAAAAAAAAACA"
    )
    port map (
      I0 => NlwRenamedSig_OI_reg_numWords_bcd_2(2),
      I1 => byteNum_2(2),
      I2 => N14,
      I3 => Mmux_cmdValid_startSeq18_354,
      I4 => Mmux_cmdValid_startSeq11_350,
      I5 => Mmux_cmdValid_startSeq17_353,
      O => reg_numWords_bcd_2_2_rstpot_391
    );
  reg_numWords_bcd_2_3_rstpot : LUT6
    generic map(
      INIT => X"AAAAAAAAAAAAAACA"
    )
    port map (
      I0 => NlwRenamedSig_OI_reg_numWords_bcd_2(3),
      I1 => byteNum_2(3),
      I2 => N14,
      I3 => Mmux_cmdValid_startSeq18_354,
      I4 => Mmux_cmdValid_startSeq11_350,
      I5 => Mmux_cmdValid_startSeq17_353,
      O => reg_numWords_bcd_2_3_rstpot_392
    );
  Mmux_cmdValid_startSeq19_SW3 : LUT3
    generic map(
      INIT => X"DF"
    )
    port map (
      I0 => curState_FSM_FFd18_239,
      I1 => reset,
      I2 => rxnow,
      O => N49
    );
  Mmux_cmdValid_startSeq19_SW4 : LUT4
    generic map(
      INIT => X"ABFF"
    )
    port map (
      I0 => reset,
      I1 => curState_FSM_FFd14_242,
      I2 => curState_FSM_FFd18_239,
      I3 => rxnow,
      O => N50
    );
  curState_FSM_FFd17_rstpot : LUT6
    generic map(
      INIT => X"553355335533550F"
    )
    port map (
      I0 => N49,
      I1 => N50,
      I2 => N48,
      I3 => N16,
      I4 => Mmux_cmdValid_startSeq11_350,
      I5 => Mmux_cmdValid_startSeq17_353,
      O => curState_FSM_FFd17_rstpot_375
    );
  Mmux_cmdValid_startSeq19_SW2 : MUXF7
    port map (
      I0 => N52,
      I1 => N53,
      S => Mmux_cmdValid_startSeq18_354,
      O => N48
    );
  Mmux_cmdValid_startSeq19_SW2_F : LUT6
    generic map(
      INIT => X"FFFFFFFF5111FFFF"
    )
    port map (
      I0 => curState_FSM_FFd18_239,
      I1 => curState_FSM_FFd14_242,
      I2 => byteNum_0(5),
      I3 => byteNum_0(4),
      I4 => rxnow,
      I5 => reset,
      O => N52
    );
  Mmux_cmdValid_startSeq19_SW2_G : LUT4
    generic map(
      INIT => X"ABFF"
    )
    port map (
      I0 => reset,
      I1 => curState_FSM_FFd14_242,
      I2 => curState_FSM_FFd18_239,
      I3 => rxnow,
      O => N53
    );
  dataStarted_glue_rst : LUT4
    generic map(
      INIT => X"CF8A"
    )
    port map (
      I0 => Q_n0472_inv,
      I1 => reset,
      I2 => NlwRenamedSig_OI_curState_FSM_FFd9,
      I3 => dataStarted_194,
      O => dataStarted_glue_rst_370
    );
  dataComplete_glue_rst_SW0 : LUT2
    generic map(
      INIT => X"E"
    )
    port map (
      I0 => nibbleCount(2),
      I1 => reset,
      O => N54
    );
  dataComplete_glue_rst : LUT6
    generic map(
      INIT => X"FFFFFFFFAAAAA8AA"
    )
    port map (
      I0 => dataComplete_158,
      I1 => lastByte_195,
      I2 => nibbleCount(0),
      I3 => nibbleCount(1),
      I4 => N54,
      I5 => NlwRenamedSig_OI_curState_FSM_FFd9,
      O => dataComplete_glue_rst_371
    );
  Mmux_txData63_SW0 : LUT6
    generic map(
      INIT => X"FFFFFFFFCE8ADF8A"
    )
    port map (
      I0 => printResults_INV_61_o,
      I1 => Q_n0435,
      I2 => Q_n0437(6),
      I3 => printPeak_INV_62_o,
      I4 => Q_n0440(6),
      I5 => Mmux_txData511_308,
      O => N56
    );
  Mmux_txData63 : LUT6
    generic map(
      INIT => X"FDDDFDDDA888FDDD"
    )
    port map (
      I0 => dataStarted_194,
      I1 => N56,
      I2 => Mmux_byteNum_0_7_GND_8_o_mux_218_OUT311,
      I3 => byteNum_0(5),
      I4 => Q_n0434(6),
      I5 => Q_n0435,
      O => txData(5)
    );
  Mmux_txData29_SW0 : LUT6
    generic map(
      INIT => X"FFFFFFFF55551114"
    )
    port map (
      I0 => Mmux_txData24_326,
      I1 => reg_dataResults_3(5),
      I2 => reg_dataResults_3_7_PWR_8_o_LessThan_203_o,
      I3 => reg_dataResults_3(4),
      I4 => nibbleCount(0),
      I5 => nibbleCount(2),
      O => N58
    );
  Mmux_txData29 : LUT6
    generic map(
      INIT => X"FFFFFFFF57020202"
    )
    port map (
      I0 => GND_8_o_peakByteCount_2_equal_201_o,
      I1 => nibbleCount(1),
      I2 => N58,
      I3 => Mcount_peakByteCount1,
      I4 => reg_maxIndex_2(1),
      I5 => Mmux_txData26,
      O => Mmux_txData28
    );
  Mmux_txData18 : LUT6
    generic map(
      INIT => X"F888F888FFFFF888"
    )
    port map (
      I0 => Mmux_byteNum_0_7_GND_8_o_mux_218_OUT312,
      I1 => GND_8_o_X_8_o_wide_mux_215_OUT(2),
      I2 => byteNum_0(0),
      I3 => Mmux_byteNum_0_7_GND_8_o_mux_218_OUT311,
      I4 => printPeak_INV_62_o,
      I5 => N60,
      O => Mmux_txData17
    );
  Mmux_txData38_SW0 : LUT6
    generic map(
      INIT => X"4445444455555555"
    )
    port map (
      I0 => Mmux_txData33_340,
      I1 => nibbleCount(0),
      I2 => reg_dataResults_3(5),
      I3 => reg_dataResults_3(4),
      I4 => reg_dataResults_3(7),
      I5 => reg_dataResults_3(6),
      O => N62
    );
  Mmux_txData38 : LUT6
    generic map(
      INIT => X"FFFFFFFF57020202"
    )
    port map (
      I0 => GND_8_o_peakByteCount_2_equal_201_o,
      I1 => Q_n0435,
      I2 => N62,
      I3 => Mcount_peakByteCount1,
      I4 => reg_maxIndex_2(2),
      I5 => Mmux_txData35,
      O => Mmux_txData37
    );
  curState_FSM_FFd3_In1_SW0 : LUT6
    generic map(
      INIT => X"FFFFFFFFFEEE5555"
    )
    port map (
      I0 => peakByteCount(2),
      I1 => nibbleCount(2),
      I2 => nibbleCount(1),
      I3 => nibbleCount(0),
      I4 => peakByteCount(0),
      I5 => peakByteCount(1),
      O => N4
    );
  Mcount_ctrlByteCount_val1 : LUT6
    generic map(
      INIT => X"FFFFFFFF00080000"
    )
    port map (
      I0 => curState_FSM_FFd13_203,
      I1 => ctrlByteCount(3),
      I2 => ctrlByteCount(2),
      I3 => ctrlByteCount(1),
      I4 => ctrlByteCount(0),
      I5 => reset,
      O => Mcount_ctrlByteCount_val
    );
  curState_FSM_FFd8_In1 : LUT6
    generic map(
      INIT => X"FFFFFFFF00080808"
    )
    port map (
      I0 => curState_FSM_FFd7_245,
      I1 => txdone,
      I2 => nibbleCount(2),
      I3 => nibbleCount(1),
      I4 => nibbleCount(0),
      I5 => curState_FSM_FFd6_201,
      O => curState_FSM_FFd8_In
    );
  curState_FSM_FFd17_In11_SW1 : LUT5
    generic map(
      INIT => X"00000140"
    )
    port map (
      I0 => byteNum_0(0),
      I1 => byteNum_0(3),
      I2 => byteNum_0(2),
      I3 => byteNum_0(4),
      I4 => N22,
      O => N16
    );
  GND_8_o_X_8_o_wide_mux_215_OUT_2_1 : LUT4
    generic map(
      INIT => X"FFEB"
    )
    port map (
      I0 => ctrlByteCount(1),
      I1 => ctrlByteCount(3),
      I2 => ctrlByteCount(0),
      I3 => ctrlByteCount(2),
      O => GND_8_o_X_8_o_wide_mux_215_OUT(2)
    );
  Mmux_txData44 : LUT6
    generic map(
      INIT => X"F7D59191E6C48080"
    )
    port map (
      I0 => peakByteCount(0),
      I1 => peakByteCount(1),
      I2 => reg_maxIndex_1(3),
      I3 => peakByteCount(2),
      I4 => reg_maxIndex_2(3),
      I5 => reg_maxIndex_0(3),
      O => Mmux_txData43_356
    );
  Mmux_txData25 : LUT5
    generic map(
      INIT => X"88288808"
    )
    port map (
      I0 => nibbleCount(0),
      I1 => reg_dataResults_3(1),
      I2 => reg_dataResults_3(3),
      I3 => reg_dataResults_3(0),
      I4 => reg_dataResults_3(2),
      O => Mmux_txData24_326
    );
  Mmux_txData212 : LUT5
    generic map(
      INIT => X"88288808"
    )
    port map (
      I0 => nibbleCount(0),
      I1 => n0366_1_Q,
      I2 => Msub_GND_8_o_GND_8_o_sub_197_OUT_3_0_lut_3_Q,
      I3 => Msub_GND_8_o_GND_8_o_sub_197_OUT_3_0_cy_0_Q,
      I4 => n0366_2_Q,
      O => Mmux_txData211_331
    );
  Mmux_txData213 : LUT5
    generic map(
      INIT => X"44144404"
    )
    port map (
      I0 => nibbleCount(0),
      I1 => n0366_5_Q,
      I2 => Msub_GND_8_o_GND_8_o_sub_187_OUT_3_0_lut_3_Q,
      I3 => Msub_GND_8_o_GND_8_o_sub_187_OUT_3_0_cy_0_Q,
      I4 => n0366_6_Q,
      O => Mmux_txData212_332
    );
  Mmux_n044071 : LUT5
    generic map(
      INIT => X"8880DDD5"
    )
    port map (
      I0 => nibbleCount(0),
      I1 => reg_dataResults_3(3),
      I2 => reg_dataResults_3(1),
      I3 => reg_dataResults_3(2),
      I4 => reg_dataResults_3_7_PWR_8_o_LessThan_203_o,
      O => Q_n0440(6)
    );
  Mmux_byteNum_0_7_GND_8_o_mux_218_OUT3121 : LUT6
    generic map(
      INIT => X"0001000100010000"
    )
    port map (
      I0 => curState_FSM_FFd2_206,
      I1 => curState_FSM_FFd3_202,
      I2 => curState_FSM_FFd1_248,
      I3 => printResults_INV_61_o,
      I4 => curState_FSM_FFd4_247,
      I5 => curState_FSM_FFd5_246,
      O => Mmux_byteNum_0_7_GND_8_o_mux_218_OUT312
    );
  Mmux_txData81 : LUT6
    generic map(
      INIT => X"0000000000000008"
    )
    port map (
      I0 => dataStarted_194,
      I1 => byteNum_0(7),
      I2 => printPeak_INV_62_o,
      I3 => printResults_INV_61_o,
      I4 => curState_FSM_FFd4_247,
      I5 => curState_FSM_FFd5_246,
      O => txData(7)
    );
  Mmux_n043771 : LUT5
    generic map(
      INIT => X"8880DDD5"
    )
    port map (
      I0 => nibbleCount(0),
      I1 => Msub_GND_8_o_GND_8_o_sub_197_OUT_3_0_lut_3_Q,
      I2 => n0366_1_Q,
      I3 => n0366_2_Q,
      I4 => GND_8_o_PWR_8_o_LessThan_182_o,
      O => Q_n0437(6)
    );
  Mmux_n043471 : LUT5
    generic map(
      INIT => X"8880DDD5"
    )
    port map (
      I0 => nibbleCount(0),
      I1 => byte(3),
      I2 => byte(1),
      I3 => byte(2),
      I4 => byte_7_PWR_8_o_LessThan_170_o,
      O => Q_n0434(6)
    );
  Mmux_txData11 : LUT4
    generic map(
      INIT => X"666C"
    )
    port map (
      I0 => byte(7),
      I1 => byte(4),
      I2 => byte(5),
      I3 => byte(6),
      O => Mmux_txData1
    );
  Mmux_txData410 : LUT6
    generic map(
      INIT => X"1010001010000000"
    )
    port map (
      I0 => n0366_2_Q,
      I1 => n0366_1_Q,
      I2 => nibbleCount(0),
      I3 => byteCount(1),
      I4 => Mmux_n03664,
      I5 => Mmux_n036641_323,
      O => Mmux_txData49
    );
  Mmux_txData411 : LUT6
    generic map(
      INIT => X"0000000404000404"
    )
    port map (
      I0 => nibbleCount(0),
      I1 => Msub_GND_8_o_GND_8_o_sub_187_OUT_3_0_lut_3_Q,
      I2 => n0366_5_Q,
      I3 => byteCount(1),
      I4 => Mmux_n036671_313,
      I5 => Mmux_n03667,
      O => Mmux_txData410_360
    );
  Mmux_txData51 : LUT6
    generic map(
      INIT => X"FFFFFFFF00000002"
    )
    port map (
      I0 => byteNum_0(4),
      I1 => printPeak_INV_62_o,
      I2 => printResults_INV_61_o,
      I3 => curState_FSM_FFd4_247,
      I4 => curState_FSM_FFd5_246,
      I5 => Mmux_txData511_308,
      O => Mmux_txData5
    );
  Mmux_txData52 : LUT6
    generic map(
      INIT => X"5554555755545554"
    )
    port map (
      I0 => Q_n0437(6),
      I1 => curState_FSM_FFd6_201,
      I2 => curState_FSM_FFd8_205,
      I3 => curState_FSM_FFd7_245,
      I4 => Q_n0440(6),
      I5 => printPeak_INV_62_o,
      O => Mmux_txData51_369
    );
  Mmux_txData53 : LUT6
    generic map(
      INIT => X"888888A8888888FD"
    )
    port map (
      I0 => dataStarted_194,
      I1 => Mmux_txData5,
      I2 => Mmux_txData51_369,
      I3 => nibbleCount(2),
      I4 => nibbleCount(1),
      I5 => Q_n0434(6),
      O => txData(4)
    );
  Mmux_txData15 : LUT6
    generic map(
      INIT => X"F7D59191E6C48080"
    )
    port map (
      I0 => peakByteCount(0),
      I1 => peakByteCount(1),
      I2 => reg_maxIndex_1(0),
      I3 => peakByteCount(2),
      I4 => reg_maxIndex_2(0),
      I5 => reg_maxIndex_0(0),
      O => Mmux_txData14
    );
  peakByteCount_0_glue_set : LUT3
    generic map(
      INIT => X"F6"
    )
    port map (
      I0 => curState_FSM_FFd3_202,
      I1 => peakByteCount(0),
      I2 => reset,
      O => peakByteCount_0_glue_set_372
    );
  byteCount_0_glue_set : LUT5
    generic map(
      INIT => X"FFFFD5AA"
    )
    port map (
      I0 => curState_FSM_FFd6_201,
      I1 => byteCount(1),
      I2 => byteCount(2),
      I3 => byteCount(0),
      I4 => reset,
      O => byteCount_0_glue_set_373
    );
  lastByte_rstpot : LUT5
    generic map(
      INIT => X"FFFFE0EE"
    )
    port map (
      I0 => NlwRenamedSig_OI_curState_FSM_FFd9,
      I1 => lastByte_195,
      I2 => dataStarted_194,
      I3 => seqDone,
      I4 => reset,
      O => lastByte_rstpot_376
    );
  Mmux_txData24 : LUT5
    generic map(
      INIT => X"00040400"
    )
    port map (
      I0 => ctrlByteCount(1),
      I1 => Mmux_byteNum_0_7_GND_8_o_mux_218_OUT312,
      I2 => ctrlByteCount(2),
      I3 => ctrlByteCount(0),
      I4 => ctrlByteCount(3),
      O => Mmux_txData23_325
    );
  Mmux_txData27 : LUT4
    generic map(
      INIT => X"9810"
    )
    port map (
      I0 => peakByteCount(1),
      I1 => peakByteCount(0),
      I2 => reg_maxIndex_0(1),
      I3 => reg_maxIndex_1(1),
      O => Mmux_txData26
    );
  Mmux_txData36 : LUT4
    generic map(
      INIT => X"9810"
    )
    port map (
      I0 => peakByteCount(1),
      I1 => peakByteCount(0),
      I2 => reg_maxIndex_0(2),
      I3 => reg_maxIndex_1(2),
      O => Mmux_txData35
    );
  Mmux_byteNum_0_7_GND_8_o_mux_218_OUT3111 : LUT6
    generic map(
      INIT => X"0000000000000001"
    )
    port map (
      I0 => curState_FSM_FFd2_206,
      I1 => curState_FSM_FFd3_202,
      I2 => curState_FSM_FFd1_248,
      I3 => printResults_INV_61_o,
      I4 => curState_FSM_FFd4_247,
      I5 => curState_FSM_FFd5_246,
      O => Mmux_byteNum_0_7_GND_8_o_mux_218_OUT311
    );
  Mmux_txData18_SW0 : LUT6
    generic map(
      INIT => X"FFFFFFFF55515555"
    )
    port map (
      I0 => Mmux_txData14,
      I1 => GND_8_o_peakByteCount_2_equal_201_o,
      I2 => nibbleCount(2),
      I3 => nibbleCount(1),
      I4 => Mmux_txData15_336,
      I5 => printResults_INV_61_o,
      O => N60
    );
  Mmux_txData211 : LUT5
    generic map(
      INIT => X"11111110"
    )
    port map (
      I0 => nibbleCount(2),
      I1 => nibbleCount(1),
      I2 => curState_FSM_FFd8_205,
      I3 => curState_FSM_FFd6_201,
      I4 => curState_FSM_FFd7_245,
      O => Mmux_txData210_330
    );
  Mmux_txData39 : LUT6
    generic map(
      INIT => X"0101010101010100"
    )
    port map (
      I0 => curState_FSM_FFd7_245,
      I1 => curState_FSM_FFd8_205,
      I2 => curState_FSM_FFd6_201,
      I3 => curState_FSM_FFd3_202,
      I4 => curState_FSM_FFd1_248,
      I5 => curState_FSM_FFd2_206,
      O => Mmux_txData38_343
    );
  ctrlByteCount_0_rstpot : LUT6
    generic map(
      INIT => X"6666666666666E66"
    )
    port map (
      I0 => curState_FSM_FFd13_203,
      I1 => ctrlByteCount(0),
      I2 => reset,
      I3 => ctrlByteCount(3),
      I4 => ctrlByteCount(2),
      I5 => ctrlByteCount(1),
      O => ctrlByteCount_0_rstpot_374
    );
  GND_8_o_PWR_8_o_LessThan_182_o1 : LUT6
    generic map(
      INIT => X"02AA57FF07AF57FF"
    )
    port map (
      I0 => byteCount(1),
      I1 => Mmux_n03667,
      I2 => n0366_5_Q,
      I3 => Mmux_n03668,
      I4 => Mmux_n036681_315,
      I5 => Mmux_n036671_313,
      O => GND_8_o_PWR_8_o_LessThan_182_o
    );
  GND_8_o_PWR_8_o_LessThan_192_o1 : LUT6
    generic map(
      INIT => X"02AA57FF07AF57FF"
    )
    port map (
      I0 => byteCount(1),
      I1 => Mmux_n03663,
      I2 => n0366_1_Q,
      I3 => Mmux_n03664,
      I4 => Mmux_n036641_323,
      I5 => Mmux_n036631_321,
      O => GND_8_o_PWR_8_o_LessThan_192_o
    );
  Mcount_nibbleCount_xor_0_11_INV_0 : INV
    port map (
      I => nibbleCount(0),
      O => Result(0)
    );
  Mmux_n036653 : MUXF7
    port map (
      I0 => N64,
      I1 => N65,
      S => byteCount(0),
      O => Msub_GND_8_o_GND_8_o_sub_187_OUT_3_0_cy_0_Q
    );
  Mmux_n036653_F : LUT6
    generic map(
      INIT => X"FD75B931EC64A820"
    )
    port map (
      I0 => byteCount(2),
      I1 => byteCount(1),
      I2 => reg_dataResults_3(4),
      I3 => reg_dataResults_1(4),
      I4 => reg_dataResults_5(4),
      I5 => reg_dataResults_6(4),
      O => N64
    );
  Mmux_n036653_G : LUT6
    generic map(
      INIT => X"FD75B931EC64A820"
    )
    port map (
      I0 => byteCount(1),
      I1 => byteCount(2),
      I2 => reg_dataResults_4(4),
      I3 => reg_dataResults_0(4),
      I4 => reg_dataResults_2(4),
      I5 => reg_dataResults_6(4),
      O => N65
    );
  Mmux_txData47 : MUXF7
    port map (
      I0 => N66,
      I1 => N67,
      S => nibbleCount(0),
      O => Mmux_txData46
    );
  Mmux_txData47_F : LUT5
    generic map(
      INIT => X"00001000"
    )
    port map (
      I0 => Q_n0435,
      I1 => reg_dataResults_3(6),
      I2 => reg_dataResults_3(7),
      I3 => GND_8_o_peakByteCount_2_equal_201_o,
      I4 => reg_dataResults_3(5),
      O => N66
    );
  Mmux_txData47_G : LUT5
    generic map(
      INIT => X"00001000"
    )
    port map (
      I0 => Q_n0435,
      I1 => reg_dataResults_3(2),
      I2 => reg_dataResults_3(3),
      I3 => GND_8_o_peakByteCount_2_equal_201_o,
      I4 => reg_dataResults_3(1),
      O => N67
    );
  Mmux_txData33 : MUXF7
    port map (
      I0 => N68,
      I1 => N69,
      S => nibbleCount(0),
      O => Mmux_txData32
    );
  Mmux_txData33_F : LUT6
    generic map(
      INIT => X"1010101010001010"
    )
    port map (
      I0 => dataStarted_194,
      I1 => Q_n0435,
      I2 => byte(6),
      I3 => byte(4),
      I4 => byte(7),
      I5 => byte(5),
      O => N68
    );
  Mmux_txData33_G : LUT6
    generic map(
      INIT => X"1010101010001010"
    )
    port map (
      I0 => dataStarted_194,
      I1 => Q_n0435,
      I2 => byte(2),
      I3 => byte(0),
      I4 => byte(3),
      I5 => byte(1),
      O => N69
    );
  Mmux_txData23 : MUXF7
    port map (
      I0 => N70,
      I1 => N71,
      S => nibbleCount(0),
      O => Mmux_txData22
    );
  Mmux_txData23_F : LUT6
    generic map(
      INIT => X"0100010001000001"
    )
    port map (
      I0 => dataStarted_194,
      I1 => nibbleCount(2),
      I2 => nibbleCount(1),
      I3 => byte(5),
      I4 => byte_7_PWR_8_o_LessThan_170_o,
      I5 => byte(4),
      O => N70
    );
  Mmux_txData23_G : LUT6
    generic map(
      INIT => X"0100010001000001"
    )
    port map (
      I0 => dataStarted_194,
      I1 => nibbleCount(2),
      I2 => nibbleCount(1),
      I3 => byte(1),
      I4 => byte_3_PWR_8_o_LessThan_174_o,
      I5 => byte(0),
      O => N71
    );
  Mmux_txData43 : MUXF7
    port map (
      I0 => N72,
      I1 => N73,
      S => nibbleCount(0),
      O => Mmux_txData42
    );
  Mmux_txData43_F : LUT6
    generic map(
      INIT => X"0000000000000010"
    )
    port map (
      I0 => dataStarted_194,
      I1 => nibbleCount(2),
      I2 => byte(7),
      I3 => nibbleCount(1),
      I4 => byte(6),
      I5 => byte(5),
      O => N72
    );
  Mmux_txData43_G : LUT6
    generic map(
      INIT => X"0000000000000010"
    )
    port map (
      I0 => dataStarted_194,
      I1 => nibbleCount(2),
      I2 => byte(3),
      I3 => nibbleCount(1),
      I4 => byte(1),
      I5 => byte(2),
      O => N73
    );
  curState_FSM_FFd17_In11_SW0 : MUXF7
    port map (
      I0 => N74,
      I1 => Mcount_byteCount_lut(3),
      S => byteNum_0(1),
      O => N12
    );
  curState_FSM_FFd17_In11_SW0_F : LUT6
    generic map(
      INIT => X"0004000000000400"
    )
    port map (
      I0 => byteNum_0(7),
      I1 => byteNum_0(6),
      I2 => byteNum_0(0),
      I3 => byteNum_0(4),
      I4 => byteNum_0(2),
      I5 => byteNum_0(3),
      O => N74
    );

end Structure;

