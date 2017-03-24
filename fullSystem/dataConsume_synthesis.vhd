--------------------------------------------------------------------------------
-- Copyright (c) 1995-2012 Xilinx, Inc.  All rights reserved.
--------------------------------------------------------------------------------
--   ____  ____
--  /   /\/   /
-- /___/  \  /    Vendor: Xilinx
-- \   \   \/     Version: P.15xf
--  \   \         Application: netgen
--  /   /         Filename: dataConsume_synthesis.vhd
-- /___/   /\     Timestamp: Mon Feb 03 16:10:05 2014
-- \   \  /  \ 
--  \___\/\___\
--             
-- Command	: -intstyle ise -ar Structure -tm dataConsume -w -dir netgen/synthesis -ofmt vhdl -sim dataConsume.ngc dataConsume_synthesis.vhd 
-- Device	: xc6slx16-3-csg324
-- Input file	: dataConsume.ngc
-- Output file	: \\ads.bris.ac.uk\filestore\myfiles\staff\eezgz\Documents\EENG28010\Source Code\peakdetector\Projects\data_Consume1\netgen\synthesis\dataConsume_synthesis.vhd
-- # of Entities	: 1
-- Design Name	: dataConsume
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

entity dataConsume_synthesised is
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
end dataConsume_synthesised;

architecture Structure of dataConsume_synthesised is
  signal ctrlIn_delayed_56 : STD_LOGIC; 
  signal curState_FSM_FFd2_57 : STD_LOGIC; 
  signal curState_FSM_FFd3_58 : STD_LOGIC; 
  signal curState_FSM_FFd1_59 : STD_LOGIC; 
  signal NlwRenamedSig_OI_dataReady : STD_LOGIC; 
  signal sig_seqDone_delayed_61 : STD_LOGIC; 
  signal NlwRenamedSig_OI_ctrlOut_reg : STD_LOGIC; 
  signal startUpdate_131 : STD_LOGIC; 
  signal reset_sig_seqDone_delayed_OR_12_o : STD_LOGIC; 
  signal NlwRenamedSig_OI_seqDone : STD_LOGIC; 
  signal Q_n0310_inv : STD_LOGIC; 
  signal Q_n0304_inv : STD_LOGIC; 
  signal Q_n0316_inv : STD_LOGIC; 
  signal Q_n0374_inv_137 : STD_LOGIC; 
  signal curState_FSM_FFd3_In_138 : STD_LOGIC; 
  signal curState_FSM_FFd2_In : STD_LOGIC; 
  signal curState_FSM_FFd1_In : STD_LOGIC; 
  signal Mcount_updateCount_val : STD_LOGIC; 
  signal sig_grtThan1 : STD_LOGIC; 
  signal curState_FSM_FFd3_In1 : STD_LOGIC; 
  signal Q_n0374_inv1_158 : STD_LOGIC; 
  signal N01 : STD_LOGIC; 
  signal seqDone1_160 : STD_LOGIC; 
  signal seqDone2_161 : STD_LOGIC; 
  signal seqDone3_162 : STD_LOGIC; 
  signal seqDone4_163 : STD_LOGIC; 
  signal N2 : STD_LOGIC; 
  signal N3 : STD_LOGIC; 
  signal N5 : STD_LOGIC; 
  signal ctrlOut_reg_rstpot_167 : STD_LOGIC; 
  signal sig_seqDone_delayed_rstpot_168 : STD_LOGIC; 
  signal startUpdate_rstpot_169 : STD_LOGIC; 
  signal N9 : STD_LOGIC; 
  signal bcd_wordCounting_var_bcdCount_2_0_rstpot_171 : STD_LOGIC; 
  signal bcd_wordCounting_var_bcdCount_2_2_rstpot_172 : STD_LOGIC; 
  signal bcd_wordCounting_var_bcdCount_2_1_rstpot_173 : STD_LOGIC; 
  signal bcd_wordCounting_var_bcdCount_2_3_rstpot_174 : STD_LOGIC; 
  signal N11 : STD_LOGIC; 
  signal N12 : STD_LOGIC; 
  signal Q_n0409_inv1_rstpot_177 : STD_LOGIC; 
  signal Q_n0409_inv1_cepot : STD_LOGIC; 
  signal bcd_wordCounting_var_bcdCount_0_0_dpot_179 : STD_LOGIC; 
  signal bcd_wordCounting_var_bcdCount_0_1_dpot_180 : STD_LOGIC; 
  signal bcd_wordCounting_var_bcdCount_0_2_dpot_181 : STD_LOGIC; 
  signal bcd_wordCounting_var_bcdCount_0_3_dpot_182 : STD_LOGIC; 
  signal bcd_wordCounting_var_bcdCount_1_0_rstpot_183 : STD_LOGIC; 
  signal bcd_wordCounting_var_bcdCount_1_2_rstpot_184 : STD_LOGIC; 
  signal bcd_wordCounting_var_bcdCount_1_1_rstpot_185 : STD_LOGIC; 
  signal bcd_wordCounting_var_bcdCount_1_3_rstpot_186 : STD_LOGIC; 
  signal N14 : STD_LOGIC; 
  signal N15 : STD_LOGIC; 
  signal N17 : STD_LOGIC; 
  signal N18 : STD_LOGIC; 
  signal N20 : STD_LOGIC; 
  signal N21 : STD_LOGIC; 
  signal N23 : STD_LOGIC; 
  signal N24 : STD_LOGIC; 
  signal Q_n0324_inv1_rstpot_195 : STD_LOGIC; 
  signal dataArraySaved_3_0_dpot_196 : STD_LOGIC; 
  signal dataArraySaved_3_1_dpot_197 : STD_LOGIC; 
  signal dataArraySaved_3_2_dpot_198 : STD_LOGIC; 
  signal dataArraySaved_3_3_dpot_199 : STD_LOGIC; 
  signal dataArraySaved_3_4_dpot_200 : STD_LOGIC; 
  signal dataArraySaved_3_5_dpot_201 : STD_LOGIC; 
  signal dataArraySaved_3_6_dpot_202 : STD_LOGIC; 
  signal dataArraySaved_3_7_dpot_203 : STD_LOGIC; 
  signal maxIndex_reg_2_0_dpot_204 : STD_LOGIC; 
  signal maxIndex_reg_2_1_dpot_205 : STD_LOGIC; 
  signal maxIndex_reg_2_2_dpot_206 : STD_LOGIC; 
  signal maxIndex_reg_2_3_dpot_207 : STD_LOGIC; 
  signal maxIndex_reg_0_0_dpot_208 : STD_LOGIC; 
  signal maxIndex_reg_0_1_dpot_209 : STD_LOGIC; 
  signal maxIndex_reg_0_2_dpot_210 : STD_LOGIC; 
  signal maxIndex_reg_0_3_dpot_211 : STD_LOGIC; 
  signal maxIndex_reg_1_0_dpot_212 : STD_LOGIC; 
  signal maxIndex_reg_1_1_dpot_213 : STD_LOGIC; 
  signal maxIndex_reg_1_2_dpot_214 : STD_LOGIC; 
  signal maxIndex_reg_1_3_dpot_215 : STD_LOGIC; 
  signal dataArraySaved_6_0_dpot_216 : STD_LOGIC; 
  signal dataArraySaved_6_1_dpot_217 : STD_LOGIC; 
  signal dataArraySaved_6_2_dpot_218 : STD_LOGIC; 
  signal dataArraySaved_6_3_dpot_219 : STD_LOGIC; 
  signal dataArraySaved_6_4_dpot_220 : STD_LOGIC; 
  signal dataArraySaved_6_5_dpot_221 : STD_LOGIC; 
  signal dataArraySaved_6_6_dpot_222 : STD_LOGIC; 
  signal dataArraySaved_6_7_dpot_223 : STD_LOGIC; 
  signal dataArraySaved_5_0_dpot_224 : STD_LOGIC; 
  signal dataArraySaved_5_1_dpot_225 : STD_LOGIC; 
  signal dataArraySaved_5_2_dpot_226 : STD_LOGIC; 
  signal dataArraySaved_5_3_dpot_227 : STD_LOGIC; 
  signal dataArraySaved_5_4_dpot_228 : STD_LOGIC; 
  signal dataArraySaved_5_5_dpot_229 : STD_LOGIC; 
  signal dataArraySaved_5_6_dpot_230 : STD_LOGIC; 
  signal dataArraySaved_5_7_dpot_231 : STD_LOGIC; 
  signal dataArraySaved_4_0_dpot_232 : STD_LOGIC; 
  signal dataArraySaved_4_1_dpot_233 : STD_LOGIC; 
  signal dataArraySaved_4_2_dpot_234 : STD_LOGIC; 
  signal dataArraySaved_4_3_dpot_235 : STD_LOGIC; 
  signal dataArraySaved_4_4_dpot_236 : STD_LOGIC; 
  signal dataArraySaved_4_5_dpot_237 : STD_LOGIC; 
  signal dataArraySaved_4_6_dpot_238 : STD_LOGIC; 
  signal dataArraySaved_4_7_dpot_239 : STD_LOGIC; 
  signal curState_FSM_FFd1_1_240 : STD_LOGIC; 
  signal curState_FSM_FFd2_1_241 : STD_LOGIC; 
  signal curState_FSM_FFd3_1_242 : STD_LOGIC; 
  signal updateCount_1_rstpot_243 : STD_LOGIC; 
  signal updateCount_0_rstpot_244 : STD_LOGIC; 
  signal curState_FSM_FFd1_2_245 : STD_LOGIC; 
  signal NlwRenamedSignal_byte : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal dataArraySaved_0 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal dataArraySaved_1 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal dataArraySaved_2 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal dataArrayCurrent_0 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal NlwRenamedSig_OI_dataArraySaved_3 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal NlwRenamedSig_OI_dataArraySaved_4 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal NlwRenamedSig_OI_dataArraySaved_5 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal NlwRenamedSig_OI_dataArraySaved_6 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal dataArrayCurrent_1 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal dataArrayCurrent_2 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal dataArrayCurrent_3 : STD_LOGIC_VECTOR ( 7 downto 0 ); 
  signal NlwRenamedSig_OI_maxIndex_reg_2 : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal NlwRenamedSig_OI_maxIndex_reg_1 : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal NlwRenamedSig_OI_maxIndex_reg_0 : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal bcd_wordCounting_var_bcdCount_1 : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal bcd_wordCounting_var_bcdCount_2 : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal bcd_wordCounting_var_bcdCount_0 : STD_LOGIC_VECTOR ( 3 downto 0 ); 
  signal updateCount : STD_LOGIC_VECTOR ( 1 downto 0 ); 
begin
  NlwRenamedSignal_byte(7) <= data(7);
  NlwRenamedSignal_byte(6) <= data(6);
  NlwRenamedSignal_byte(5) <= data(5);
  NlwRenamedSignal_byte(4) <= data(4);
  NlwRenamedSignal_byte(3) <= data(3);
  NlwRenamedSignal_byte(2) <= data(2);
  NlwRenamedSignal_byte(1) <= data(1);
  NlwRenamedSignal_byte(0) <= data(0);
  byte(7) <= NlwRenamedSignal_byte(7);
  byte(6) <= NlwRenamedSignal_byte(6);
  byte(5) <= NlwRenamedSignal_byte(5);
  byte(4) <= NlwRenamedSignal_byte(4);
  byte(3) <= NlwRenamedSignal_byte(3);
  byte(2) <= NlwRenamedSignal_byte(2);
  byte(1) <= NlwRenamedSignal_byte(1);
  byte(0) <= NlwRenamedSignal_byte(0);
  maxIndex(2, 3) <= NlwRenamedSig_OI_maxIndex_reg_2(3);
  maxIndex(2, 2) <= NlwRenamedSig_OI_maxIndex_reg_2(2);
  maxIndex(2, 1) <= NlwRenamedSig_OI_maxIndex_reg_2(1);
  maxIndex(2, 0) <= NlwRenamedSig_OI_maxIndex_reg_2(0);
  maxIndex(1, 3) <= NlwRenamedSig_OI_maxIndex_reg_1(3);
  maxIndex(1, 2) <= NlwRenamedSig_OI_maxIndex_reg_1(2);
  maxIndex(1, 1) <= NlwRenamedSig_OI_maxIndex_reg_1(1);
  maxIndex(1, 0) <= NlwRenamedSig_OI_maxIndex_reg_1(0);
  maxIndex(0, 3) <= NlwRenamedSig_OI_maxIndex_reg_0(3);
  maxIndex(0, 2) <= NlwRenamedSig_OI_maxIndex_reg_0(2);
  maxIndex(0, 1) <= NlwRenamedSig_OI_maxIndex_reg_0(1);
  maxIndex(0, 0) <= NlwRenamedSig_OI_maxIndex_reg_0(0);
  dataResults(0, 7) <= dataArraySaved_0(7);
  dataResults(0, 6) <= dataArraySaved_0(6);
  dataResults(0, 5) <= dataArraySaved_0(5);
  dataResults(0, 4) <= dataArraySaved_0(4);
  dataResults(0, 3) <= dataArraySaved_0(3);
  dataResults(0, 2) <= dataArraySaved_0(2);
  dataResults(0, 1) <= dataArraySaved_0(1);
  dataResults(0, 0) <= dataArraySaved_0(0);
  dataResults(1, 7) <= dataArraySaved_1(7);
  dataResults(1, 6) <= dataArraySaved_1(6);
  dataResults(1, 5) <= dataArraySaved_1(5);
  dataResults(1, 4) <= dataArraySaved_1(4);
  dataResults(1, 3) <= dataArraySaved_1(3);
  dataResults(1, 2) <= dataArraySaved_1(2);
  dataResults(1, 1) <= dataArraySaved_1(1);
  dataResults(1, 0) <= dataArraySaved_1(0);
  dataResults(2, 7) <= dataArraySaved_2(7);
  dataResults(2, 6) <= dataArraySaved_2(6);
  dataResults(2, 5) <= dataArraySaved_2(5);
  dataResults(2, 4) <= dataArraySaved_2(4);
  dataResults(2, 3) <= dataArraySaved_2(3);
  dataResults(2, 2) <= dataArraySaved_2(2);
  dataResults(2, 1) <= dataArraySaved_2(1);
  dataResults(2, 0) <= dataArraySaved_2(0);
  dataResults(3, 7) <= NlwRenamedSig_OI_dataArraySaved_3(7);
  dataResults(3, 6) <= NlwRenamedSig_OI_dataArraySaved_3(6);
  dataResults(3, 5) <= NlwRenamedSig_OI_dataArraySaved_3(5);
  dataResults(3, 4) <= NlwRenamedSig_OI_dataArraySaved_3(4);
  dataResults(3, 3) <= NlwRenamedSig_OI_dataArraySaved_3(3);
  dataResults(3, 2) <= NlwRenamedSig_OI_dataArraySaved_3(2);
  dataResults(3, 1) <= NlwRenamedSig_OI_dataArraySaved_3(1);
  dataResults(3, 0) <= NlwRenamedSig_OI_dataArraySaved_3(0);
  dataResults(4, 7) <= NlwRenamedSig_OI_dataArraySaved_4(7);
  dataResults(4, 6) <= NlwRenamedSig_OI_dataArraySaved_4(6);
  dataResults(4, 5) <= NlwRenamedSig_OI_dataArraySaved_4(5);
  dataResults(4, 4) <= NlwRenamedSig_OI_dataArraySaved_4(4);
  dataResults(4, 3) <= NlwRenamedSig_OI_dataArraySaved_4(3);
  dataResults(4, 2) <= NlwRenamedSig_OI_dataArraySaved_4(2);
  dataResults(4, 1) <= NlwRenamedSig_OI_dataArraySaved_4(1);
  dataResults(4, 0) <= NlwRenamedSig_OI_dataArraySaved_4(0);
  dataResults(5, 7) <= NlwRenamedSig_OI_dataArraySaved_5(7);
  dataResults(5, 6) <= NlwRenamedSig_OI_dataArraySaved_5(6);
  dataResults(5, 5) <= NlwRenamedSig_OI_dataArraySaved_5(5);
  dataResults(5, 4) <= NlwRenamedSig_OI_dataArraySaved_5(4);
  dataResults(5, 3) <= NlwRenamedSig_OI_dataArraySaved_5(3);
  dataResults(5, 2) <= NlwRenamedSig_OI_dataArraySaved_5(2);
  dataResults(5, 1) <= NlwRenamedSig_OI_dataArraySaved_5(1);
  dataResults(5, 0) <= NlwRenamedSig_OI_dataArraySaved_5(0);
  dataResults(6, 7) <= NlwRenamedSig_OI_dataArraySaved_6(7);
  dataResults(6, 6) <= NlwRenamedSig_OI_dataArraySaved_6(6);
  dataResults(6, 5) <= NlwRenamedSig_OI_dataArraySaved_6(5);
  dataResults(6, 4) <= NlwRenamedSig_OI_dataArraySaved_6(4);
  dataResults(6, 3) <= NlwRenamedSig_OI_dataArraySaved_6(3);
  dataResults(6, 2) <= NlwRenamedSig_OI_dataArraySaved_6(2);
  dataResults(6, 1) <= NlwRenamedSig_OI_dataArraySaved_6(1);
  dataResults(6, 0) <= NlwRenamedSig_OI_dataArraySaved_6(0);
  ctrlOut <= NlwRenamedSig_OI_ctrlOut_reg;
  dataReady <= NlwRenamedSig_OI_dataReady;
  seqDone <= NlwRenamedSig_OI_seqDone;
  dataArraySaved_1_0 : FDRE
    port map (
      C => clk,
      CE => Q_n0310_inv,
      D => NlwRenamedSignal_byte(0),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_1(0)
    );
  dataArraySaved_1_1 : FDRE
    port map (
      C => clk,
      CE => Q_n0310_inv,
      D => NlwRenamedSignal_byte(1),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_1(1)
    );
  dataArraySaved_1_2 : FDRE
    port map (
      C => clk,
      CE => Q_n0310_inv,
      D => NlwRenamedSignal_byte(2),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_1(2)
    );
  dataArraySaved_1_3 : FDRE
    port map (
      C => clk,
      CE => Q_n0310_inv,
      D => NlwRenamedSignal_byte(3),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_1(3)
    );
  dataArraySaved_1_4 : FDRE
    port map (
      C => clk,
      CE => Q_n0310_inv,
      D => NlwRenamedSignal_byte(4),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_1(4)
    );
  dataArraySaved_1_5 : FDRE
    port map (
      C => clk,
      CE => Q_n0310_inv,
      D => NlwRenamedSignal_byte(5),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_1(5)
    );
  dataArraySaved_1_6 : FDRE
    port map (
      C => clk,
      CE => Q_n0310_inv,
      D => NlwRenamedSignal_byte(6),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_1(6)
    );
  dataArraySaved_1_7 : FDRE
    port map (
      C => clk,
      CE => Q_n0310_inv,
      D => NlwRenamedSignal_byte(7),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_1(7)
    );
  dataArraySaved_0_0 : FDRE
    port map (
      C => clk,
      CE => Q_n0304_inv,
      D => NlwRenamedSignal_byte(0),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_0(0)
    );
  dataArraySaved_0_1 : FDRE
    port map (
      C => clk,
      CE => Q_n0304_inv,
      D => NlwRenamedSignal_byte(1),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_0(1)
    );
  dataArraySaved_0_2 : FDRE
    port map (
      C => clk,
      CE => Q_n0304_inv,
      D => NlwRenamedSignal_byte(2),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_0(2)
    );
  dataArraySaved_0_3 : FDRE
    port map (
      C => clk,
      CE => Q_n0304_inv,
      D => NlwRenamedSignal_byte(3),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_0(3)
    );
  dataArraySaved_0_4 : FDRE
    port map (
      C => clk,
      CE => Q_n0304_inv,
      D => NlwRenamedSignal_byte(4),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_0(4)
    );
  dataArraySaved_0_5 : FDRE
    port map (
      C => clk,
      CE => Q_n0304_inv,
      D => NlwRenamedSignal_byte(5),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_0(5)
    );
  dataArraySaved_0_6 : FDRE
    port map (
      C => clk,
      CE => Q_n0304_inv,
      D => NlwRenamedSignal_byte(6),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_0(6)
    );
  dataArraySaved_0_7 : FDRE
    port map (
      C => clk,
      CE => Q_n0304_inv,
      D => NlwRenamedSignal_byte(7),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_0(7)
    );
  dataArraySaved_2_0 : FDRE
    port map (
      C => clk,
      CE => Q_n0316_inv,
      D => NlwRenamedSignal_byte(0),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_2(0)
    );
  dataArraySaved_2_1 : FDRE
    port map (
      C => clk,
      CE => Q_n0316_inv,
      D => NlwRenamedSignal_byte(1),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_2(1)
    );
  dataArraySaved_2_2 : FDRE
    port map (
      C => clk,
      CE => Q_n0316_inv,
      D => NlwRenamedSignal_byte(2),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_2(2)
    );
  dataArraySaved_2_3 : FDRE
    port map (
      C => clk,
      CE => Q_n0316_inv,
      D => NlwRenamedSignal_byte(3),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_2(3)
    );
  dataArraySaved_2_4 : FDRE
    port map (
      C => clk,
      CE => Q_n0316_inv,
      D => NlwRenamedSignal_byte(4),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_2(4)
    );
  dataArraySaved_2_5 : FDRE
    port map (
      C => clk,
      CE => Q_n0316_inv,
      D => NlwRenamedSignal_byte(5),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_2(5)
    );
  dataArraySaved_2_6 : FDRE
    port map (
      C => clk,
      CE => Q_n0316_inv,
      D => NlwRenamedSignal_byte(6),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_2(6)
    );
  dataArraySaved_2_7 : FDRE
    port map (
      C => clk,
      CE => Q_n0316_inv,
      D => NlwRenamedSignal_byte(7),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArraySaved_2(7)
    );
  dataArrayCurrent_0_0 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => NlwRenamedSignal_byte(0),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_0(0)
    );
  dataArrayCurrent_0_1 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => NlwRenamedSignal_byte(1),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_0(1)
    );
  dataArrayCurrent_0_2 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => NlwRenamedSignal_byte(2),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_0(2)
    );
  dataArrayCurrent_0_3 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => NlwRenamedSignal_byte(3),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_0(3)
    );
  dataArrayCurrent_0_4 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => NlwRenamedSignal_byte(4),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_0(4)
    );
  dataArrayCurrent_0_5 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => NlwRenamedSignal_byte(5),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_0(5)
    );
  dataArrayCurrent_0_6 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => NlwRenamedSignal_byte(6),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_0(6)
    );
  dataArrayCurrent_0_7 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => NlwRenamedSignal_byte(7),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_0(7)
    );
  ctrlIn_delayed : FD
    port map (
      C => clk,
      D => ctrlIn,
      Q => ctrlIn_delayed_56
    );
  dataArraySaved_3_0 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_3_0_dpot_196,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_3(0)
    );
  dataArraySaved_3_1 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_3_1_dpot_197,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_3(1)
    );
  dataArraySaved_3_2 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_3_2_dpot_198,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_3(2)
    );
  dataArraySaved_3_3 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_3_3_dpot_199,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_3(3)
    );
  dataArraySaved_3_4 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_3_4_dpot_200,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_3(4)
    );
  dataArraySaved_3_5 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_3_5_dpot_201,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_3(5)
    );
  dataArraySaved_3_6 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_3_6_dpot_202,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_3(6)
    );
  dataArraySaved_3_7 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_3_7_dpot_203,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_3(7)
    );
  dataArrayCurrent_1_0 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_0(0),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_1(0)
    );
  dataArrayCurrent_1_1 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_0(1),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_1(1)
    );
  dataArrayCurrent_1_2 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_0(2),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_1(2)
    );
  dataArrayCurrent_1_3 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_0(3),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_1(3)
    );
  dataArrayCurrent_1_4 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_0(4),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_1(4)
    );
  dataArrayCurrent_1_5 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_0(5),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_1(5)
    );
  dataArrayCurrent_1_6 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_0(6),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_1(6)
    );
  dataArrayCurrent_1_7 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_0(7),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_1(7)
    );
  dataArrayCurrent_2_0 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_1(0),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_2(0)
    );
  dataArrayCurrent_2_1 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_1(1),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_2(1)
    );
  dataArrayCurrent_2_2 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_1(2),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_2(2)
    );
  dataArrayCurrent_2_3 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_1(3),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_2(3)
    );
  dataArrayCurrent_2_4 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_1(4),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_2(4)
    );
  dataArrayCurrent_2_5 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_1(5),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_2(5)
    );
  dataArrayCurrent_2_6 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_1(6),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_2(6)
    );
  dataArrayCurrent_2_7 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_1(7),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_2(7)
    );
  bcd_wordCounting_var_bcdCount_0_0 : FDRE
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      CE => Q_n0409_inv1_cepot,
      D => bcd_wordCounting_var_bcdCount_0_0_dpot_179,
      R => reset,
      Q => bcd_wordCounting_var_bcdCount_0(0)
    );
  bcd_wordCounting_var_bcdCount_0_1 : FDRE
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      CE => Q_n0409_inv1_cepot,
      D => bcd_wordCounting_var_bcdCount_0_1_dpot_180,
      R => reset,
      Q => bcd_wordCounting_var_bcdCount_0(1)
    );
  bcd_wordCounting_var_bcdCount_0_2 : FDRE
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      CE => Q_n0409_inv1_cepot,
      D => bcd_wordCounting_var_bcdCount_0_2_dpot_181,
      R => reset,
      Q => bcd_wordCounting_var_bcdCount_0(2)
    );
  bcd_wordCounting_var_bcdCount_0_3 : FDRE
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      CE => Q_n0409_inv1_cepot,
      D => bcd_wordCounting_var_bcdCount_0_3_dpot_182,
      R => reset,
      Q => bcd_wordCounting_var_bcdCount_0(3)
    );
  dataArraySaved_4_0 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_4_0_dpot_232,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_4(0)
    );
  dataArraySaved_4_1 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_4_1_dpot_233,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_4(1)
    );
  dataArraySaved_4_2 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_4_2_dpot_234,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_4(2)
    );
  dataArraySaved_4_3 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_4_3_dpot_235,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_4(3)
    );
  dataArraySaved_4_4 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_4_4_dpot_236,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_4(4)
    );
  dataArraySaved_4_5 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_4_5_dpot_237,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_4(5)
    );
  dataArraySaved_4_6 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_4_6_dpot_238,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_4(6)
    );
  dataArraySaved_4_7 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_4_7_dpot_239,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_4(7)
    );
  dataArrayCurrent_3_0 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_2(0),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_3(0)
    );
  dataArrayCurrent_3_1 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_2(1),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_3(1)
    );
  dataArrayCurrent_3_2 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_2(2),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_3(2)
    );
  dataArrayCurrent_3_3 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_2(3),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_3(3)
    );
  dataArrayCurrent_3_4 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_2(4),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_3(4)
    );
  dataArrayCurrent_3_5 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_2(5),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_3(5)
    );
  dataArrayCurrent_3_6 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_2(6),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_3(6)
    );
  dataArrayCurrent_3_7 : FDRE
    port map (
      C => clk,
      CE => NlwRenamedSig_OI_dataReady,
      D => dataArrayCurrent_2(7),
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => dataArrayCurrent_3(7)
    );
  maxIndex_reg_2_0 : FDSE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => maxIndex_reg_2_0_dpot_204,
      S => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_maxIndex_reg_2(0)
    );
  maxIndex_reg_2_1 : FDSE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => maxIndex_reg_2_1_dpot_205,
      S => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_maxIndex_reg_2(1)
    );
  maxIndex_reg_2_2 : FDSE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => maxIndex_reg_2_2_dpot_206,
      S => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_maxIndex_reg_2(2)
    );
  maxIndex_reg_2_3 : FDSE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => maxIndex_reg_2_3_dpot_207,
      S => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_maxIndex_reg_2(3)
    );
  maxIndex_reg_1_0 : FDSE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => maxIndex_reg_1_0_dpot_212,
      S => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_maxIndex_reg_1(0)
    );
  maxIndex_reg_1_1 : FDSE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => maxIndex_reg_1_1_dpot_213,
      S => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_maxIndex_reg_1(1)
    );
  maxIndex_reg_1_2 : FDSE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => maxIndex_reg_1_2_dpot_214,
      S => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_maxIndex_reg_1(2)
    );
  maxIndex_reg_1_3 : FDSE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => maxIndex_reg_1_3_dpot_215,
      S => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_maxIndex_reg_1(3)
    );
  dataArraySaved_5_0 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_5_0_dpot_224,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_5(0)
    );
  dataArraySaved_5_1 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_5_1_dpot_225,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_5(1)
    );
  dataArraySaved_5_2 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_5_2_dpot_226,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_5(2)
    );
  dataArraySaved_5_3 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_5_3_dpot_227,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_5(3)
    );
  dataArraySaved_5_4 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_5_4_dpot_228,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_5(4)
    );
  dataArraySaved_5_5 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_5_5_dpot_229,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_5(5)
    );
  dataArraySaved_5_6 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_5_6_dpot_230,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_5(6)
    );
  dataArraySaved_5_7 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_5_7_dpot_231,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_5(7)
    );
  maxIndex_reg_0_0 : FDSE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => maxIndex_reg_0_0_dpot_208,
      S => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_maxIndex_reg_0(0)
    );
  maxIndex_reg_0_1 : FDSE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => maxIndex_reg_0_1_dpot_209,
      S => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_maxIndex_reg_0(1)
    );
  maxIndex_reg_0_2 : FDSE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => maxIndex_reg_0_2_dpot_210,
      S => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_maxIndex_reg_0(2)
    );
  maxIndex_reg_0_3 : FDSE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => maxIndex_reg_0_3_dpot_211,
      S => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_maxIndex_reg_0(3)
    );
  dataArraySaved_6_0 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_6_0_dpot_216,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_6(0)
    );
  dataArraySaved_6_1 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_6_1_dpot_217,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_6(1)
    );
  dataArraySaved_6_2 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_6_2_dpot_218,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_6(2)
    );
  dataArraySaved_6_3 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_6_3_dpot_219,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_6(3)
    );
  dataArraySaved_6_4 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_6_4_dpot_220,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_6(4)
    );
  dataArraySaved_6_5 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_6_5_dpot_221,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_6(5)
    );
  dataArraySaved_6_6 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_6_6_dpot_222,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_6(6)
    );
  dataArraySaved_6_7 : FDRE
    port map (
      C => clk,
      CE => curState_FSM_FFd1_59,
      D => dataArraySaved_6_7_dpot_223,
      R => reset_sig_seqDone_delayed_OR_12_o,
      Q => NlwRenamedSig_OI_dataArraySaved_6(7)
    );
  curState_FSM_FFd3 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd3_In_138,
      R => reset,
      Q => curState_FSM_FFd3_58
    );
  curState_FSM_FFd2 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd2_In,
      R => reset,
      Q => curState_FSM_FFd2_57
    );
  curState_FSM_FFd1 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd1_In,
      R => reset,
      Q => curState_FSM_FFd1_59
    );
  reset_sig_seqDone_delayed_OR_12_o1 : LUT2
    generic map(
      INIT => X"E"
    )
    port map (
      I0 => reset,
      I1 => sig_seqDone_delayed_61,
      O => reset_sig_seqDone_delayed_OR_12_o
    );
  curState_dataReady1 : LUT2
    generic map(
      INIT => X"8"
    )
    port map (
      I0 => curState_FSM_FFd3_1_242,
      I1 => curState_FSM_FFd2_1_241,
      O => NlwRenamedSig_OI_dataReady
    );
  curState_FSM_FFd2_In1 : LUT2
    generic map(
      INIT => X"6"
    )
    port map (
      I0 => curState_FSM_FFd3_58,
      I1 => curState_FSM_FFd2_57,
      O => curState_FSM_FFd2_In
    );
  curState_FSM_FFd1_In1 : LUT3
    generic map(
      INIT => X"80"
    )
    port map (
      I0 => curState_FSM_FFd3_58,
      I1 => curState_FSM_FFd2_57,
      I2 => curState_FSM_FFd3_In1,
      O => curState_FSM_FFd1_In
    );
  sig_grtThan2 : LUT6
    generic map(
      INIT => X"0080C0E0F0F8FCFE"
    )
    port map (
      I0 => NlwRenamedSignal_byte(0),
      I1 => NlwRenamedSignal_byte(1),
      I2 => NlwRenamedSignal_byte(2),
      I3 => NlwRenamedSig_OI_dataArraySaved_3(0),
      I4 => NlwRenamedSig_OI_dataArraySaved_3(1),
      I5 => NlwRenamedSig_OI_dataArraySaved_3(2),
      O => sig_grtThan1
    );
  curState_FSM_FFd3_In_SW0 : LUT2
    generic map(
      INIT => X"6"
    )
    port map (
      I0 => ctrlIn_delayed_56,
      I1 => ctrlIn,
      O => N01
    );
  curState_FSM_FFd3_In : LUT6
    generic map(
      INIT => X"88F888AAAAF888AA"
    )
    port map (
      I0 => start,
      I1 => curState_FSM_FFd1_59,
      I2 => N01,
      I3 => curState_FSM_FFd3_58,
      I4 => curState_FSM_FFd2_57,
      I5 => curState_FSM_FFd3_In1,
      O => curState_FSM_FFd3_In_138
    );
  seqDone1 : LUT6
    generic map(
      INIT => X"8008400420021001"
    )
    port map (
      I0 => numWords_bcd(1, 2),
      I1 => numWords_bcd(0, 1),
      I2 => numWords_bcd(0, 2),
      I3 => bcd_wordCounting_var_bcdCount_0(2),
      I4 => bcd_wordCounting_var_bcdCount_1(2),
      I5 => bcd_wordCounting_var_bcdCount_0(1),
      O => seqDone1_160
    );
  seqDone2 : LUT6
    generic map(
      INIT => X"8040201008040201"
    )
    port map (
      I0 => numWords_bcd(1, 0),
      I1 => numWords_bcd(1, 1),
      I2 => numWords_bcd(0, 0),
      I3 => bcd_wordCounting_var_bcdCount_1(0),
      I4 => bcd_wordCounting_var_bcdCount_1(1),
      I5 => bcd_wordCounting_var_bcdCount_0(0),
      O => seqDone2_161
    );
  seqDone3 : LUT6
    generic map(
      INIT => X"8020401008020401"
    )
    port map (
      I0 => numWords_bcd(2, 2),
      I1 => numWords_bcd(2, 3),
      I2 => numWords_bcd(1, 3),
      I3 => bcd_wordCounting_var_bcdCount_2(3),
      I4 => bcd_wordCounting_var_bcdCount_2(2),
      I5 => bcd_wordCounting_var_bcdCount_1(3),
      O => seqDone3_162
    );
  seqDone4 : LUT6
    generic map(
      INIT => X"8020401008020401"
    )
    port map (
      I0 => numWords_bcd(2, 0),
      I1 => numWords_bcd(2, 1),
      I2 => numWords_bcd(0, 3),
      I3 => bcd_wordCounting_var_bcdCount_2(1),
      I4 => bcd_wordCounting_var_bcdCount_2(0),
      I5 => bcd_wordCounting_var_bcdCount_0(3),
      O => seqDone4_163
    );
  seqDone5 : LUT4
    generic map(
      INIT => X"8000"
    )
    port map (
      I0 => seqDone4_163,
      I1 => seqDone3_162,
      I2 => seqDone1_160,
      I3 => seqDone2_161,
      O => NlwRenamedSig_OI_seqDone
    );
  sig_grtThan1_SW0 : LUT4
    generic map(
      INIT => X"0A8E"
    )
    port map (
      I0 => NlwRenamedSignal_byte(4),
      I1 => NlwRenamedSignal_byte(3),
      I2 => NlwRenamedSig_OI_dataArraySaved_3(4),
      I3 => NlwRenamedSig_OI_dataArraySaved_3(3),
      O => N2
    );
  sig_grtThan1_SW1 : LUT4
    generic map(
      INIT => X"8CEF"
    )
    port map (
      I0 => NlwRenamedSignal_byte(3),
      I1 => NlwRenamedSignal_byte(4),
      I2 => NlwRenamedSig_OI_dataArraySaved_3(3),
      I3 => NlwRenamedSig_OI_dataArraySaved_3(4),
      O => N3
    );
  Q_n0374_inv1_SW0 : LUT2
    generic map(
      INIT => X"E"
    )
    port map (
      I0 => curState_FSM_FFd1_1_240,
      I1 => bcd_wordCounting_var_bcdCount_0(2),
      O => N5
    );
  Q_n0374_inv1 : LUT6
    generic map(
      INIT => X"0000080000000000"
    )
    port map (
      I0 => curState_FSM_FFd3_1_242,
      I1 => bcd_wordCounting_var_bcdCount_0(3),
      I2 => bcd_wordCounting_var_bcdCount_0(1),
      I3 => bcd_wordCounting_var_bcdCount_0(0),
      I4 => N5,
      I5 => curState_FSM_FFd2_57,
      O => Q_n0374_inv1_158
    );
  ctrlOut_reg : FDR
    port map (
      C => clk,
      D => ctrlOut_reg_rstpot_167,
      R => reset,
      Q => NlwRenamedSig_OI_ctrlOut_reg
    );
  sig_seqDone_delayed : FD
    port map (
      C => clk,
      D => sig_seqDone_delayed_rstpot_168,
      Q => sig_seqDone_delayed_61
    );
  startUpdate : FD
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => startUpdate_rstpot_169,
      Q => startUpdate_131
    );
  Q_n0374_inv_SW0_SW0 : LUT4
    generic map(
      INIT => X"FFF7"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_1(0),
      I1 => bcd_wordCounting_var_bcdCount_1(3),
      I2 => bcd_wordCounting_var_bcdCount_1(2),
      I3 => bcd_wordCounting_var_bcdCount_1(1),
      O => N9
    );
  bcd_wordCounting_var_bcdCount_2_0 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => bcd_wordCounting_var_bcdCount_2_0_rstpot_171,
      R => reset,
      Q => bcd_wordCounting_var_bcdCount_2(0)
    );
  bcd_wordCounting_var_bcdCount_2_2 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => bcd_wordCounting_var_bcdCount_2_2_rstpot_172,
      R => reset,
      Q => bcd_wordCounting_var_bcdCount_2(2)
    );
  bcd_wordCounting_var_bcdCount_2_1 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => bcd_wordCounting_var_bcdCount_2_1_rstpot_173,
      R => reset,
      Q => bcd_wordCounting_var_bcdCount_2(1)
    );
  bcd_wordCounting_var_bcdCount_2_3 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => bcd_wordCounting_var_bcdCount_2_3_rstpot_174,
      R => reset,
      Q => bcd_wordCounting_var_bcdCount_2(3)
    );
  bcd_wordCounting_var_bcdCount_2_0_rstpot : LUT3
    generic map(
      INIT => X"4A"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_2(0),
      I1 => curState_FSM_FFd2_57,
      I2 => Q_n0374_inv_137,
      O => bcd_wordCounting_var_bcdCount_2_0_rstpot_171
    );
  bcd_wordCounting_var_bcdCount_2_2_rstpot : LUT5
    generic map(
      INIT => X"4888AAAA"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_2(2),
      I1 => curState_FSM_FFd2_57,
      I2 => bcd_wordCounting_var_bcdCount_2(1),
      I3 => bcd_wordCounting_var_bcdCount_2(0),
      I4 => Q_n0374_inv_137,
      O => bcd_wordCounting_var_bcdCount_2_2_rstpot_172
    );
  bcd_wordCounting_var_bcdCount_2_1_rstpot : LUT6
    generic map(
      INIT => X"0DF00000F0F0F0F0"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_2(3),
      I1 => bcd_wordCounting_var_bcdCount_2(2),
      I2 => bcd_wordCounting_var_bcdCount_2(1),
      I3 => bcd_wordCounting_var_bcdCount_2(0),
      I4 => curState_FSM_FFd2_57,
      I5 => Q_n0374_inv_137,
      O => bcd_wordCounting_var_bcdCount_2_1_rstpot_173
    );
  bcd_wordCounting_var_bcdCount_2_3_rstpot : LUT6
    generic map(
      INIT => X"48808888AAAAAAAA"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_2(3),
      I1 => curState_FSM_FFd2_57,
      I2 => bcd_wordCounting_var_bcdCount_2(1),
      I3 => bcd_wordCounting_var_bcdCount_2(2),
      I4 => bcd_wordCounting_var_bcdCount_2(0),
      I5 => Q_n0374_inv_137,
      O => bcd_wordCounting_var_bcdCount_2_3_rstpot_174
    );
  Mcount_updateCount_val1 : LUT6
    generic map(
      INIT => X"FEEEEEEEEEEEEEEE"
    )
    port map (
      I0 => reset,
      I1 => curState_FSM_FFd1_2_245,
      I2 => seqDone4_163,
      I3 => seqDone3_162,
      I4 => seqDone1_160,
      I5 => seqDone2_161,
      O => Mcount_updateCount_val
    );
  sig_grtThan1_SW2 : LUT6
    generic map(
      INIT => X"0080C0E0F0F8FCFE"
    )
    port map (
      I0 => NlwRenamedSignal_byte(5),
      I1 => NlwRenamedSignal_byte(6),
      I2 => NlwRenamedSignal_byte(7),
      I3 => NlwRenamedSig_OI_dataArraySaved_3(5),
      I4 => NlwRenamedSig_OI_dataArraySaved_3(6),
      I5 => NlwRenamedSig_OI_dataArraySaved_3(7),
      O => N11
    );
  sig_grtThan1_SW3 : LUT6
    generic map(
      INIT => X"80A0A8AAEAFAFEFF"
    )
    port map (
      I0 => NlwRenamedSignal_byte(7),
      I1 => NlwRenamedSignal_byte(5),
      I2 => NlwRenamedSignal_byte(6),
      I3 => NlwRenamedSig_OI_dataArraySaved_3(5),
      I4 => NlwRenamedSig_OI_dataArraySaved_3(6),
      I5 => NlwRenamedSig_OI_dataArraySaved_3(7),
      O => N12
    );
  curState_FSM_FFd3_In11 : LUT5
    generic map(
      INIT => X"FF35CA00"
    )
    port map (
      I0 => N2,
      I1 => N3,
      I2 => sig_grtThan1,
      I3 => N12,
      I4 => N11,
      O => curState_FSM_FFd3_In1
    );
  bcd_wordCounting_var_bcdCount_1_0 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => bcd_wordCounting_var_bcdCount_1_0_rstpot_183,
      R => reset,
      Q => bcd_wordCounting_var_bcdCount_1(0)
    );
  bcd_wordCounting_var_bcdCount_1_2 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => bcd_wordCounting_var_bcdCount_1_2_rstpot_184,
      R => reset,
      Q => bcd_wordCounting_var_bcdCount_1(2)
    );
  bcd_wordCounting_var_bcdCount_1_1 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => bcd_wordCounting_var_bcdCount_1_1_rstpot_185,
      R => reset,
      Q => bcd_wordCounting_var_bcdCount_1(1)
    );
  bcd_wordCounting_var_bcdCount_1_3 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => bcd_wordCounting_var_bcdCount_1_3_rstpot_186,
      R => reset,
      Q => bcd_wordCounting_var_bcdCount_1(3)
    );
  bcd_wordCounting_var_bcdCount_0_0_dpot : LUT3
    generic map(
      INIT => X"4A"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_0(0),
      I1 => curState_FSM_FFd2_57,
      I2 => Q_n0409_inv1_rstpot_177,
      O => bcd_wordCounting_var_bcdCount_0_0_dpot_179
    );
  bcd_wordCounting_var_bcdCount_0_1_dpot : LUT6
    generic map(
      INIT => X"0DF00000F0F0F0F0"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_0(3),
      I1 => bcd_wordCounting_var_bcdCount_0(2),
      I2 => bcd_wordCounting_var_bcdCount_0(1),
      I3 => bcd_wordCounting_var_bcdCount_0(0),
      I4 => curState_FSM_FFd2_57,
      I5 => Q_n0409_inv1_rstpot_177,
      O => bcd_wordCounting_var_bcdCount_0_1_dpot_180
    );
  bcd_wordCounting_var_bcdCount_0_2_dpot : LUT5
    generic map(
      INIT => X"4888AAAA"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_0(2),
      I1 => curState_FSM_FFd2_57,
      I2 => bcd_wordCounting_var_bcdCount_0(1),
      I3 => bcd_wordCounting_var_bcdCount_0(0),
      I4 => Q_n0409_inv1_rstpot_177,
      O => bcd_wordCounting_var_bcdCount_0_2_dpot_181
    );
  bcd_wordCounting_var_bcdCount_0_3_dpot : LUT6
    generic map(
      INIT => X"48808888AAAAAAAA"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_0(3),
      I1 => curState_FSM_FFd2_57,
      I2 => bcd_wordCounting_var_bcdCount_0(1),
      I3 => bcd_wordCounting_var_bcdCount_0(2),
      I4 => bcd_wordCounting_var_bcdCount_0(0),
      I5 => Q_n0409_inv1_rstpot_177,
      O => bcd_wordCounting_var_bcdCount_0_3_dpot_182
    );
  Q_n0409_inv1_rstpot : LUT6
    generic map(
      INIT => X"9888888888888888"
    )
    port map (
      I0 => curState_FSM_FFd3_1_242,
      I1 => curState_FSM_FFd2_1_241,
      I2 => seqDone4_163,
      I3 => seqDone3_162,
      I4 => seqDone1_160,
      I5 => seqDone2_161,
      O => Q_n0409_inv1_rstpot_177
    );
  bcd_wordCounting_var_bcdCount_1_0_rstpot : LUT6
    generic map(
      INIT => X"5500AAA85500AAAA"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_1(0),
      I1 => curState_FSM_FFd1_59,
      I2 => curState_FSM_FFd3_58,
      I3 => curState_FSM_FFd2_57,
      I4 => Q_n0374_inv1_158,
      I5 => NlwRenamedSig_OI_seqDone,
      O => bcd_wordCounting_var_bcdCount_1_0_rstpot_183
    );
  Q_n0393_inv1_SW0 : LUT4
    generic map(
      INIT => X"95FF"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_1(2),
      I1 => bcd_wordCounting_var_bcdCount_1(1),
      I2 => bcd_wordCounting_var_bcdCount_1(0),
      I3 => curState_FSM_FFd2_57,
      O => N14
    );
  Q_n0393_inv1_SW1 : LUT3
    generic map(
      INIT => X"A8"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_1(2),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      O => N15
    );
  bcd_wordCounting_var_bcdCount_1_2_rstpot : LUT6
    generic map(
      INIT => X"0F0FBB880F0FAAAA"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_1(2),
      I1 => curState_FSM_FFd1_59,
      I2 => N14,
      I3 => N15,
      I4 => Q_n0374_inv1_158,
      I5 => NlwRenamedSig_OI_seqDone,
      O => bcd_wordCounting_var_bcdCount_1_2_rstpot_184
    );
  Q_n0393_inv1_SW2 : LUT5
    generic map(
      INIT => X"99D9FFFF"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_1(1),
      I1 => bcd_wordCounting_var_bcdCount_1(0),
      I2 => bcd_wordCounting_var_bcdCount_1(3),
      I3 => bcd_wordCounting_var_bcdCount_1(2),
      I4 => curState_FSM_FFd2_57,
      O => N17
    );
  Q_n0393_inv1_SW3 : LUT3
    generic map(
      INIT => X"A8"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_1(1),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      O => N18
    );
  bcd_wordCounting_var_bcdCount_1_1_rstpot : LUT6
    generic map(
      INIT => X"0F0FBB880F0FAAAA"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_1(1),
      I1 => curState_FSM_FFd1_59,
      I2 => N17,
      I3 => N18,
      I4 => Q_n0374_inv1_158,
      I5 => NlwRenamedSig_OI_seqDone,
      O => bcd_wordCounting_var_bcdCount_1_1_rstpot_185
    );
  Q_n0393_inv1_SW4 : LUT5
    generic map(
      INIT => X"9755FFFF"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_1(3),
      I1 => bcd_wordCounting_var_bcdCount_1(2),
      I2 => bcd_wordCounting_var_bcdCount_1(1),
      I3 => bcd_wordCounting_var_bcdCount_1(0),
      I4 => curState_FSM_FFd2_57,
      O => N20
    );
  Q_n0393_inv1_SW5 : LUT3
    generic map(
      INIT => X"A8"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_1(3),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      O => N21
    );
  bcd_wordCounting_var_bcdCount_1_3_rstpot : LUT6
    generic map(
      INIT => X"0F0FBB880F0FAAAA"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_1(3),
      I1 => curState_FSM_FFd1_59,
      I2 => N20,
      I3 => N21,
      I4 => Q_n0374_inv1_158,
      I5 => NlwRenamedSig_OI_seqDone,
      O => bcd_wordCounting_var_bcdCount_1_3_rstpot_186
    );
  sig_seqDone_delayed_rstpot : LUT6
    generic map(
      INIT => X"D888888888888888"
    )
    port map (
      I0 => reset,
      I1 => sig_seqDone_delayed_61,
      I2 => seqDone1_160,
      I3 => seqDone2_161,
      I4 => seqDone3_162,
      I5 => seqDone4_163,
      O => sig_seqDone_delayed_rstpot_168
    );
  Q_n0310_inv1 : LUT5
    generic map(
      INIT => X"10000000"
    )
    port map (
      I0 => updateCount(1),
      I1 => startUpdate_131,
      I2 => updateCount(0),
      I3 => curState_FSM_FFd3_58,
      I4 => curState_FSM_FFd2_57,
      O => Q_n0310_inv
    );
  Q_n0304_inv1 : LUT5
    generic map(
      INIT => X"02000000"
    )
    port map (
      I0 => updateCount(1),
      I1 => startUpdate_131,
      I2 => updateCount(0),
      I3 => curState_FSM_FFd3_58,
      I4 => curState_FSM_FFd2_57,
      O => Q_n0304_inv
    );
  Q_n0316_inv1 : LUT5
    generic map(
      INIT => X"01000000"
    )
    port map (
      I0 => startUpdate_131,
      I1 => updateCount(1),
      I2 => updateCount(0),
      I3 => curState_FSM_FFd3_58,
      I4 => curState_FSM_FFd2_57,
      O => Q_n0316_inv
    );
  Q_n0374_inv : MUXF7
    port map (
      I0 => N23,
      I1 => N24,
      S => curState_FSM_FFd3_58,
      O => Q_n0374_inv_137
    );
  Q_n0374_inv_F : LUT6
    generic map(
      INIT => X"1000000000000000"
    )
    port map (
      I0 => curState_FSM_FFd1_2_245,
      I1 => curState_FSM_FFd2_1_241,
      I2 => seqDone3_162,
      I3 => seqDone4_163,
      I4 => seqDone1_160,
      I5 => seqDone2_161,
      O => N23
    );
  Q_n0374_inv_G : LUT6
    generic map(
      INIT => X"0000000000000800"
    )
    port map (
      I0 => curState_FSM_FFd2_1_241,
      I1 => bcd_wordCounting_var_bcdCount_0(3),
      I2 => bcd_wordCounting_var_bcdCount_0(1),
      I3 => bcd_wordCounting_var_bcdCount_0(0),
      I4 => N5,
      I5 => N9,
      O => N24
    );
  ctrlOut_reg_rstpot : LUT3
    generic map(
      INIT => X"9A"
    )
    port map (
      I0 => NlwRenamedSig_OI_ctrlOut_reg,
      I1 => curState_FSM_FFd2_57,
      I2 => curState_FSM_FFd3_58,
      O => ctrlOut_reg_rstpot_167
    );
  startUpdate_rstpot : LUT5
    generic map(
      INIT => X"FFFF5444"
    )
    port map (
      I0 => curState_FSM_FFd1_59,
      I1 => startUpdate_131,
      I2 => updateCount(0),
      I3 => updateCount(1),
      I4 => reset,
      O => startUpdate_rstpot_169
    );
  Q_n0324_inv1_rstpot : LUT2
    generic map(
      INIT => X"7"
    )
    port map (
      I0 => curState_FSM_FFd3_58,
      I1 => curState_FSM_FFd2_57,
      O => Q_n0324_inv1_rstpot_195
    );
  maxIndex_reg_0_3_dpot : LUT6
    generic map(
      INIT => X"CCCCCCC3AAAAAAAA"
    )
    port map (
      I0 => NlwRenamedSig_OI_maxIndex_reg_0(3),
      I1 => bcd_wordCounting_var_bcdCount_0(3),
      I2 => bcd_wordCounting_var_bcdCount_0(2),
      I3 => bcd_wordCounting_var_bcdCount_0(1),
      I4 => bcd_wordCounting_var_bcdCount_0(0),
      I5 => Q_n0324_inv1_rstpot_195,
      O => maxIndex_reg_0_3_dpot_211
    );
  maxIndex_reg_2_0_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_2(0),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_maxIndex_reg_2(0),
      O => maxIndex_reg_2_0_dpot_204
    );
  maxIndex_reg_2_1_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_2(1),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_maxIndex_reg_2(1),
      O => maxIndex_reg_2_1_dpot_205
    );
  maxIndex_reg_2_2_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_2(2),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_maxIndex_reg_2(2),
      O => maxIndex_reg_2_2_dpot_206
    );
  maxIndex_reg_2_3_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_2(3),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_maxIndex_reg_2(3),
      O => maxIndex_reg_2_3_dpot_207
    );
  maxIndex_reg_1_0_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_1(0),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_maxIndex_reg_1(0),
      O => maxIndex_reg_1_0_dpot_212
    );
  maxIndex_reg_1_1_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_1(1),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_maxIndex_reg_1(1),
      O => maxIndex_reg_1_1_dpot_213
    );
  maxIndex_reg_1_2_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_1(2),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_maxIndex_reg_1(2),
      O => maxIndex_reg_1_2_dpot_214
    );
  maxIndex_reg_1_3_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_1(3),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_maxIndex_reg_1(3),
      O => maxIndex_reg_1_3_dpot_215
    );
  curState_FSM_FFd1_1 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd1_In,
      R => reset,
      Q => curState_FSM_FFd1_1_240
    );
  dataArraySaved_3_0_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_0(0),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_3(0),
      O => dataArraySaved_3_0_dpot_196
    );
  dataArraySaved_3_1_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_0(1),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_3(1),
      O => dataArraySaved_3_1_dpot_197
    );
  dataArraySaved_3_2_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_0(2),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_3(2),
      O => dataArraySaved_3_2_dpot_198
    );
  dataArraySaved_3_3_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_0(3),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_3(3),
      O => dataArraySaved_3_3_dpot_199
    );
  dataArraySaved_3_4_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_0(4),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_3(4),
      O => dataArraySaved_3_4_dpot_200
    );
  dataArraySaved_3_5_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_0(5),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_3(5),
      O => dataArraySaved_3_5_dpot_201
    );
  dataArraySaved_3_6_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_0(6),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_3(6),
      O => dataArraySaved_3_6_dpot_202
    );
  dataArraySaved_3_7_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_0(7),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_3(7),
      O => dataArraySaved_3_7_dpot_203
    );
  dataArraySaved_6_0_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_3(0),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_6(0),
      O => dataArraySaved_6_0_dpot_216
    );
  dataArraySaved_6_1_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_3(1),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_6(1),
      O => dataArraySaved_6_1_dpot_217
    );
  dataArraySaved_6_2_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_3(2),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_6(2),
      O => dataArraySaved_6_2_dpot_218
    );
  dataArraySaved_6_3_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_3(3),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_6(3),
      O => dataArraySaved_6_3_dpot_219
    );
  dataArraySaved_6_4_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_3(4),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_6(4),
      O => dataArraySaved_6_4_dpot_220
    );
  dataArraySaved_6_5_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_3(5),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_6(5),
      O => dataArraySaved_6_5_dpot_221
    );
  dataArraySaved_6_6_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_3(6),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_6(6),
      O => dataArraySaved_6_6_dpot_222
    );
  dataArraySaved_6_7_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_3(7),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_6(7),
      O => dataArraySaved_6_7_dpot_223
    );
  dataArraySaved_5_0_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_2(0),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_5(0),
      O => dataArraySaved_5_0_dpot_224
    );
  dataArraySaved_5_1_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_2(1),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_5(1),
      O => dataArraySaved_5_1_dpot_225
    );
  dataArraySaved_5_2_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_2(2),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_5(2),
      O => dataArraySaved_5_2_dpot_226
    );
  dataArraySaved_5_3_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_2(3),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_5(3),
      O => dataArraySaved_5_3_dpot_227
    );
  dataArraySaved_5_4_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_2(4),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_5(4),
      O => dataArraySaved_5_4_dpot_228
    );
  dataArraySaved_5_5_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_2(5),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_5(5),
      O => dataArraySaved_5_5_dpot_229
    );
  dataArraySaved_5_6_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_2(6),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_5(6),
      O => dataArraySaved_5_6_dpot_230
    );
  dataArraySaved_5_7_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_2(7),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_5(7),
      O => dataArraySaved_5_7_dpot_231
    );
  dataArraySaved_4_0_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_1(0),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_4(0),
      O => dataArraySaved_4_0_dpot_232
    );
  dataArraySaved_4_1_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_1(1),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_4(1),
      O => dataArraySaved_4_1_dpot_233
    );
  dataArraySaved_4_2_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_1(2),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_4(2),
      O => dataArraySaved_4_2_dpot_234
    );
  dataArraySaved_4_3_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_1(3),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_4(3),
      O => dataArraySaved_4_3_dpot_235
    );
  dataArraySaved_4_4_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_1(4),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_4(4),
      O => dataArraySaved_4_4_dpot_236
    );
  dataArraySaved_4_5_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_1(5),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_4(5),
      O => dataArraySaved_4_5_dpot_237
    );
  dataArraySaved_4_6_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_1(6),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_4(6),
      O => dataArraySaved_4_6_dpot_238
    );
  dataArraySaved_4_7_dpot : LUT4
    generic map(
      INIT => X"EA2A"
    )
    port map (
      I0 => dataArrayCurrent_1(7),
      I1 => curState_FSM_FFd3_58,
      I2 => curState_FSM_FFd2_57,
      I3 => NlwRenamedSig_OI_dataArraySaved_4(7),
      O => dataArraySaved_4_7_dpot_239
    );
  maxIndex_reg_0_2_dpot : LUT6
    generic map(
      INIT => X"F8F8F88F70707007"
    )
    port map (
      I0 => curState_FSM_FFd3_58,
      I1 => curState_FSM_FFd2_57,
      I2 => bcd_wordCounting_var_bcdCount_0(2),
      I3 => bcd_wordCounting_var_bcdCount_0(0),
      I4 => bcd_wordCounting_var_bcdCount_0(1),
      I5 => NlwRenamedSig_OI_maxIndex_reg_0(2),
      O => maxIndex_reg_0_2_dpot_210
    );
  maxIndex_reg_0_1_dpot : LUT5
    generic map(
      INIT => X"F88F7007"
    )
    port map (
      I0 => curState_FSM_FFd3_58,
      I1 => curState_FSM_FFd2_57,
      I2 => bcd_wordCounting_var_bcdCount_0(1),
      I3 => bcd_wordCounting_var_bcdCount_0(0),
      I4 => NlwRenamedSig_OI_maxIndex_reg_0(1),
      O => maxIndex_reg_0_1_dpot_209
    );
  maxIndex_reg_0_0_dpot : LUT4
    generic map(
      INIT => X"D515"
    )
    port map (
      I0 => bcd_wordCounting_var_bcdCount_0(0),
      I1 => curState_FSM_FFd2_57,
      I2 => curState_FSM_FFd3_58,
      I3 => NlwRenamedSig_OI_maxIndex_reg_0(0),
      O => maxIndex_reg_0_0_dpot_208
    );
  curState_FSM_FFd2_1 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd2_In,
      R => reset,
      Q => curState_FSM_FFd2_1_241
    );
  curState_FSM_FFd3_1 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd3_In_138,
      R => reset,
      Q => curState_FSM_FFd3_1_242
    );
  updateCount_1 : FD
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => updateCount_1_rstpot_243,
      Q => updateCount(1)
    );
  updateCount_0 : FD
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => updateCount_0_rstpot_244,
      Q => updateCount(0)
    );
  curState_FSM_FFd1_2 : FDR
    generic map(
      INIT => '0'
    )
    port map (
      C => clk,
      D => curState_FSM_FFd1_In,
      R => reset,
      Q => curState_FSM_FFd1_2_245
    );
  updateCount_1_rstpot : LUT6
    generic map(
      INIT => X"4414444444444444"
    )
    port map (
      I0 => Mcount_updateCount_val,
      I1 => updateCount(1),
      I2 => curState_FSM_FFd2_57,
      I3 => startUpdate_131,
      I4 => curState_FSM_FFd3_58,
      I5 => updateCount(0),
      O => updateCount_1_rstpot_243
    );
  updateCount_0_rstpot : LUT5
    generic map(
      INIT => X"44441444"
    )
    port map (
      I0 => Mcount_updateCount_val,
      I1 => updateCount(0),
      I2 => curState_FSM_FFd2_57,
      I3 => curState_FSM_FFd3_58,
      I4 => startUpdate_131,
      O => updateCount_0_rstpot_244
    );
  Q_n0409_inv1_cepot_INV_0 : INV
    port map (
      I => curState_FSM_FFd1_59,
      O => Q_n0409_inv1_cepot
    );

end Structure;

