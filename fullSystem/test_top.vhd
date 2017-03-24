----------------------------------------------------------------------------
--  test_top.vhd -- Top level component for testing
----------------------------------------------------------------------------
-- Author:  Dinesh Pamunuwa
----------------------------------------------------------------------------
--
----------------------------------------------------------------------------
--  This component instantiates the Rx, Tx and control_unit_test
----------------------------------------------------------------------------
--
----------------------------------------------------------------------------
-- Version:     1.0
-- Revision History:
--  08/01/2013 (Dinesh): Created using Xilinx Tools 14.2 for 64 bit Win
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.common_pack.all;
-- synthesis translate_off
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
use UNISIM.VPKG.ALL;
-- synthesis translate_on

entity PEAK_DETECTOR is
  port (
    clk:  in std_logic;
    reset:  in std_logic; -- synchronous reset
    rxdata: in std_logic;
    txdata: out std_logic
  );
end;

architecture STRUCT of PEAK_DETECTOR is

  component UART_TX_CTRL is
    port(
      SEND    : in STD_LOGIC; -- start Tx (active high)
      DATA    : in STD_LOGIC_VECTOR (7 downto 0); -- parallel data in
      CLK     : in STD_LOGIC; -- system clock
      READY   : out STD_LOGIC; -- Tx done (active high)
      UART_TX : out STD_LOGIC -- serial data out
    );
  end component;

  component UART_RX_CTRL is
  port(
      RxD        : in std_logic; -- serial data in
      sysclk     : in std_logic; -- system clock
      reset      : in std_logic; -- synchronous reset
      rxDone     : in std_logic; -- data succesfully read (active high)
      rcvDataReg : out std_logic_vector(7 downto 0); -- received data
      dataReady  : out std_logic; -- data ready to be read
      setOE      : out std_logic; -- overrun error (active high)
      setFE      : out std_logic  -- frame error (active high)
  );
  end component;


  component cmdProc is
    port(
      clk          : in std_logic;
      reset        : in std_logic;
      rxnow        : in std_logic; --rxvalid
      rxData       : in std_logic_vector (7 downto 0);
      txData       : out std_logic_vector (7 downto 0);
      rxdone       : out std_logic;
      ovErr        : in std_logic;
      framErr      : in std_logic;
      txnow        : out std_logic;
      txdone       : in std_logic;
      start        : out std_logic;
      numWords_bcd : out BCD_ARRAY_TYPE(2 downto 0);
      dataReady    : in std_logic;
      byte         : in std_logic_vector(7 downto 0);
      maxIndex     : in BCD_ARRAY_TYPE(2 downto 0);
      dataResults  : in CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
      seqDone      : in std_logic
    );
  end component;

  component dataConsume is
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
  end component;

  component dataGen is
    port(
      clk     : in std_logic;
      reset   : in std_logic; -- synchronous reset
      ctrlIn  : in std_logic;
      ctrlOut : out std_logic;
      data    : out std_logic_vector(7 downto 0)
    );
  end component;

  --for rx: UART_RX_CTRL use
    --entity work.UART_RX_CTRL(rcvr);

  signal sig_rxnow, sig_rxdone, sig_overr, sig_framerr, sig_txnow, sig_txdone: std_logic;
  signal sig_rxdata, sig_txdata: std_logic_vector (7 downto 0);
  signal sig_dataReady, sig_ctrlIn, sig_ctrlOut, sig_start, sig_seqDone: std_logic;
  signal sig_data, sig_byte: std_logic_vector(7 downto 0);
  signal sig_dataResults: CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
  signal sig_numWords_bcd, sig_maxIndex: BCD_ARRAY_TYPE(2 downto 0);
begin

  gen: dataGen
  port map(
    clk => clk,
    reset => reset,
    ctrlIn => sig_ctrlIn,
    ctrlOut => sig_ctrlOut,
    data => sig_data
  );

  data: dataConsume
  port map(
    clk => clk,
    reset => reset,
    ctrlIn => sig_ctrlOut,
    ctrlOut => sig_ctrlIn,
    data => sig_data,
    start => sig_start,
    numWords_bcd => sig_numWords_bcd,
    dataReady => sig_dataReady,
    byte => sig_byte,
    maxIndex => sig_maxIndex,
    dataResults => sig_dataResults,
    seqDone => sig_seqDone
  );

  cmd: cmdProc
  port map(
    clk => clk,
    reset => reset,
    rxnow => sig_rxnow,
    rxData => sig_rxdata,
    txData => sig_txdata,
    rxdone => sig_rxdone,
    ovErr => sig_overr,
    framErr => sig_framerr,
    txnow => sig_txnow,
    txdone => sig_txdone,
    start => sig_start,
    numWords_bcd => sig_numWords_bcd,
    dataReady => sig_dataReady,
    byte => sig_byte,
    maxIndex => sig_maxIndex,
    dataResults => sig_dataResults,
    seqDone => sig_seqDone
  );

  tx: UART_TX_CTRL
  port map(
    SEND => sig_txnow,
    DATA => sig_txdata,
    CLK => clk,
    READY => sig_txdone,
    UART_TX => txdata
  );

  rx: UART_RX_CTRL
  port map(
    RxD => rxdata, -- input serial line
    sysclk => clk,
    reset => reset,
    rxDone => sig_rxdone,
    rcvDataReg => sig_rxdata,
    dataReady => sig_rxnow,
    setOE => sig_overr,
    setFE =>  sig_framerr
   );

end;