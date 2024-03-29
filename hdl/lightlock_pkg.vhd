-----------------------------------------------------------------------------------------------------------------------------------------------
--
--   Project              :  SafeDE
--   File name            :  lightlock_pkg.vhd
--   Title                :  lightlock_pkg
--   Description          :  Pakage containing the all the component and type definitions used in SafeDE module
--   Design library       :  None
--   Analysis dependency  :  None
--   Initialization       :  Initialized by RESET
--   Notes                :  None
--   Simulator(s)         :  QuestaSim-64 10.7c
-----------------------------------------------------------------------------------------------------------------------------------------------
--    Revisions :
--           Date           Author        Revision             Comments
--        04/30/2022     Francisco Bas      1.0         Finished first version
--                       francisco.basjalon@bsc.es
-----------------------------------------------------------------------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;


package lightlock_pkg is

    -- Types definitions

    type registers_vector is array (integer range <>) of std_logic_vector(31 downto 0);


    -- Components definitions 
    component apb_lightlock is
        generic (
            lanes_number        : integer := 2;
            register_output     : integer := 0;   -- If is 1, the output is registered. Can be used to improve timing
            register_input      : integer := 2;   -- The inputs (icnts) are registered with as many registers as the value of register_input
            en_cycles_limit     : integer := 500;
            min_staggering_init : integer := 20   -- If no min_staggering is configured through the API, this will be take as the default minimum threshold
        );                                        -- between both cores is never bigger than a maximum threshold. Otherwise only the minimum threshold is taken on account.
        port (
            rstn           : in  std_ulogic;
            clk            : in  std_ulogic;
            -- apb signals
            apbi_psel_i    : in  std_logic;                       
            apbi_paddr_i   : in  std_logic_vector(31 downto 0);                      
            apbi_penable_i : in  std_logic;                     
            apbi_pwrite_i  : in  std_logic;
            apbi_pwdata_i  : in  std_logic_vector(31 downto 0);                   
            apbo_prdata_o  : out std_logic_vector(31 downto 0);                  
            -- lockstep signals 
            icnt1_i        : in  std_logic_vector(1 downto 0);    -- Instruction counter from the first core
            icnt2_i        : in  std_logic_vector(1 downto 0);    -- Instruction counter from the second core
            stall1_o       : out std_logic;                       -- Signal to stall the first core
            stall2_o       : out std_logic;                       -- Signal to stall the second core
            error_o        : out std_logic                        -- Reset the program if the result of both ALUs does not match
        );
    end component apb_lightlock;

    component staggering_handler is
        generic (
            register_input   : integer := 0;
            register_output  : integer := 0;
            lanes_number     : integer := 2;    
            en_cycles_limit  : integer := 500;
            REGISTERS_NUMBER : integer := 14       -- Number of registers
        );
        port (
            clk            : in  std_logic;    
            rstn           : in  std_logic;
            enable_core1_i : in  std_logic;
            enable_core2_i : in  std_logic;
            icnt1_i        : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the first core
            icnt2_i        : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the second core
            max_staggering_i    : in  std_logic_vector(14 downto 0);
            min_staggering_i    : in  std_logic_vector(14 downto 0);
            regs_in        : in  registers_vector(REGISTERS_NUMBER-1 downto 3); -- Registers of the module (in)
            regs_out       : out registers_vector(REGISTERS_NUMBER-1 downto 3); -- Registers of the module (out) 
            stall1_o       : out std_logic;                                     -- Signal to stall the first core
            stall2_o       : out std_logic;                                     -- Signal to stall the second core
            error_o        : out std_logic                                      -- Signal to assert an error
        );
    end component staggering_handler;

end lightlock_pkg;





