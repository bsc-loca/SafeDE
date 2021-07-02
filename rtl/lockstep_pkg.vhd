library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;


package lockstep_pkg is

    -- Types definitions

    type registers_vector is array (integer range <>) of std_logic_vector(31 downto 0);


    -- Components definitions 
    component apb_lockstep is
        generic (
            lanes_number        : integer := 2;
            register_output     : integer := 0;   -- If is 1, the output is registered. Can be used to improve timing
            min_slack_init      : integer := 20   -- If no min_slack is configured through the API, this will be take as the default minimum threshold
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
    end component apb_lockstep;

    component slack_handler is
        generic (
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
            max_slack_i    : in  std_logic_vector(14 downto 0);
            min_slack_i    : in  std_logic_vector(14 downto 0);
            regs_in        : in  registers_vector(REGISTERS_NUMBER-1 downto 3); -- Registers of the module (in)
            regs_out       : out registers_vector(REGISTERS_NUMBER-1 downto 3); -- Registers of the module (out) 
            stall1_o       : out std_logic;                                     -- Signal to stall the first core
            stall2_o       : out std_logic;                                     -- Signal to stall the second core
            error_o        : out std_logic                                      -- Signal to assert an error
        );
    end component slack_handler;

end lockstep_pkg;





