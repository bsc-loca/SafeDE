library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;


package lockstep_pkg is

    -- Types definitions

    type registers_vector is array (integer range <>) of std_logic_vector(31 downto 0);


    -- Components definitions 
    component apb_lockstep is
        generic (
            register_output     : integer := 0;   -- If is 1, the output is registered. Can be used to improve timing
            min_slack_init      : integer := 20;  -- If no min_slack is configured through the API, this will be take as the default minimum threshold
            activate_max_slack  : integer := 0    -- When it is set to 1, the module also can be configured through the API o control that the difference of instructions
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

    component slack_handler_max is
        generic (
            en_cycles_limit  : integer := 100;
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
    end component slack_handler_max;

    component slack_handler is
        generic (
            en_cycles_limit  : integer := 100;
            REGISTERS_NUMBER : integer := 14       -- Number of registers
        );
        port (
            clk            : in  std_logic;    
            rstn           : in  std_logic;
            enable_core1_i : in  std_logic;
            enable_core2_i : in  std_logic;
            icnt1_i        : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the first core
            icnt2_i        : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the second core
            min_slack_i    : in  std_logic_vector(14 downto 0);
            regs_in        : in  registers_vector(REGISTERS_NUMBER-1 downto 3); -- Registers of the module (in)
            regs_out       : out registers_vector(REGISTERS_NUMBER-1 downto 3); -- Registers of the module (out) 
            stall1_o       : out std_logic;                                     -- Signal to stall the first core
            stall2_o       : out std_logic;                                     -- Signal to stall the second core
            error_o        : out std_logic                                      -- Signal to assert an error
        );
    end component slack_handler;

    component comparator is
        generic (
            WRITE_ENTRIES : integer := 256;
            BUS_LENGTH    : integer := 128
        );
        port(
            clk            : in  std_logic;
            rstn           : in  std_logic;
            -- ahb signals ----------------
            ahbmo1_write_i : in  std_logic;
            ahbmo2_write_i : in  std_logic;
            ahbmo1_wdata_i : in  std_logic_vector(BUS_LENGTH-1 downto 0);
            ahbmo2_wdata_i : in  std_logic_vector(BUS_LENGTH-1 downto 0);
            ahbmo1_trans_i : in  std_logic;
            ahbmo2_trans_i : in  std_logic;
            ahbmi_rdata_i  : in  std_logic_vector(BUS_LENGTH-1 downto 0);
            ahbmi_hresp_i  : in  std_logic_vector(1 downto 0);
            ahbmi_ready_i  : in  std_logic;
            ahb_access_i   : in std_logic_vector(1 downto 0);
            -------------------------------
            enable_core1_i : in  std_logic; 
            enable_core2_i : in  std_logic;
            c1_ahead_c2_i  : in  std_logic; 
            error_o        : out std_logic
        );
    end component comparator;

    component fifo is
        generic (
            RAM_LENGTH : integer := 256;
            BUS_LENGTH : integer := 128
        );
        port(
            clk            : in  std_logic;
            rstn           : in  std_logic;
            write_i        : in  std_logic;
            read_i         : in  std_logic;
            wdata_i        : in  std_logic_vector(BUS_LENGTH-1 downto 0);
            rdata_o        : out std_logic_vector(BUS_LENGTH-1 downto 0)
        );
    end component fifo;


    component lockstep_testbench is
        port (
            rst                  : in  std_ulogic;
            clk                  : in  std_ulogic;
            stall2_i             : in  std_logic;
            icnt2_o              : out std_logic_vector(1 downto 0)
        );
    end component lockstep_testbench;



--    function generate_random_number(seed : positive; number_range : real) return integer;

end lockstep_pkg;




--------------------------------------------------------------------------------------------
------------------------------------unused function-----------------------------------------
--------------------------------------------------------------------------------------------
--package body comparator_module is
--
--      -- Function to generate a random number between 0 and number_range
--    function generate_random_number(seed : positive; number_range : real) return integer is
--        variable seed1, seed2 : positive;
--        variable x : real;
--        variable y : integer;
--    begin
--        seed1 := seed;
--        uniform(seed1, seed2, x);
--        y := integer(floor(x * number_range));
--
--        return y;
--    end function;
--
--end comparator_module;
