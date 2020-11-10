library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;


package lockstep_pkg is

    -- Types definitions

    type registers_vector is array (integer range <>) of std_logic_vector(31 downto 0);


    -- Components definitions 
    component apb_lockstep is
        generic (
            -- comparator genercis
            min_slack_init  : integer := 100;  -- Number of cycles that the core is going to be stalled
            max_slack_init  : integer := 500;  -- When one core is 'max_instructions_differece" instrucctions ahead of the other, it is stalled
            -- config
            activate_slack      : integer := 1;          -- It activates the module that controls the max and min instruction that one core is ahead of the other
            activate_comparator : integer := 1;          -- It activates the module that compares results between both cores
            -- ahb bus
            BUS_LENGTH : integer := 128
        );  
        port (
            rstn           : in  std_ulogic;
            clk            : in  std_ulogic;
            -- apb signals
            apbi_psel_i     : in  std_logic;    
            apbi_paddr_i    : in  std_logic_vector(31 downto 0);    
            apbi_penable_i  : in  std_logic;    
            apbi_pwrite_i   : in  std_logic;    
            apbi_pwdata_i   : in  std_logic_vector(31 downto 0);    
            apbo_prdata_o   : out std_logic_vector(31 downto 0);    
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
            -- lockstep signals 
            icnt1_i          : in  std_logic_vector(1 downto 0);    -- Instruction counter from the first core
            icnt2_i          : in  std_logic_vector(1 downto 0);    -- Instruction counter from the second core
            stall1_o         : out std_logic;                       -- Signal to stall the first core
            stall2_o         : out std_logic;                       -- Signal to stall the second core
            error_o          : out std_logic                        -- Reset the program if the result of both ALUs does not match
        );  
    end component apb_lockstep;

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
            max_slack_i    : in  std_logic_vector(9 downto 0);
            min_slack_i    : in  std_logic_vector(9 downto 0);
            regs_in        : in  registers_vector(REGISTERS_NUMBER-1 downto 3); -- Registers of the module (in)
            regs_out       : out registers_vector(REGISTERS_NUMBER-1 downto 3); -- Registers of the module (out) 
            c1_ahead_c2_o  : out std_logic;                                     -- It is 1 when core1 is ahead of core2 and the other way round
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
