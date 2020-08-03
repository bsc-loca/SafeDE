library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;
library grlib;
use grlib.amba.all;


package lockstep_pkg is

    component slack_handler is
        generic (
            min_slack  : integer := 100;  -- Minimum difference of instrucctions between both cores
            max_slack  : integer := 500   -- Maximum difference of instrucctions between both cores
        );
        port (
            clk            : in  std_logic;    
            rstn           : in  std_logic;
            enable         : in  std_logic;                       -- Enables the module 
            icnt1          : in  std_logic_vector(1 downto 0);    -- Instruction counter from the first core
            icnt2          : in  std_logic_vector(1 downto 0);    -- Instruction counter from the second core
            stall1         : out std_logic;                       -- Signal to stall the first core
            stall2         : out std_logic                        -- Signal to stall the second core
        );
    end component slack_handler;

    component comparator is
        port(
            clk            : in  std_logic;
            rstn           : in  std_logic;
            enable         : in  std_logic;                       -- Enables the module
            alu1           : in  std_logic_vector(63 downto 0);   -- Result form the first core ALU
            alu2           : in  std_logic_vector(63 downto 0);   -- Result form the second core ALU
            pc1            : in  std_logic_vector(63 downto 0);   -- Current PC of the first core
            pc2            : in  std_logic_vector(63 downto 0);   -- Current PC of the second core
            reset_program  : out std_logic                        -- Reset the program if the result of both ALUs does not match
        );
    end component comparator;


    component apb_lockstep is
        generic (
            -- apb generics
            pindex : integer := 0;
            paddr  : integer := 0;
            pmask  : integer := 16#fff#;
            -- comparator genercis
            min_slack  : integer := 1000;  -- Number of cycles that the core is going to be stalled
            max_slack  : integer := 1000   -- When one core is 'max_instructions_differece" instrucctions ahead of the other, it is stalled.
        );
        port (
            -- apb signals
            rst           : in  std_ulogic;
            clk           : in  std_ulogic;
            apbi          : in  apb_slv_in_type;
            apbo          : out apb_slv_out_type;
            -- comparator signals
            icnt1         : in  std_logic_vector(1 downto 0);    -- Instruction counter from the first core
            icnt2         : in  std_logic_vector(1 downto 0);    -- Instruction counter from the second core
            alu1          : in  std_logic_vector(63 downto 0);   -- Result form the first core ALU
            alu2          : in  std_logic_vector(63 downto 0);   -- Result form the second core ALU
            pc1           : in  std_logic_vector(63 downto 0);   -- Current PC of the first core
            pc2           : in  std_logic_vector(63 downto 0);   -- Current PC of the second core
            stall1        : out std_logic;                       -- Signal to stall the first core
            stall2        : out std_logic;                       -- Signal to stall the second core
            reset_program : out std_logic                        -- Reset the program if the result of both ALUs does not match
        );
    end component apb_lockstep;


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
