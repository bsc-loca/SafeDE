library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;
library grlib;
use grlib.amba.all;



package lockstep_top_component is
     component apb_wrapper_lockstep is
        generic (
            -- apb generics
            pindex : integer := 0;
            paddr  : integer := 0;
            pmask  : integer := 16#fff#;
            -- comparator genercis
            min_slack_init  : integer := 500;  -- Number of cycles that the core is going to be stalled
            max_slack_init  : integer := 100;  -- When one core is 'max_instructions_differece" instrucctions ahead of the other, it is stalled.
            -- config
            activate_slack      : integer := 1;          -- It activates the module that controls the max and min instruction that one core is ahead of the other.
            activate_comparator : integer := 1           -- It activates the module that compares results between both cores

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
    end component apb_wrapper_lockstep; 

end lockstep_top_component;
