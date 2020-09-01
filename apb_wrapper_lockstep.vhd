library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library grlib;
use grlib.amba.all;
use grlib.devices.all;
library bsc;
use bsc.lockstep_pkg.all;


entity apb_wrapper_lockstep is
    generic (
        -- apb generics
        pindex : integer := 0;
        paddr  : integer := 0;
        pmask  : integer := 16#fff#;
        -- comparator genercis
        min_slack_init  : integer := 100;  -- Number of cycles that the core is going to be stalled
        max_slack_init  : integer := 500;  -- When one core is 'max_instructions_differece" instrucctions ahead of the other, it is stalled
        -- config
        activate_slack      : integer := 1;          -- It activates the module that controls the max and min instruction that one core is ahead of the other
        activate_comparator : integer := 1           -- It activates the module that compares results between both cores
    );
    port (
        -- apb signals
        rst            : in  std_ulogic;
        clk            : in  std_ulogic;
        apbi           : in  apb_slv_in_type;
        apbo           : out apb_slv_out_type;
        -- lockstep signals 
        icnt1          : in  std_logic_vector(1 downto 0);    -- Instruction counter from the first core
        icnt2          : in  std_logic_vector(1 downto 0);    -- Instruction counter from the second core
        alu1           : in  std_logic_vector(63 downto 0);   -- Result from the first core ALU 
        alu2           : in  std_logic_vector(63 downto 0);   -- Result from the second core ALU 
        pc1            : in  std_logic_vector(63 downto 0);   -- Current PC of the first core
        pc2            : in  std_logic_vector(63 downto 0);   -- Current PC of the second core
        stall1         : out std_logic;                       -- Signal to stall the first core
        stall2         : out std_logic;                       -- Signal to stall the second core
        reset_program  : out std_logic                        -- Reset the program if the result of both ALUs does not match
    );
    end;

architecture rtl of apb_wrapper_lockstep is

    -- apb wrapper signals
    constant REVISION  : integer := 0;
    constant VENDOR_ID : integer := 16#0e#;
    constant DEVICE_ID : integer := 16#002#;

    constant PCONFIG : apb_config_type := (
    0 => ahb_device_reg (VENDOR_ID, DEVICE_ID, 0, REVISION, 0),
    1 => apb_iobar(paddr, pmask));

begin
    
    apb_lockstep_inst : apb_lockstep
    generic map(
        -- comparator genercis
        min_slack_init => min_slack_init,
        max_slack_init => max_slack_init,
        -- config
        activate_slack      => activate_slack,
        activate_comparator => activate_comparator
        )
    port map(
        rst           => rst, 
        clk           => clk, 
        -- apb signals
        apbi_psel     => apbi.psel(pindex),     
        apbi_paddr    => apbi.paddr,    
        apbi_penable  => apbi.penable,  
        apbi_pwrite   => apbi.pwrite,   
        apbi_pwdata   => apbi.pwdata,   
        apbo_prdata   => apbo.prdata,   
        -- lockstep signals
        icnt1         => icnt1,
        icnt2         => icnt2,        
        alu1          => alu1,         
        alu2          => alu2,         
        pc1           => pc1,          
        pc2           => pc2,           
        stall1        => stall1,       
        stall2        => stall2,       
        reset_program => reset_program
    );

        
    apbo.pirq <= (others => '0');
    apbo.pindex <= pindex;
    apbo.pconfig <= PCONFIG;
    -- No IRQ
    -- VHDL generic
    -- Config constant
    -- registers

    
end;
