library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library gaisler; 
use gaisler.misc.all;
library grlib;
use grlib.amba.all;
use grlib.devices.all;
library bsc;
use bsc.lockstep_pkg.all;


entity apb_lockstep is
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
        -- comparator signals 
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

architecture rtl of apb_lockstep is

    constant REGISTERS_NUMBER : integer := 2; -- minimum 2
    constant SLV_INDEX_CEIL : integer := integer(ceil(log2(real(REGISTERS_NUMBER))));

    constant REVISION  : integer := 0;
    constant VENDOR_ID : integer := 16#0e#;
    constant DEVICE_ID : integer := 16#002#;

    constant PCONFIG : apb_config_type := (
    0 => ahb_device_reg (VENDOR_ID, DEVICE_ID, 0, REVISION, 0),
    1 => apb_iobar(paddr, pmask));
    signal r, rin : registers_vector(REGISTERS_NUMBER-1 downto 0) ;
    signal enable_comparator : std_logic;

begin
    SLACK: if activate_slack = 1 generate
        slack_handler_inst : slack_handler 
        generic map(
            min_slack_init => min_slack_init,
            max_slack_init => max_slack_init,
            REGISTERS_NUMBER => REGISTERS_NUMBER 
            )
        port map(
            clk     => clk,
            rstn    => rst, 
            icnt1   => icnt1,
            icnt2   => icnt2,
            stall1  => stall1, 
            stall2  => stall2,
            regs    => r
            );
    end generate SLACK;

    COMP: if activate_comparator = 1 generate
        comparator_inst : comparator
        port map(
            clk           => clk,
            rstn          => rst, 
            enable        => enable_comparator,
            alu1          => alu1, 
            alu2          => alu2, 
            pc1           => pc1, 
            pc2           => pc2, 
            reset_program => reset_program
            );
    end generate COMP;

    comb : process(rst, r, apbi)
        variable readdata : std_logic_vector(31 downto 0);
        variable v        : registers_vector(REGISTERS_NUMBER-1 downto 0);
        variable slave_index : std_logic_vector(SLV_INDEX_CEIL-1 downto 0);
    begin
        v := r;
        -- select slave
        slave_index := apbi.paddr(SLV_INDEX_CEIL+1 downto 2);
        -- read register
        readdata := (others => '0');
        if (apbi.psel(pindex) and apbi.penable) = '1' and apbi.pwrite = '0' then
            readdata := r(to_integer(unsigned(slave_index)));
        end if;
        -- write registers
        if (apbi.psel(pindex) and apbi.pwrite) = '1' then
            v(to_integer(unsigned(slave_index))) := apbi.pwdata;
        end if;
        -- system reset
        if rst = '0' then
            v := (others => (others => '0'));
        end if;
        rin <= v;
        apbo.prdata <= readdata; -- drive apb read bus
    end process;
    
    apbo.pirq <= (others => '0');
    apbo.pindex <= pindex;
    apbo.pconfig <= PCONFIG;
    -- No IRQ
    -- VHDL generic
    -- Config constant
    -- registers

    regs : process(clk)
    begin
        if rising_edge(clk) then r <= rin; end if;
    end process;

end;
