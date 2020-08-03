library ieee; 
use ieee.std_logic_1164.all;
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
        min_slack  : integer := 1000;  -- Number of cycles that the core is going to be stalled
        max_slack  : integer := 1000   -- When one core is 'max_instructions_differece" instrucctions ahead of the other, it is stalled.
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

    constant REVISION  : integer := 0;
    constant VENDOR_ID : integer := 16#0e#;
    constant DEVICE_ID : integer := 16#002#;

    constant PCONFIG : apb_config_type := (
    0 => ahb_device_reg (VENDOR_ID, DEVICE_ID, 0, REVISION, 0),
    1 => apb_iobar(paddr, pmask));
    type registers is record
        reg : std_logic_vector(31 downto 0);
    end record;
    signal r, rin : registers;
    signal enable_comparator : std_logic;

begin
    slack_handler_inst : slack_handler 
    generic map(
        min_slack => min_slack,
        max_slack => max_slack
        )
    port map(
        clk     => clk,
        rstn    => rst, 
        enable  => enable_comparator,
        icnt1   => icnt1,
        icnt2   => icnt2,
        stall1  => stall1, 
        stall2  => stall2
        );

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

    comb : process(rst, r, apbi)
        variable readdata : std_logic_vector(31 downto 0);
        variable v        : registers;
    begin
        v := r;
        -- read register
        readdata := (others => '0');
        if (apbi.psel(pindex) and apbi.penable) = '1' and apbi.pwrite = '0' then
            readdata := r.reg(31 downto 0);
        end if;
        -- write registers
        if (apbi.psel(pindex) and apbi.penable and apbi.pwrite) = '1' then
            v.reg := apbi.pwdata;
        end if;
        -- system reset
        if rst = '0' then
            v.reg := (others => '0');
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

    -- calculate enable 
    process(r)
        variable ored_bits : std_logic;
    begin
        ored_bits := r.reg(0) or r.reg(1);
        for index in 2 to 31 loop
            ored_bits := ored_bits or r.reg(index);
        end loop; 
        enable_comparator <= ored_bits;
    end process;
end;
