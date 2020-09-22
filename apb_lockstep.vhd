library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.lockstep_pkg.all;


entity apb_lockstep is
    generic (
        -- comparator genercis
        min_slack_init  : integer := 100;  -- Number of cycles that the core is going to be stalled
        max_slack_init  : integer := 500;  -- When one core is 'max_instructions_differece" instrucctions ahead of the other, it is stalled
        -- config
        activate_slack      : integer := 1;          -- It activates the module that controls the max and min instruction that one core is ahead of the other
        activate_comparator : integer := 1           -- It activates the module that compares results between both cores
    );
    port (
        rst            : in  std_ulogic;
        clk            : in  std_ulogic;
        -- apb signals
        apbi_psel      : in  std_logic;                       
        apbi_paddr     : in  std_logic_vector(31 downto 0);                      
        apbi_penable   : in  std_logic;                     
        apbi_pwrite    : in  std_logic;
        apbi_pwdata    : in  std_logic_vector(31 downto 0);                   
        apbo_prdata    : out std_logic_vector(31 downto 0);                  
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

architecture rtl of apb_lockstep is

    constant REGISTERS_NUMBER : integer := 6; -- minimum 2
    constant SLV_INDEX_CEIL : integer := integer(ceil(log2(real(REGISTERS_NUMBER))));

    signal r, rin, regs_slack : registers_vector(REGISTERS_NUMBER-1 downto 0) ;
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
            clk      => clk,
            rstn     => rst, 
            icnt1    => icnt1,
            icnt2    => icnt2,
            stall1   => stall1, 
            stall2   => stall2,
            regs_in  => r,
            regs_out => regs_slack
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

    comb : process(rst, r, apbi_psel, apbi_paddr, apbi_penable, apbi_pwrite, apbi_pwdata, regs_slack)
        variable readdata : std_logic_vector(31 downto 0);
        variable v        : registers_vector(REGISTERS_NUMBER-1 downto 0);
        variable slave_index : std_logic_vector(SLV_INDEX_CEIL-1 downto 0);
    begin
        v := r;
        -- select slave
        slave_index := apbi_paddr(SLV_INDEX_CEIL+1 downto 2);
        -- read register
        readdata := (others => '0');
        if (apbi_psel and apbi_penable) = '1' and apbi_pwrite = '0' then
            readdata := r(to_integer(unsigned(slave_index)));
        end if;
        -- write registers (only the first one so far)
        if (apbi_psel and apbi_pwrite) = '1' and unsigned(slave_index) = 0 then
            v(to_integer(unsigned(slave_index))) := apbi_pwdata;
        end if;

        -- update registers
        if rst = '0' then
        -- if systems reset set all registers to 0
            rin <= (others => (others => '0'));
        elsif v(0)(31) = '1' then
        -- if rst bit is set, data from slack handler and reset bit are set to 0
            rin <= (others => (others => '0'));
            rin(0) <= v(0);
            rin(0)(31) <= '0';
        else
            -- change registers with data from slack handler
            rin <= regs_slack;
            -- configuration register shouldn't be changed by the slack handler
            rin(0) <= v(0);
        end if;
        apbo_prdata <= readdata; -- drive apb read bus
    end process;
    

    regs : process(clk)
    begin
        if rising_edge(clk) then r <= rin; end if;
    end process;

end;
