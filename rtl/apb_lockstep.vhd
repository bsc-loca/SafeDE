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
        activate_comparator : integer := 0           -- It activates the module that compares results between both cores
    );
    port (
        rst            : in  std_ulogic;
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
end;

architecture rtl of apb_lockstep is

    constant REGISTERS_NUMBER : integer := 10; -- minimum 2
    constant SLV_INDEX_CEIL : integer := integer(ceil(log2(real(REGISTERS_NUMBER))));

    -- registers signals
    signal r, rin     : registers_vector(REGISTERS_NUMBER-1 downto 0) ;
    signal regs_handler_o : registers_vector(REGISTERS_NUMBER-1 downto 1) ;

    -- configuration signals
    signal max_slack : std_logic_vector(14 downto 0);
    signal min_slack : std_logic_vector(14 downto 0);
    signal enable : std_logic;


begin

    -- Enable module, min and max slack
    enable <= r(0)(0);
    max_slack <= r(0)(30 downto 16) when unsigned(r(0)(30 downto 16)) /= 0 else
                 std_logic_vector(to_unsigned(max_slack_init, 15));
    min_slack <= r(0)(15 downto  1) when unsigned(r(0)(15 downto  1)) /= 0 else
                 std_logic_vector(to_unsigned(min_slack_init, 15));


    SLACK: if activate_slack = 1 generate
        slack_handler_inst : slack_handler 
        generic map(
            REGISTERS_NUMBER => REGISTERS_NUMBER 
            )
        port map(
            clk           => clk,
            rstn          => rst, 
            enable        => enable,
            icnt1_i       => icnt1_i,
            icnt2_i       => icnt2_i,
            min_slack_i   => min_slack,
            max_slack_i   => max_slack, 
            regs_in       => r(REGISTERS_NUMBER-1 downto 1),
            regs_out      => regs_handler_o,
            c1_ahead_c2_o => open,
            stall1_o      => stall1_o, 
            stall2_o      => stall2_o
            );
    end generate SLACK;

    --COMP: if activate_comparator = 1 generate
    --    comparator_inst : comparator
    --    port map(
    --        clk           => clk,
    --        rstn          => rst, 
    --        enable        => enable,
    --        --alu1          => alu1, 
    --        --alu2          => alu2, 
    --        --pc1           => pc1, 
    --        --pc2           => pc2, 
    --        reset_program       => error_o
    --        );
    --end generate COMP;

    --Here the registers take their new values. It can be:
    -- 1.- A change in the configuration register
    -- 2.- Reset if the 31th bit of the configuration register is set to 1
    -- 3.- Just update the counters from the slack handler
    comb : process(rst, r, apbi_psel_i, apbi_paddr_i, apbi_penable_i, apbi_pwrite_i, apbi_pwdata_i, regs_handler_o)
        variable readdata : std_logic_vector(31 downto 0);
        variable v        : registers_vector(REGISTERS_NUMBER-1 downto 0);
        variable slave_index : std_logic_vector(SLV_INDEX_CEIL-1 downto 0);
    begin
        v := r;
        -- select slave
        slave_index := apbi_paddr_i(SLV_INDEX_CEIL+1 downto 2);
        -- read register
        readdata := (others => '0');
        if (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' then
            readdata := r(to_integer(unsigned(slave_index)));
        end if;
        -- write registers (only the first one so far)
        if (apbi_psel_i and apbi_pwrite_i) = '1' and unsigned(slave_index) = 0 then
            v(to_integer(unsigned(slave_index))) := apbi_pwdata_i;
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
            rin(REGISTERS_NUMBER-1 downto 1)  <= regs_handler_o;
            -- configuration register shouldn't be changed by the slack handler
            rin(0) <= v(0);
        end if;
        apbo_prdata_o <= readdata; -- drive apb read bus
    end process;
    

    regs : process(clk)
    begin
        if rising_edge(clk) then r <= rin; end if;
    end process;

end;
