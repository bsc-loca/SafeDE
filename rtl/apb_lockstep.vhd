library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.lockstep_pkg.all;


entity apb_lockstep is
    generic (
        lanes_number        : integer := 2;   -- Number of lanes of each core (1 icnt bit per lane)
        register_output     : integer := 0;   -- If is 1, the output is registered. Can be used to improve timing
        min_slack_init      : integer := 20   -- If no min_slack is configured through the API, this will be take as the default minimum threshold
    );                                        -- between both cores is never bigger than a maximum threshold. Otherwise only the minimum threshold is taken on account.
    port (
        rstn           : in  std_ulogic;
        clk            : in  std_ulogic;
        -- APB signals
        apbi_psel_i    : in  std_logic;                       
        apbi_paddr_i   : in  std_logic_vector(31 downto 0);                      
        apbi_penable_i : in  std_logic;                     
        apbi_pwrite_i  : in  std_logic;
        apbi_pwdata_i  : in  std_logic_vector(31 downto 0);                   
        apbo_prdata_o  : out std_logic_vector(31 downto 0);                  
        -- lockstep signals 
        icnt1_i        : in  std_logic_vector(lanes_number-1 downto 0);    -- Instruction counter from the first core
        icnt2_i        : in  std_logic_vector(lanes_number-1 downto 0);    -- Instruction counter from the second core
        stall1_o       : out std_logic;                                    -- Signal to stall the first core
        stall2_o       : out std_logic;                                    -- Signal to stall the second core
        error_o        : out std_logic                                     -- Reset the program if the result of both ALUs does not match
    );
end;

architecture rtl of apb_lockstep is

    constant REGISTERS_NUMBER : integer := 13; -- minimum 2
    constant SLV_INDEX_CEIL   : integer := integer(ceil(log2(real(REGISTERS_NUMBER))));

    -- registers signals
    signal r, rin         : registers_vector(REGISTERS_NUMBER-1 downto 0) ;
    signal regs_handler_o : registers_vector(REGISTERS_NUMBER-1 downto 3) ;

    -- configuration signals
    signal max_slack, min_slack : std_logic_vector(14 downto 0);
    signal enable_core1, enable_core2 : std_logic;

    -- error signals
    signal error_from_sh : std_logic;

    -- Soft reset
    signal soft_rstn, rstn_int : std_logic;
    
    -- Global enable
    signal global_enable : std_logic;
    signal r_stall1, r_stall2, stall1 , stall2 : std_logic;


begin
    
    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- CONFIGURATION REGISTERS -----------------------------------------------------------------------------------------------------------------------------
    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- The values for the configuration registers are taken from the APB interface
    global_enable <= r(0)(30);
    enable_core1  <= r(1)(0); -- Set to 1 when the core1 enters in the critical section
    enable_core2  <= r(2)(0); -- Set to 1 when the core1 enters in the critical section
    -- If no max_slack is especified through the API, the signal max_slack will get the maximum value
    -- to prevent an overflow
    max_slack <= r(0)(29 downto 15) when unsigned(r(0)(29 downto 15)) /= 0 else
                 (others => '1'); 
    -- If no min_slack is specified throuhg the API, the signal min_slack will take the value of
    -- the generic min_slack_init 
    min_slack <= r(0)(14 downto 0) when unsigned(r(0)(14 downto 0)) /= 0 else
                 std_logic_vector(to_unsigned(min_slack_init, 15));
    rstn_int <= soft_rstn and rstn;


    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- COMPONENT INSTANTIATION -----------------------------------------------------------------------------------------------------------------------------
    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- This component is encharged of handling the instruction difference between both cores or slack. When the slack is out of the allowed limits
    -- it sets to 1 the stall signal of the core that has to be stalled.
    slack_handler_inst : slack_handler 
    generic map(
        register_output  => register_output,
        lanes_number     => lanes_number,
        en_cycles_limit  => 100,
        REGISTERS_NUMBER => REGISTERS_NUMBER 
        )
    port map(
        clk            => clk,
        rstn           => rstn_int,
        enable_core1_i => enable_core1,
        enable_core2_i => enable_core2,
        icnt1_i        => icnt1_i,
        icnt2_i        => icnt2_i,
        min_slack_i    => min_slack,
        max_slack_i    => max_slack, 
        regs_in        => r(REGISTERS_NUMBER-1 downto 3),
        regs_out       => regs_handler_o,
        stall1_o       => stall1, 
        stall2_o       => stall2,
        error_o        => error_from_sh
        );

    
    -- Depending on the generic register_output, the output will be registered or not.
    -- This is useful to break the combiantional path and prevent timing problems.
    NO_REGISTERED_OUTPUT: if register_output = 0 generate
        r_stall1 <= stall1;
        r_stall2 <= stall2;
        stall1_o <= r_stall1 and global_enable;
        stall2_o <= r_stall2 and global_enable;
    end generate NO_REGISTERED_OUTPUT;

    REGISTERED_OUTPUT: if register_output = 1 generate
        process(clk)
        begin
            if rising_edge(clk) then
                if rstn = '0' then
                    r_stall1  <= '0';                     
                    r_stall2  <= '0';                     
       	        else
                    r_stall1  <= stall1;        -- Register used to detect the flank 
                    r_stall2  <= stall2;        -- Register used to detect the flank 
                end if;
            end if;   
        end process;
        stall1_o <= r_stall1 and global_enable;
        stall2_o <= r_stall2 and global_enable;
    end generate REGISTERED_OUTPUT;

        
    --------------------------------------------------------------------------------------------------------------------------------------------------------


    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- APB INTERFACE ---------------------------------------------------------------------------------------------------------------------------------------
    --------------------------------------------------------------------------------------------------------------------------------------------------------
    comb : process(rstn, r, apbi_psel_i, apbi_paddr_i, apbi_penable_i, apbi_pwrite_i, apbi_pwdata_i, regs_handler_o)
        variable readdata : std_logic_vector(31 downto 0);
        variable v        : registers_vector(REGISTERS_NUMBER-1 downto 0);
        variable slave_index : std_logic_vector(SLV_INDEX_CEIL-1 downto 0);
    begin
        v := r;
        -- select slave
        slave_index := apbi_paddr_i(SLV_INDEX_CEIL+1 downto 2);
        -- Read statistical registers
        readdata := (others => '0');
        if (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' then
            readdata := r(to_integer(unsigned(slave_index)));
        end if;
        -- Write registers configuration registers
        if (apbi_psel_i and apbi_pwrite_i) = '1' and unsigned(slave_index) = 0 then
            v(to_integer(unsigned(slave_index))) := apbi_pwdata_i;
        elsif (apbi_psel_i and apbi_pwrite_i) = '1' and unsigned(slave_index) = 1 then
            v(to_integer(unsigned(slave_index))) := apbi_pwdata_i;
        elsif (apbi_psel_i and apbi_pwrite_i) = '1' and unsigned(slave_index) = 2 then
            v(to_integer(unsigned(slave_index))) := apbi_pwdata_i;
        end if;

        if rstn = '0' then
            rin <= (others => (others => '0'));
            -- Register containing minimum instructions difference should be preset to a high value
            rin(12) <= (others => '1');
        elsif v(0)(31) = '1' then
        -- if softreset bit is set, data from slack handler and reset bit are set to 0
            rin <= (others => (others => '0'));
            -- Register containing minimum instructions difference should be reset to a high value
            rin(12) <= (others => '1');
            rin(0) <= v(0);
            rin(0)(31) <= '0';
        else
            -- change registers with data from slack handler
            rin(REGISTERS_NUMBER-1 downto 3)  <= regs_handler_o;
            -- configuration register shouldn't be changed by the slack handler
            rin(0) <= v(0);
            rin(1) <= v(1);
            rin(2) <= v(2);
        end if;
        apbo_prdata_o <= readdata; -- drive apb read bus
        -- soft reset
        soft_rstn <= not v(0)(31);
    end process;
    

    regs : process(clk)
    begin
        if rising_edge(clk) then r <= rin; end if;
    end process;
    --------------------------------------------------------------------------------------------------------------------------------------------------------

    ------------------- ERROR -------------------
    error_o <= error_from_sh;

    -- Verification -----------------
    -- pragma translate_off
    process
    begin
        wait for 1 ns;
        assert (error_from_sh = '0') report "Result: error detected by lockstep" severity warning;
    end process;
    -- pragma translate_on
    

end;
