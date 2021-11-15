library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library safety;
use safety.lightlock_pkg.all;


entity apb_lightlock is
    generic (
        lanes_number        : integer := 2;   -- Number of lanes of each core (1 icnt bit per lane)
        register_output     : integer := 3;   -- The outputs (stalls) are registered with as many registers as the value of register_output
        register_input      : integer := 3;   -- The inputs (icnts) are registered with as many registers as the value of register_input
        en_cycles_limit     : integer := 500; -- If one core activates lockstep and the other core doesn't activate the lockstep before 500 cycles it rise an interrupt
        min_staggering_init : integer := 20   -- If no min_staggering is configured through the API, this will be take as the default minimum threshold
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

architecture rtl of apb_lightlock is

    constant REGISTERS_NUMBER : integer := 13; -- minimum 2
    constant SLV_INDEX_CEIL   : integer := integer(ceil(log2(real(REGISTERS_NUMBER))));

    -- registers signals
    signal r, rin         : registers_vector(REGISTERS_NUMBER-1 downto 0) ;
    signal regs_handler_o : registers_vector(REGISTERS_NUMBER-1 downto 3) ;

    -- configuration signals
    signal max_staggering, min_staggering : std_logic_vector(14 downto 0);
    signal enable_core1, enable_core2 : std_logic;

    -- error signals
    signal error_from_sh : std_logic;

    -- Soft reset
    signal soft_rstn, rstn_int : std_logic;
    
    -- Global enable
    signal global_enable : std_logic;

    -- OUTPUTS
    signal stall1 , stall2 : std_logic;
    signal r_stall1, r_stall2, n_stall1, n_stall2 : std_logic_vector(register_output-1 downto 0);

    -- INPUTS
    signal r_icnt1, r_icnt2, n_icnt1, n_icnt2 : std_logic_vector((register_input)*lanes_number-1 downto 0);
    signal icnt1, icnt2 : std_logic_vector(1 downto 0);


begin
    
    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- CONFIGURATION REGISTERS -----------------------------------------------------------------------------------------------------------------------------
    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- The values for the configuration registers are taken from the APB interface
    global_enable <= r(0)(30);
    enable_core1  <= r(1)(0); -- Set to 1 when the core1 enters in the critical section
    enable_core2  <= r(2)(0); -- Set to 1 when the core1 enters in the critical section
    -- If no max_staggering is especified through the API, the signal max_staggering will get the maximum value
    -- to prevent an overflow
    max_staggering <= r(0)(29 downto 15) when unsigned(r(0)(29 downto 15)) /= 0 else
                 std_logic_vector(to_unsigned(32750, 15)); -- Set it to 32750 to have a margin with its biggest value 32767 and avoid overflow
    -- If no min_staggering is specified throuhg the API, the signal min_staggering will take the value of
    -- the generic min_staggering_init 
    min_staggering <= r(0)(14 downto 0) when unsigned(r(0)(14 downto 0)) /= 0 else
                 std_logic_vector(to_unsigned(min_staggering_init, 15));
    rstn_int <= soft_rstn and rstn;


    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- COMPONENT INSTANTIATION -----------------------------------------------------------------------------------------------------------------------------
    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- This component is encharged of handling the instruction difference between both cores or staggering. When the staggering is out of the allowed limits
    -- it sets to 1 the stall signal of the core that has to be stalled.
    staggering_handler_inst : staggering_handler 
    generic map(
        register_input   => register_input,
        register_output  => register_output,
        lanes_number     => lanes_number,
        en_cycles_limit  => en_cycles_limit,
        REGISTERS_NUMBER => REGISTERS_NUMBER 
        )
    port map(
        clk              => clk,
        rstn             => rstn_int,
        enable_core1_i   => enable_core1,
        enable_core2_i   => enable_core2,
        icnt1_i          => icnt1,
        icnt2_i          => icnt2,
        min_staggering_i => min_staggering,
        max_staggering_i => max_staggering, 
        regs_in          => r(REGISTERS_NUMBER-1 downto 3),
        regs_out         => regs_handler_o,
        stall1_o         => stall1, 
        stall2_o         => stall2,
        error_o          => error_from_sh
        );

    
    -- Depending on the generics register_output and register_input, the outputs and the
    -- inputs will be registered or not.
    -- This is useful to break the combiantional path and prevent timing problems.

    --If the outputs are not registered
    NO_REGISTERED_OUTPUT: if register_output = 0 generate
        stall1_o <= stall1 and global_enable;
        stall2_o <= stall2 and global_enable;
    end generate NO_REGISTERED_OUTPUT;

    --If the inputs are not registered
    NO_REGISTERED_INPUT: if register_input = 0 generate
        icnt1 <= icnt1_i;
        icnt2 <= icnt2_i;
    end generate NO_REGISTERED_INPUT;

    -- If there is only 1 register at the outputs
    REGISTERED_OUTPUT_1: if register_output = 1 generate
        n_stall1(0) <= stall1;
        n_stall2(0) <= stall2;
        process(clk)
        begin
            if rising_edge(clk) then
                if rstn = '0' then
                    r_stall1(0)  <= '0';                     
                    r_stall2(0)  <= '0';                     
       	        else
                    r_stall1 <= n_stall1;  
                    r_stall2 <= n_stall2;  
                end if;
            end if;   
        end process;
        stall1_o <= r_stall1(0) and global_enable;
        stall2_o <= r_stall2(0) and global_enable;
    end generate REGISTERED_OUTPUT_1;

    -- If there is only 1 register at the inputs
    REGISTERED_INPUT_1: if register_input = 1 generate
        n_icnt1 <= icnt1_i;
        n_icnt2 <= icnt2_i;
        process(clk)
        begin
            if rising_edge(clk) then
                if rstn = '0' then
                    r_icnt1 <= "00";                     
                    r_icnt2 <= "00";                     
       	        else
                    r_icnt1 <= n_icnt1;  
                    r_icnt2 <= n_icnt2;  
                end if;
            end if;   
        end process;
        icnt1 <= r_icnt1;
        icnt2 <= r_icnt2;
    end generate REGISTERED_INPUT_1;

    -- If there are more than 1 register at the outputs
    REGISTERED_OUTPUT_MULTIPLE: if register_output > 1 generate
        n_stall1 <= r_stall1(register_output-2 downto 0) & stall1;
        n_stall2 <= r_stall2(register_output-2 downto 0) & stall2;
        process(clk)
        begin
            if rising_edge(clk) then
                if rstn = '0' then
                    r_stall1  <= (others => '0');                     
                    r_stall2  <= (others => '0');                     
       	        else
                    r_stall1 <= n_stall1;  
                    r_stall2 <= n_stall2;  
                end if;
            end if;   
        end process;
        stall1_o <= r_stall1(register_output-1) and global_enable;
        stall2_o <= r_stall2(register_output-1) and global_enable;
    end generate REGISTERED_OUTPUT_MULTIPLE;

    -- If there are more than 1 register at the inputs
    REGISTERED_INPUT_MULTIPLE: if register_input > 1 generate
        n_icnt1 <= r_icnt1((register_input-1)*lanes_number-1 downto 0) & icnt1_i;
        n_icnt2 <= r_icnt2((register_input-1)*lanes_number-1 downto 0) & icnt2_i;
        process(clk)
        begin
            if rising_edge(clk) then
                if rstn = '0' then
                    r_icnt1  <= (others => '0');                     
                    r_icnt2  <= (others => '0');                     
       	        else
                    r_icnt1 <= n_icnt1;  
                    r_icnt2 <= n_icnt2;  
                end if;
            end if;   
        end process;
        icnt1 <= r_icnt1(register_input*lanes_number-1 downto (register_input-1)*lanes_number);
        icnt2 <= r_icnt2(register_input*lanes_number-1 downto (register_input-1)*lanes_number);
    end generate REGISTERED_INPUT_MULTIPLE;

        
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
        -- if softreset bit is set, data from staggering handler and reset bit are set to 0
            rin <= (others => (others => '0'));
            -- Register containing minimum instructions difference should be reset to a high value
            rin(12) <= (others => '1');
            rin(0) <= v(0);
            rin(0)(31) <= '0';
        else
            -- change registers with data from staggering handler
            rin(REGISTERS_NUMBER-1 downto 3)  <= regs_handler_o;
            -- configuration register shouldn't be changed by the staggering handler
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
        assert (error_from_sh = '0') report "Result: one core got hung: core1" severity warning;
        -- Show the error just once 
        if error_from_sh = '1' then
            wait;
        end if;
    end process;
    -- pragma translate_on
    

end;
