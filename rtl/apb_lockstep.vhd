library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.lockstep_pkg.all;


entity apb_lockstep is
    generic (
        -- genercis
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
        apbi_psel_i    : in  std_logic;                       
        apbi_paddr_i   : in  std_logic_vector(31 downto 0);                      
        apbi_penable_i : in  std_logic;                     
        apbi_pwrite_i  : in  std_logic;
        apbi_pwdata_i  : in  std_logic_vector(31 downto 0);                   
        apbo_prdata_o  : out std_logic_vector(31 downto 0);                  
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
        icnt1_i        : in  std_logic_vector(1 downto 0);    -- Instruction counter from the first core
        icnt2_i        : in  std_logic_vector(1 downto 0);    -- Instruction counter from the second core
        stall1_o       : out std_logic;                       -- Signal to stall the first core
        stall2_o       : out std_logic;                       -- Signal to stall the second core
        error_o        : out std_logic                        -- Reset the program if the result of both ALUs does not match
    );
end;

architecture rtl of apb_lockstep is

    constant REGISTERS_NUMBER : integer := 14; -- minimum 2
    constant SLV_INDEX_CEIL : integer := integer(ceil(log2(real(REGISTERS_NUMBER))));

    -- registers signals
    signal r, rin     : registers_vector(REGISTERS_NUMBER-1 downto 0) ;
    signal regs_handler_o : registers_vector(REGISTERS_NUMBER-1 downto 3) ;

    -- configuration signals
    signal max_slack : std_logic_vector(9 downto 0);
    signal min_slack : std_logic_vector(9 downto 0);
    signal enable_core1, enable_core2 : std_logic;

    -- error signals
    signal error_from_sh, error_from_comp, r_error : std_logic;

    -- Interconect signals
    signal c1_ahead_c2_from_sh : std_logic;

    -- Soft reset
    signal soft_rstn, rstn_int : std_logic;

begin
    
    ---------------------------------- CONFIG REGISTER --------------------------------

    -- Get enable signals and min and max slack
    enable_core1 <= r(1)(0);
    enable_core2 <= r(2)(0);
    max_slack <= r(0)(30 downto 21) when unsigned(r(0)(30 downto 21)) /= 0 else
                 std_logic_vector(to_unsigned(max_slack_init, 10));
    min_slack <= r(0)(20 downto 11) when unsigned(r(0)(20 downto 11)) /= 0 else
                 std_logic_vector(to_unsigned(min_slack_init, 10));


    ------------------------------- MODULES INSTANTIATION -----------------------------

    rstn_int <= soft_rstn and rstn;

    SLACK: if activate_slack = 1 generate
        slack_handler_inst : slack_handler 
        generic map(
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
            c1_ahead_c2_o  => c1_ahead_c2_from_sh,
            stall1_o       => stall1_o, 
            stall2_o       => stall2_o,
            error_o        => error_from_sh
            );
    end generate SLACK;

    COMP: if activate_comparator = 1 generate
        comparator_inst : comparator
        generic map(
            WRITE_ENTRIES => 16,
            BUS_LENGTH    => BUS_LENGTH
            )
        port map(
            clk            => clk,
            rstn           => rstn_int,
            -- ahb signals ----------------
            ahbmo1_write_i => ahbmo1_write_i,  
            ahbmo2_write_i => ahbmo2_write_i,  
            ahbmo1_wdata_i => ahbmo1_wdata_i,  
            ahbmo2_wdata_i => ahbmo2_wdata_i,  
            ahbmo1_trans_i => ahbmo1_trans_i,  
            ahbmo2_trans_i => ahbmo2_trans_i,  
            ahbmi_rdata_i  => ahbmi_rdata_i,  
            ahbmi_hresp_i  => ahbmi_hresp_i,   
            ahbmi_ready_i  => ahbmi_ready_i,   
            ahb_access_i   => ahb_access_i,
            -------------------------------
            enable_core1_i => enable_core1,
            enable_core2_i => enable_core2,
            c1_ahead_c2_i  => c1_ahead_c2_from_sh,
            error_o        => error_from_comp
            );
    end generate COMP;


    -------------------------------------------- APB REGISTERS ----------------------------------------------------

    --Here the registers take their new values. It can be:
    -- 1.- A change in the configuration register
    -- 2.- Reset if the 31th bit of the configuration register is set to 1
    -- 3.- Just update the counters from the slack handler
    comb : process(rstn, r, apbi_psel_i, apbi_paddr_i, apbi_penable_i, apbi_pwrite_i, apbi_pwdata_i, regs_handler_o, r_error, error_from_comp)
        variable readdata : std_logic_vector(31 downto 0);
        variable v        : registers_vector(REGISTERS_NUMBER-1 downto 0);
        variable slave_index : std_logic_vector(SLV_INDEX_CEIL-1 downto 0);
        variable error_count : unsigned(31 downto 0) := x"00000000";
    begin
        v := r;
        -- select slave
        slave_index := apbi_paddr_i(SLV_INDEX_CEIL+1 downto 2);
        -- read register
        readdata := (others => '0');
        if (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' then
            readdata := r(to_integer(unsigned(slave_index)));
        end if;
        -- write registers (only the first three so far)
        if (apbi_psel_i and apbi_pwrite_i) = '1' and unsigned(slave_index) = 0 then
            v(to_integer(unsigned(slave_index))) := apbi_pwdata_i;
        elsif (apbi_psel_i and apbi_pwrite_i) = '1' and unsigned(slave_index) = 1 then
            v(to_integer(unsigned(slave_index))) := apbi_pwdata_i;
        elsif (apbi_psel_i and apbi_pwrite_i) = '1' and unsigned(slave_index) = 2 then
            v(to_integer(unsigned(slave_index))) := apbi_pwdata_i;
        end if;

        -- update registers
        if rstn = '0' then
        -- if systems reset set all registers to 0
            rin <= (others => (others => '0'));
        elsif v(0)(31) = '1' then
        -- if rst bit is set, data from slack handler and reset bit are set to 0
            rin <= (others => (others => '0'));
            rin(0) <= v(0);
            rin(0)(31) <= '0';
        else
            -- change registers with data from slack handler
            rin(REGISTERS_NUMBER-1 downto 3)  <= regs_handler_o;
            -- configuration register shouldn't be changed by the slack handler
            rin(0) <= v(0);
            rin(1) <= v(1);
            rin(2) <= v(2);
            if v(12) = x"00000000" and error_from_comp = '1' then
                rin(12) <= v(5); 
            else
                rin(12) <= v(12);
            end if;
            if error_from_comp = '1' then
                error_count := unsigned(v(13)) + 1;
                rin(13) <= std_logic_vector(error_count);
            else
                rin(13) <= v(13);
            end if;
        end if;
        apbo_prdata_o <= readdata; -- drive apb read bus
        -- soft reset
        soft_rstn <= not v(0)(31);
    end process;
    

    regs : process(clk)
    begin
        if rising_edge(clk) then r <= rin; end if;
    end process;

    ------------------- ERROR -------------------

    error_o <= error_from_sh or error_from_comp;
    --error_reg : process(clk)
    --begin
    --    if rising_edge(clk) then 
    --        if rst = '0' then
    --            r_error <= '0';
    --        else
    --            r_error <= r_error or error_from_comp;
    --        end if;
    --    end if;
    --end process;

    -- Verification -----------------
    process
    begin
        wait for 1 ns;
        assert (error_from_sh = '0') report "Result: error detected by lockstep" severity failure;
    end process;
    

end;
