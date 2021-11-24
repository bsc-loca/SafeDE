library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library safety;
use safety.lightlock_pkg.all;


entity apb_lightlock_tb is
    --port ();
end;


architecture rtl of apb_lightlock_tb is

    -- Generics definitions
    constant lanes_number : integer := 2;
    constant register_output     : integer := 0;   -- TODO: adapt it for a value different than 0
    constant register_input      : integer := 0;   -- TODO: adapt it for a value different than 0
    constant en_cycles_limit     : integer := 500; 
    constant min_staggering_init : integer := 20;   

    -- Top component declaration
    component apb_lightlock 
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
    end component;


    -- Component to generate icnt1 and icnt2
    component inst_count_sim 
        port(
            clk                  : in  std_ulogic;
            stall2_i             : in  std_logic;
            stall1_i             : in  std_logic;
            icnt1_o              : out std_logic_vector(1 downto 0);
            icnt2_o              : out std_logic_vector(1 downto 0)
        );
    end component;


    -- Procedure to read in the APB bus
    procedure apb_read(
            constant addr   : in integer;                                 -- Bus address to read from
            constant print  : in string(1 to 30);                         -- Message to print before printing the data read
            signal compare_value : in unsigned(31 downto 0);              -- Value to compare with the data read (if not equal an error is raised
            -- apb signals
            signal apbo_prdata  : in std_logic_vector(31 downto 0);
            signal apbi_psel    : out std_logic;
            signal apbi_penable : out std_logic;
            signal apbi_pwrite  : out std_logic;
            signal apbi_paddr   : out std_logic_vector(31 downto 0);
            signal apbi_pwdata  : out std_logic_vector(31 downto 0);
            signal data : out std_logic_vector(31 downto 0)) is           -- This signal is equal to the read data during once cycle
        begin
            -- First cycle
            apbi_penable <= '0';
            apbi_psel <= '1';
            apbi_paddr <= std_logic_vector(to_unsigned(addr, 32));
            apbi_pwrite <= '0';
            apbi_pwdata <= (others => '0');
            data <= (others => '0');
            wait for 10 ns;
            -- Second cycle
            apbi_penable <= '1';
            data <= apbo_prdata;
            wait for 10 ns;
            -- Third cycle
            report print & integer'image(to_integer(unsigned(apbo_prdata))) & " =? " & integer'image(to_integer(compare_value)) severity note;
            assert compare_value = unsigned(apbo_prdata) report print & "Lightlock register value = " & integer'image(to_integer(unsigned(apbo_prdata))) & " does not match the value of the testbench = " & integer'image(to_integer(compare_value)) severity error;
            apbi_penable <= '0';
            apbi_psel <= '0';
            data <= (others => '0');
            wait for 10 ns;
        end apb_read;
            

    -- Procedure to write in the APB bus
    procedure apb_write(
           constant addr   : in integer;                                      -- Bus address to write in
           constant data   : in std_logic_vector(31 downto 0);                -- Data to write
           -- APB signals
           signal apbo_prdata  : in std_logic_vector(31 downto 0);
           signal apbi_psel    : out std_logic;
           signal apbi_penable : out std_logic;
           signal apbi_pwrite  : out std_logic;
           signal apbi_paddr   : out std_logic_vector(31 downto 0);
           signal apbi_pwdata  : out std_logic_vector(31 downto 0)) is
       begin
           -- First cycle
           apbi_penable <= '0';
           apbi_psel <= '1';
           apbi_paddr <= std_logic_vector(to_unsigned(addr, 32));
           apbi_pwrite <= '1';
           apbi_pwdata <= data;
           wait for 10 ns;
           -- Second cycle
           apbi_penable <= '1';
           wait for 10 ns;
           -- Third cycle
           apbi_pwdata <= (others => '0');
           apbi_penable <= '0';
           apbi_psel <= '0';
           wait for 10 ns;
       end apb_write;
           


    -- Signal definition
    signal rstn            : std_ulogic;
    signal clk             : std_ulogic;
    -- APB signals
    signal apbi_psel       : std_logic;                       
    signal apbi_paddr      : std_logic_vector(31 downto 0);                      
    signal apbi_penable    : std_logic;                     
    signal apbi_pwrite     : std_logic;
    signal apbi_pwdata     : std_logic_vector(31 downto 0);                   
    signal apbo_prdata     : std_logic_vector(31 downto 0);                  
    -- Lockstep signals
    signal icnt1           : std_logic_vector(lanes_number-1 downto 0);    -- Instruction counter from the first core
    signal icnt2           : std_logic_vector(lanes_number-1 downto 0);    -- Instruction counter from the second core
    signal stall1          : std_logic;                                    -- Signal to stall the first core
    signal stall2          : std_logic;                                    -- Signal to stall the second core
    signal error_lightlock : std_logic;                                    -- Reset the program if the result of both ALUs does not match


    -- Local signals
    signal data_read : std_logic_vector(31 downto 0); -- Signal to read the read data in the bus

    signal enable_core1, enable_core2, r_enable_core1, r_enable_core2, enable : std_logic;

    -- Signal for statistics
    signal r_total_cycles, r_executed_inst1, r_executed_inst2, r_times_stalled_core1, r_times_stalled_core2, r_cycles_stalled_core1, r_cycles_stalled_core2, r_max_inst_diff, r_min_inst_diff : unsigned(31 downto 0);
    signal n_total_cycles, n_executed_inst1, n_executed_inst2, n_times_stalled_core1, n_times_stalled_core2, n_cycles_stalled_core1, n_cycles_stalled_core2, n_max_inst_diff, n_min_inst_diff : unsigned(31 downto 0);
    signal core1_ahead_core2 : std_logic;

    signal r_stall1, r_stall2, f_stall1, f_stall2 : std_logic;

    constant fill_zeros : unsigned(16 downto 0) := to_unsigned(0, 17);
    signal instruction_difference : unsigned(14 downto 0);

    signal r_activate_minimum_inst_comp : std_logic;    


begin

    -- Top instantiation
    apb_lightlock_inst : apb_lightlock
    generic map(
        lanes_number        => 2, 
        register_output     => 0,
        register_input      => 0,
        en_cycles_limit     => 200,
        min_staggering_init => 20
        )
    port map(
        rstn  => rstn,         
        clk   => clk,         
        -- APB signals
        apbi_psel_i    =>  apbi_psel,   
        apbi_paddr_i   =>  apbi_paddr,   
        apbi_penable_i =>  apbi_penable,
        apbi_pwrite_i  =>  apbi_pwrite, 
        apbi_pwdata_i  =>  apbi_pwdata, 
        apbo_prdata_o  =>  apbo_prdata, 
        -- lockstep signals
        icnt1_i  => icnt1,      
        icnt2_i  => icnt2,      
        stall1_o => stall1,     
        stall2_o => stall2,     
        error_o  => error_lightlock 
        );


    -------------------------------------------------------------------------------------------------------------------------
    -- CALCULATE STATISTICS TO COMPARE THEM WITH THE VALUES OF THE LOCKSTEP
    -------------------------------------------------------------------------------------------------------------------------
     process(clk)
     begin
         if rising_edge(clk) then
             if rstn = '0' then
                 r_enable_core1 <= '0';
                 r_enable_core2 <= '0';
                 core1_ahead_core2 <= '0';
             else
                 r_enable_core1 <= enable_core1;
                 r_enable_core2 <= enable_core2;
                 if enable_core1 = '1' and r_enable_core1 = '0' and enable_core2 = '0' then
                     core1_ahead_core2 <= '1';
                 elsif enable_core1 = '0' and r_enable_core2 = '0' and enable_core2 = '1' then
                     core1_ahead_core2 <= '0';
                 end if;
             end if;
         end if;
     end process;

     enable <= '1' when (core1_ahead_core2 and enable_core1) = '1' or (not(core1_ahead_core2) and enable_core2) = '1' else
               '0';

     -- It calculates the cycles since lockstep has been enable (starting when one core is in the critical section)
     n_total_cycles   <= r_total_cycles + 1 when enable = '1' else
                         r_total_cycles;

     -- It calculates the number of executed instructions for both cores using the insturction counter of each core
     -- intruction counter icnt has a bit per lane (both bits)
     process(icnt1, icnt2, r_executed_inst1, r_executed_inst2, enable_core1, enable_core2)
         variable executed_inst1_temp : unsigned(n_executed_inst1'RANGE);
         variable executed_inst2_temp : unsigned(n_executed_inst2'RANGE);
     begin
         executed_inst1_temp := (others => '0');
         executed_inst2_temp := (others => '0');
         for n in 0 to lanes_number-1 loop
             executed_inst1_temp := executed_inst1_temp + unsigned(icnt1(n downto n));
             executed_inst2_temp := executed_inst2_temp + unsigned(icnt2(n downto n));
         end loop;
         if enable_core1 = '1' then
             n_executed_inst1 <= r_executed_inst1 + executed_inst1_temp;
         else
             n_executed_inst1 <= r_executed_inst1;
         end if;
         if enable_core2 = '1' then
             n_executed_inst2 <= r_executed_inst2 + executed_inst2_temp;
         else
             n_executed_inst2 <= r_executed_inst2;
         end if;
     end process;


     -- Counts how many times each core has been stalled
     process(clk)
     begin
         if rising_edge(clk) then
             if rstn = '0' then
                 r_stall1 <= '0';
                 r_stall2 <= '0';
             else
                 r_stall1 <= stall1;
                 r_stall2 <= stall2;
            end if;
        end if;
    end process;

     f_stall1 <= stall1 and (not r_stall1);  -- When core1 is stalled this signal is '1' for one cycle
     f_stall2 <= stall2 and (not r_stall2);  -- When core2 is stalled this signal is '1' for one cycle

     n_times_stalled_core1 <= r_times_stalled_core1 + 1 when (f_stall1 = '1') else
                              r_times_stalled_core1;
     n_times_stalled_core2 <= r_times_stalled_core2 + 1 when (f_stall2 = '1') else
                              r_times_stalled_core2;


     -- Counts the total of cycles that core1 and core2 have been stalled
     n_cycles_stalled_core1 <= r_cycles_stalled_core1 + 1 when stall1 = '1' else
                               r_cycles_stalled_core1;
     n_cycles_stalled_core2 <= r_cycles_stalled_core2 + 1 when stall2 = '1' else
                               r_cycles_stalled_core2;


    -- It calculates the maximum difference of instructions along the execution starting when both cores are in the critical section
    instruction_difference <= n_executed_inst1(14 downto 0) - n_executed_inst2(14 downto 0) when core1_ahead_core2 = '1' else
                              n_executed_inst2(14 downto 0) - n_executed_inst1(14 downto 0);

    n_max_inst_diff <= (fill_zeros & instruction_difference) when (r_max_inst_diff(14 downto 0) < instruction_difference) and enable = '1' else
                       r_max_inst_diff;

    -- It calculates the minimum difference of instructions along the execution (once the staggering is acomplished)
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                r_activate_minimum_inst_comp <= '0';
            else
                if (r_activate_minimum_inst_comp = '0') then
                    if (enable_core1 = '1' and enable_core2 = '1' and stall1 = '0' and stall2 = '0') then
                        r_activate_minimum_inst_comp <= '1';
                    end if;
                elsif (enable_core1 = '0' or enable_core2 = '0') then
                    r_activate_minimum_inst_comp <= '0';
                end if;
            end if;
        end if;
    end process;
 
     n_min_inst_diff <= (fill_zeros & instruction_difference) when (r_min_inst_diff(14 downto 0) > instruction_difference) and r_activate_minimum_inst_comp = '1' else
                        r_min_inst_diff;


     process(clk)
     begin
         if rising_edge(clk) then
             if rstn = '0' then
                 r_total_cycles          <= (others => '0');
                 r_executed_inst1        <= (others => '0');
                 r_executed_inst2        <= (others => '0');
                 r_times_stalled_core1   <= (others => '0');
                 r_times_stalled_core2   <= (others => '0');
                 r_cycles_stalled_core1  <= (others => '0');
                 r_cycles_stalled_core2  <= (others => '0');
                 r_max_inst_diff         <= (others => '0');
                 r_min_inst_diff         <= (others => '1');
             else
                 r_total_cycles          <= n_total_cycles;        
                 r_executed_inst1        <= n_executed_inst1;      
                 r_executed_inst2        <= n_executed_inst2;      
                 r_times_stalled_core1   <= n_times_stalled_core1;
                 r_times_stalled_core2   <= n_times_stalled_core2; 
                 r_cycles_stalled_core1  <= n_cycles_stalled_core1;
                 r_cycles_stalled_core2  <= n_cycles_stalled_core2;
                 r_max_inst_diff         <= n_max_inst_diff;      
                 r_min_inst_diff         <= n_min_inst_diff;      
             end if;
         end if;
     end process;
    -------------------------------------------------------------------------------------------------------------------------
    -------------------------------------------------------------------------------------------------------------------------
    -------------------------------------------------------------------------------------------------------------------------

    -- Clock generation
    process
    begin
        clk <= '1';
        wait for 5 ns;
        clk <= '0';
        wait for 5 ns;
    end process;

    -- icnt generation
    -- It generates them randomly
    -- TODO: adapt it for more lanes
    icnt_generation : inst_count_sim 
    port map(
        clk      => clk,         
        stall2_i => stall2,  
        stall1_i => stall1,
        icnt1_o  => icnt1,
        icnt2_o  => icnt2
        );

    -- Main process
    process is
        variable write_register : std_logic_vector(31 downto 0);
    begin
        enable_core1 <= '0';
        enable_core2 <= '0';

        -- Configure inputs to reset value
        apbi_psel     <= '0';   
        apbi_paddr    <= (others => '0');  
        apbi_penable  <= '0';   
        apbi_pwrite   <= '0';
        apbi_pwdata   <= (others => '0');
        data_read     <= (others => '0');

        --Reset
        rstn <= '0';

        -- Keep reset for 10 cycles
        wait for 100 ns;
        rstn <= '1';

        -- CONFIGURE LIGHTLOCK INTERNAL REGISTERS ------------------------------------------------------------------
        -- apb_write(addr, write_data, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata)
        -- Soft reset
        write_register := x"80000000";
        apb_write(0, write_register, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata);
        wait for 10 ns;

        -- Global enable and configure min and max staggering (10,20)
        write_register := x"400a000a";
        apb_write(0, write_register, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata);
        wait for 10 ns;

        -- Start critical section core1
        enable_core1 <= '1' after 11 ns;
        write_register := x"00000001";
        apb_write(1*4, write_register, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata);
        wait for 80 ns;

        -- Start critical section core2
        enable_core2 <= '1' after 11 ns;
        write_register := x"00000001";
        apb_write(2*4, write_register, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata);
        wait for 10 ns;
        ------------------------------------------------------------------------------------------------------------

        -- SET A DURATION FOR THE TEST --------------------------------------
        wait for 20000 ns;
        ---------------------------------------------------------------------

        -- STOP CRITICAL SECTIONS ----------------------------------------------------------------------------------
        -- Stop critical section core1
        enable_core1 <= '0' after 11 ns;
        write_register := x"00000000";
        apb_write(1*4, write_register, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata);
        wait for 80 ns;

        -- Stop critical section core2
        enable_core2 <= '0' after 11 ns;
        write_register := x"00000000";
        apb_write(2*4, write_register, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata);
        wait for 10 ns;
        ------------------------------------------------------------------------------------------------------------

        -- GATHER RESULTS ------------------------------------------------------------------------------------------
        -- apb_read(addr, write_data, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata)
        apb_read(3*4, "Total cycles:                 ", r_total_cycles, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(4*4, "Executed instructions core1:  ", r_executed_inst1, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(5*4, "Executed instructions core2:  ", r_executed_inst2, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(6*4, "Times core1 has been stalled: ", r_times_stalled_core1, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(7*4, "Times core2 has been stalled: ", r_times_stalled_core2, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(8*4, "Cycles core1 has been stalled:", r_cycles_stalled_core1, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(9*4, "Cycles core2 has been stalled:", r_cycles_stalled_core2, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(10*4, "Max instructions difference:  ", r_max_inst_diff, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(12*4, "Min instructions difference:  ", r_min_inst_diff, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;
        ------------------------------------------------------------------------------------------------------------

        report "Test finished";
        -- report "Test finished" severity error;
        -- assert 1 = 2  report "Test finished." severity failure;
        wait;
    end process;

        


end; 
