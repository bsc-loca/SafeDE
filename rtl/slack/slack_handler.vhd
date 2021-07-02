library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library bsc;
use bsc.lockstep_pkg.all;

entity slack_handler is
    generic(
        register_output  : integer := 0;
        lanes_number     : integer := 2;
        en_cycles_limit  : integer := 100;  -- Max time allow from the activation of one to the activation of the other
        REGISTERS_NUMBER : integer := 10    -- Number of registers
        );  
    port(
        clk            : in  std_logic;                      
        rstn           : in  std_logic;
        enable_core1_i : in  std_logic;
        enable_core2_i : in  std_logic;
        icnt1_i        : in  std_logic_vector(lanes_number-1 downto 0);     -- Instruction counter from the first core
        icnt2_i        : in  std_logic_vector(lanes_number-1 downto 0);     -- Instruction counter from the second core
        max_slack_i    : in  std_logic_vector(14 downto 0);
        min_slack_i    : in  std_logic_vector(14 downto 0);
        regs_in        : in  registers_vector(REGISTERS_NUMBER-1 downto 3); -- Registers of the module (in)
        regs_out       : out registers_vector(REGISTERS_NUMBER-1 downto 3); -- Registers of the module (out) 
        stall1_o       : out std_logic;                                     -- Signal to stall the first core
        stall2_o       : out std_logic;                                     -- Signal to stall the second core
        error_o        : out std_logic                                      -- Signal to assert an error
        );  
end;

architecture rtl of slack_handler is

    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- SIGNALS ---------------------------------------------------------------------------------------------------------------------------------------------
    --------------------------------------------------------------------------------------------------------------------------------------------------------

    -- LOCKSTEP SIGNALS -------------------------------------------------------------------------------
    -- Signals to implement the register to calculate the number of executed instructions
    signal r_executed_inst1, n_executed_inst1 : unsigned(31 downto 0);
    signal r_executed_inst2, n_executed_inst2 : unsigned(31 downto 0);

    -- Signals to compare executed instructions of both cores
    signal core1_ahead_core2      : std_logic;
    signal r_enable_core1, r_enable_core2 : std_logic;
    signal instruction_difference : unsigned(14 downto 0);  

    -- Signals to stall each of the cores
    signal stall1, stall2 : std_logic;

    -- Signals to implement the FSM to decide when stall a core
    type state_type is (stalled1, stalled2, not_stalled); -- Define the states 
    signal n_stall_fsm, stall_fsm : state_type;

    --enable signal
    signal enable : std_logic;

    -- error counter signals
    constant ERROR_COUNT_INDEX : integer := integer(ceil(log2(real(en_cycles_limit))));
    signal r_error_count : unsigned(ERROR_COUNT_INDEX-1 downto 0);
    signal en_error_count : std_logic;
    ---------------------------------------------------------------------------------------------------


    -- STATISTICS SIGNALS -----------------------------------------------------------------------------
    -- Signal to count the cycles lockstep has been active
    signal n_total_cycles, r_total_cycles: unsigned(31 downto 0);

    -- Signals to count the number of times that each core is stalled
    -- A flank detector has to be implemented for that purpose
    signal r_stall1, r_stall2, f_stall1, f_stall2 : std_logic;
    signal n_times_stalled_core1, n_times_stalled_core2, r_times_stalled_core1, r_times_stalled_core2 : unsigned(31 downto 0);

    -- Signals to implement the register to count the number of cycles that one core has been stalled
    signal n_cycles_stalled_core1, n_cycles_stalled_core2, r_cycles_stalled_core1, r_cycles_stalled_core2 : unsigned(31 downto 0); 

    -- Signals to implement the register to store the summation of all the instruction differences
    -- Average difference between cores can be obtained by dividing for the total cycles active
    signal r_accumulated_inst_diff, n_accumulated_inst_diff : unsigned(31 downto 0);

    -- Signals to implement the register to store max instruction different along the execution
    signal r_max_inst_diff, n_max_inst_diff : unsigned(31 downto 0);
    constant fill_zeros : unsigned(16 downto 0) := to_unsigned(0, 17);

    -- Signals to implement the register to store min instruction different along the execution
    signal r_min_inst_diff, n_min_inst_diff : unsigned(31 downto 0);
    signal r_activate_minimum_inst_comp     : std_logic;
    ---------------------------------------------------------------------------------------------------
    

begin 
    
    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- STATISTICS ------------------------------------------------------------------------------------------------------------------------------------------
    --------------------------------------------------------------------------------------------------------------------------------------------------------

    -- We take the current values of the statistics registers from the apb interface
    process(regs_in)
    begin
          r_total_cycles          <= unsigned(regs_in(3)); 
          r_executed_inst1        <= unsigned(regs_in(4));
          r_executed_inst2        <= unsigned(regs_in(5));
          r_times_stalled_core1   <= unsigned(regs_in(6));
          r_times_stalled_core2   <= unsigned(regs_in(7));
          r_cycles_stalled_core1  <= unsigned(regs_in(8));
          r_cycles_stalled_core2  <= unsigned(regs_in(9));
          r_max_inst_diff         <= unsigned(regs_in(10));
          r_accumulated_inst_diff <= unsigned(regs_in(11));
          r_min_inst_diff         <= unsigned(regs_in(12));
    end process;

    -- Counts how many times each core has been stalled
    n_times_stalled_core1 <= r_times_stalled_core1 + 1 when (f_stall1 = '1') else
                             r_times_stalled_core1;                          
    n_times_stalled_core2 <= r_times_stalled_core2 + 1 when (f_stall2 = '1') else
                             r_times_stalled_core2;                          

    -- Counts the total of cycles that core1 and core2 have been stalled
    n_cycles_stalled_core1 <= r_cycles_stalled_core1 + 1 when stall1 = '1' else
                              r_cycles_stalled_core1;           
    n_cycles_stalled_core2 <= r_cycles_stalled_core2 + 1 when stall2 = '1' else
                              r_cycles_stalled_core2;           

    -- It calculates the cycles since lockstep has been enable (starting when one core is in the critical section)
    n_total_cycles   <= r_total_cycles + 1 when enable = '1' else
                        r_total_cycles;

    -- It calculates the maximum difference of instructions along the execution starting when both cores are in the critical section
    n_max_inst_diff <= (fill_zeros & instruction_difference) when (r_max_inst_diff(14 downto 0) < instruction_difference) and enable = '1' else
                       r_max_inst_diff;

    -- It adds it cycle the instruction difference between cores. This way, the mean can be computed dividing by the total cycles.
    n_accumulated_inst_diff <= r_accumulated_inst_diff + instruction_difference when enable = '1' else
                               r_accumulated_inst_diff; 


    -- This code calculates the minimum instruction difference along the execution:
    -- To compute the minimum instruciton difference between cores, we have to start storing the smallest value once both cores have 
    -- entered the critical seciton and after the instruction difference between both is bigger than the minimum threshold (neither of 
    -- them is stalled).
    -- To acomplish that, a register that let the minimum difference instruction value upload is instanciated.
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                r_activate_minimum_inst_comp <= '0';
            else
                if (r_activate_minimum_inst_comp = '0') then
                    if (enable_core1_i = '1' and enable_core2_i = '1' and n_stall_fsm = not_stalled) then
                        r_activate_minimum_inst_comp <= '1';
                    end if;
                elsif (enable_core1_i = '0' or enable_core2_i = '0') then
                    r_activate_minimum_inst_comp <= '0';
                end if;
            end if;
        end if;
    end process;

    n_min_inst_diff <= (fill_zeros & instruction_difference) when (r_min_inst_diff(14 downto 0) > instruction_difference) and r_activate_minimum_inst_comp = '1' else
                       r_min_inst_diff;
    --------------------------------------------------------------------------------------------------------------------------------------------------------


    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- LOCKSTEP ----------------------------------------------------------------------------------------------------------------------------------------
    --------------------------------------------------------------------------------------------------------------------------------------------------------

    -- If the leading core is enable, enable is set to 1 and stalls can occur
    -- This way, when the leading core finishes the critical section and the trailing core exceeds the lower threshold, the trailing core is not stalled.
    -- Also, if the leading core enters the critical section and the trailing core is not active yet, if the max threshold is reach, core1 can be stalled
    enable <= '1' when (core1_ahead_core2 and enable_core1_i) = '1' or (not(core1_ahead_core2) and enable_core2_i) = '1' else
              '0';

    -- It calculates the number of executed instructions for both cores using the insturction counter of each core
    -- intruction counter icnt has a bit per lane (both bits)

    process(icnt1_i, icnt2_i, r_executed_inst1, r_executed_inst2, enable_core1_i, enable_core2_i)
        variable executed_inst1_temp : unsigned(n_executed_inst1'RANGE);
        variable executed_inst2_temp : unsigned(n_executed_inst2'RANGE);
    begin
        executed_inst1_temp := (others => '0');
        executed_inst2_temp := (others => '0');
        for n in 0 to lanes_number-1 loop
            executed_inst1_temp := executed_inst1_temp + unsigned(icnt1_i(n downto n));
            executed_inst2_temp := executed_inst2_temp + unsigned(icnt2_i(n downto n));
        end loop;
        if enable_core1_i = '1' then
            n_executed_inst1 <= r_executed_inst1 + executed_inst1_temp;
        else
            n_executed_inst1 <= r_executed_inst1;
        end if;
        if enable_core2_i = '1' then
            n_executed_inst2 <= r_executed_inst2 + executed_inst2_temp;
        else
            n_executed_inst2 <= r_executed_inst2;
        end if;
    end process;


    -- The first core that reaches the critical section is the core ahead
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                r_enable_core1 <= '0'; 
                r_enable_core2 <= '0'; 
            else 
                r_enable_core1 <= enable_core1_i;
                r_enable_core2 <= enable_core2_i;
            end if;
            if enable_core1_i = '1' and r_enable_core1 = '0' and enable_core2_i = '0' then
                core1_ahead_core2 <= '1';
            elsif enable_core1_i = '0' and r_enable_core2 = '0' and enable_core2_i = '1' then
                core1_ahead_core2 <= '0';
            end if;
        end if;
    end process;


    -- The instruction difference is calculated 
    -- Since this signals are unsigned, the value of instruction_difference will be correct even if overflow is produced
    instruction_difference <= n_executed_inst1(14 downto 0) - n_executed_inst2(14 downto 0) when core1_ahead_core2 = '1' else
                              n_executed_inst2(14 downto 0) - n_executed_inst1(14 downto 0); 

    -- REGISTERS
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                stall_fsm <= not_stalled;        
                r_stall1  <= '0';                     
                r_stall2  <= '0';                     
       	    else
                stall_fsm <= n_stall_fsm;   -- FMS state
                r_stall1  <= stall1;        -- Register used to detect the flank 
                r_stall2  <= stall2;        -- Register used to detect the flank 
            end if;
        end if;   
    end process;

    -- Flanks detectors to count how many times each core is stalled (statistics)
    f_stall1 <= stall1 and (not r_stall1);  -- When core1 is stalled this signal is '1' for one cycle
    f_stall2 <= stall2 and (not r_stall2);  -- When core2 is stalled this signal is '1' for one cycle
    
    
    -- FSM to stall the cores when upper threshold or lower threshold is exceeded
    process(core1_ahead_core2, stall_fsm, instruction_difference, enable, enable_core1_i, enable_core2_i, max_slack_i, min_slack_i)
    begin
        n_stall_fsm <= stall_fsm;
        stall1 <= '0';
        stall2 <= '0';
        case stall_fsm is
            when not_stalled =>  -- No core is stalled, check if any core has to be stalled
                if enable = '1' then  
                    if instruction_difference >= unsigned(max_slack_i) then
                        if core1_ahead_core2 = '1' then
                            n_stall_fsm <= stalled1;
                            stall1 <= '1';
                        else
                            n_stall_fsm <= stalled2;
                            stall2 <= '1';
                        end if;
                    elsif instruction_difference <= unsigned(min_slack_i) then
                        if core1_ahead_core2 = '1' then
                            if enable_core2_i = '1' then  -- It has to be checked that the trailing core is active since the enable signal is active when the trail core is active
                                n_stall_fsm <= stalled2;  -- Could happen that the enable signal is active but the trailing core has not enter the critical section yet.
                                stall2 <= '1';
                            end if;
                        else
                            if enable_core1_i = '1' then
                                n_stall_fsm <= stalled1;
                                stall1 <= '1';
                            end if;
                        end if;
                    end if;
                end if;
            when stalled1  => -- Core1 is stalled, it asserts the stall signal and check wether it has to be "unstalled" or not
                stall1 <= '1';
                --if ((instruction_difference < unsigned(min_slack_i) and core1_ahead_core2 = '1') or (instruction_difference > unsigned(max_slack_i) and core1_ahead_core2 = '0')) and enable = '1'  then
                --    n_stall_fsm <= stalled2;
                --    stall1 <= '0';
                --    stall2 <= '1';
                if (instruction_difference < unsigned(max_slack_i) and core1_ahead_core2 = '1') or (instruction_difference > unsigned(min_slack_i) and core1_ahead_core2 = '0') or enable = '0'  then
                    n_stall_fsm <= not_stalled;
                    stall1 <= '0';
                end if;
            when stalled2  => -- Core2 is stalled, it asserts the stall signal and check wether it has to be "unstalled" or not
                stall2 <= '1';
                --if ((instruction_difference < unsigned(min_slack_i) and core1_ahead_core2 = '0') or (instruction_difference > unsigned(max_slack_i) and core1_ahead_core2 = '1')) and enable = '1' then
                --    n_stall_fsm <= stalled1;
                --    stall1 <= '1';
                --    stall2 <= '0';
                if (instruction_difference < unsigned(max_slack_i) and core1_ahead_core2 = '0') or (instruction_difference > unsigned(min_slack_i) and core1_ahead_core2 = '1') or enable = '0'  then
                    n_stall_fsm <= not_stalled;
                    stall2 <= '0';
                end if;
        end case;
    end process;


    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- ERROR DETECTION -------------------------------------------------------------------------------------------------------------------------------------
    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- Error signal is asserted if one core enables lockstep and the other doesn't enable the lockstep within a ceratain number of cycles

    -- TODO: needed?
    -- Counter is enalbe as long as only one core is enabled
    en_error_count <= enable_core1_i xor enable_core2_i;
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0'or en_error_count = '0' then
                r_error_count <= (others => '0');        
       	    else
                r_error_count <= r_error_count + 1;
            end if;
        end if;   
    end process;

    error_o <= '1' when r_error_count > en_cycles_limit else
               '0';
    --------------------------------------------------------------------------------------------------------------------------------------------------------


    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- OUTPUTS ---------------------------------------------------------------------------------------------------------------------------------------------
    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- Assign stalls to its outputs
    stall1_o <= stall1;
    stall2_o <= stall2;

    -- Pass back the value of the internal registers to the apb interface
    process(regs_in, n_times_stalled_core1, n_times_stalled_core2, n_cycles_stalled_core1, n_cycles_stalled_core2, n_total_cycles, n_executed_inst1, n_executed_inst2, n_max_inst_diff, n_accumulated_inst_diff, n_min_inst_diff)
    begin
        regs_out(3) <= std_logic_vector(n_total_cycles);
        regs_out(4) <= std_logic_vector(n_executed_inst1(31 downto 0));  
        regs_out(5) <= std_logic_vector(n_executed_inst2(31 downto 0));
        regs_out(6) <= std_logic_vector(n_times_stalled_core1);
        regs_out(7) <= std_logic_vector(n_times_stalled_core2);
        regs_out(8) <= std_logic_vector(n_cycles_stalled_core1);
        regs_out(9) <= std_logic_vector(n_cycles_stalled_core2);
        regs_out(10) <= std_logic_vector(n_max_inst_diff);
        regs_out(11) <= std_logic_vector(n_accumulated_inst_diff);
        regs_out(12) <= std_logic_vector(n_min_inst_diff);
    end process;
    --------------------------------------------------------------------------------------------------------------------------------------------------------

    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- ASSERTS ---------------------------------------------------------------------------------------------------------------------------------------------
    --------------------------------------------------------------------------------------------------------------------------------------------------------
    -- It checks that staggering is kept always between both thresholds
    assert (instruction_difference <= unsigned(max_slack_i) + lanes_number + lanes_number*register_output - 1) or r_activate_minimum_inst_comp = '0'  report "Maximum threshold condition has been violated" severity error;
    assert (instruction_difference >= unsigned(min_slack_i) - lanes_number - lanes_number*register_output + 1) or r_activate_minimum_inst_comp = '0' report "Minimum threshold condition has been violated" severity error;
    --------------------------------------------------------------------------------------------------------------------------------------------------------
end;


