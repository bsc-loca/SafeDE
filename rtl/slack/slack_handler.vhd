-- -----------------------------------------------
-- Project Name   : De-RISC
-- File           : slack_handler.vhd
-- Organization   : Barcelona Supercomputing Center
-- Author(s)      : Francisco Bas
-- Email(s)       : francisco.basjalon@bsc.es
-- References     : 
-- -----------------------------------------------
-- Revision History
--  Revision   | Author        | Commit | Description
--  1.0        | Francisco Bas | 000000 | Contribution
-- -----------------------------------------------
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library bsc;
use bsc.lockstep_pkg.all;

entity slack_handler is
    generic(
        en_cycles_limit  : integer := 100;  -- Max time allow from the activation of one to the activation of the other
        REGISTERS_NUMBER : integer := 10    -- Number of registers
        );  
    port(
        clk            : in  std_logic;                      
        rstn           : in  std_logic;
        enable_core1_i : in  std_logic;
        enable_core2_i : in  std_logic;
        icnt1_i        : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the first core
        icnt2_i        : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the second core
        max_slack_i    : in  std_logic_vector(15 downto 0);
        min_slack_i    : in  std_logic_vector(14 downto 0);
        regs_in        : in  registers_vector(REGISTERS_NUMBER-2 downto 3); -- Registers of the module (in)
        regs_out       : out registers_vector(REGISTERS_NUMBER-2 downto 3); -- Registers of the module (out) 
        c1_ahead_c2_o  : out std_logic;                                     -- It is 1 when core1 is ahead of core2 and the other way round
        stall1_o       : out std_logic;                                     -- Signal to stall the first core
        stall2_o       : out std_logic;                                     -- Signal to stall the second core
        error_o        : out std_logic                                      -- Signal to assert an error
        );  
end;

architecture rtl of slack_handler is

    -- Signals to compare executed instructions of both cores
    signal core1_ahead_core2      : std_logic;
    signal instruction_difference : unsigned(15 downto 0);   -- It should be intruction_difference_length-1 but this value can be bigger than the limit

    -- Signals to stall each of the cores
    signal stall1, stall2 : std_logic;

    -- Signal to count the cycles lockstep has been active
    signal n_total_cycles, r_total_cycles: unsigned(31 downto 0);

    -- Signals to calculate the number of executed instructions/2 (each PC is two instruction)
    signal r_executed_inst1, n_executed_inst1 : unsigned(31 downto 0);
    signal r_executed_inst2, n_executed_inst2 : unsigned(31 downto 0);

    -- Signals to implement the FSM to stall a core
    type state_type is (stalled1, stalled2, not_stalled);          -- Define the states 
    signal n_stall_fsm, stall_fsm : state_type;

    -- Signals to count the number of times that each core is stalled
    --signal stall1, stall2, r_stall1, r_stall2, f_stall1, f_stall2 : std_logic;
    signal r_stall1, r_stall2, f_stall1, f_stall2 : std_logic;
    signal n_times_stalled_core1, n_times_stalled_core2, r_times_stalled_core1, r_times_stalled_core2 : unsigned(31 downto 0);

    -- Signals to count the number of cycles that one core has been stalled
    signal n_cycles_stalled_core1, n_cycles_stalled_core2, r_cycles_stalled_core1, r_cycles_stalled_core2 : unsigned(31 downto 0); 

    -- Sum of instruction differece each cycle
    signal r_accumulated_inst_diff, n_accumulated_inst_diff : unsigned(31 downto 0);

    -- max instruction different along the execution
    signal r_max_inst_diff, n_max_inst_diff : unsigned(31 downto 0);
    constant fill_zeros : unsigned(15 downto 0) := to_unsigned(0, 16);

    -- min instruction different along the execution
    signal r_min_inst_diff, n_min_inst_diff : unsigned(31 downto 0);
    signal activate_minimum_inst_comp_r     : std_logic;

    -- error counter signals
    constant ERROR_COUNT_INDEX : integer := integer(ceil(log2(real(en_cycles_limit))));
    signal r_error_count : unsigned(ERROR_COUNT_INDEX-1 downto 0);
    signal en_error_count : std_logic;
    
    --enable signal
    signal enable : std_logic;

begin 

    -- pass forward the value of the registers
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

    ----------------------------------------------------------------------- COMBINATIONAL LOGIC --------------------------------------------------------------------------------------------------

    -- If the leading core is enable, enable is asserted and stalls can ocurr
    enable <= '1' when (core1_ahead_core2 and enable_core1_i) = '1' or (not(core1_ahead_core2) and enable_core2_i) = '1' else
              '0';

    -- It calculates the number of executed instructions for both cores using the insturction counter
    -- intruction counter icnt has a bit per lane (both bits)
    n_executed_inst1 <= r_executed_inst1 + 2 when (icnt1_i(0) and icnt1_i(1)  and enable_core1_i) = '1' else 
                        r_executed_inst1 + 1 when ((icnt1_i(0) or icnt1_i(1)) and enable_core1_i) = '1' else
                        r_executed_inst1;

    n_executed_inst2 <= r_executed_inst2 + 2 when (icnt2_i(0) and icnt2_i(1)  and enable_core2_i) = '1' else 
                        r_executed_inst2 + 1 when ((icnt2_i(0) or icnt2_i(1)) and enable_core2_i) = '1' else
                        r_executed_inst2;


    -- Counts how many times a core has been stalled
    n_times_stalled_core1 <= r_times_stalled_core1 + 1 when (f_stall1 = '1') else
                             r_times_stalled_core1;                          
    n_times_stalled_core2 <= r_times_stalled_core2 + 1 when (f_stall2 = '1') else
                             r_times_stalled_core2;                          

    -- Counts the total of cycles that core1 and core2 have been stalled
    n_cycles_stalled_core1 <= r_cycles_stalled_core1 + 1 when stall1 = '1' else
                              r_cycles_stalled_core1;           
    n_cycles_stalled_core2 <= r_cycles_stalled_core2 + 1 when stall2 = '1' else
                              r_cycles_stalled_core2;           

    -- It calculates the cycles since lockstep has been enable
    n_total_cycles   <= r_total_cycles + 1 when enable = '1' else
                        r_total_cycles;

    -- It calculates the maximum difference of instructions along the execution
    n_max_inst_diff <= (fill_zeros & instruction_difference) when (r_max_inst_diff(15 downto 0) < instruction_difference) and enable = '1' else
                       r_max_inst_diff;

    -- It calculates total difference of instructions along the program
    n_accumulated_inst_diff <= r_accumulated_inst_diff + instruction_difference when (enable_core1_i and enable_core2_i) = '1' else
                               r_accumulated_inst_diff; 



    -- If it is 1 means that core1 is ahead of core 2 and viceversa
    core1_ahead_core2 <= '1' when n_executed_inst1 >= n_executed_inst2 else
                         '0';

    -- The instruction different is calculated 
    instruction_difference <= n_executed_inst1(15 downto 0) - n_executed_inst2(15 downto 0) when core1_ahead_core2 = '1' else
                              n_executed_inst2(15 downto 0) - n_executed_inst1(15 downto 0); 


    -- Minimum instruction difference after activation is calculated
    -- Register indicating that minimum instruction comparison should be performed
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                activate_minimum_inst_comp_r <= '0';
            else
                if (activate_minimum_inst_comp_r = '0') then
                    if (enable_core1_i = '1' and enable_core2_i = '1' and n_stall_fsm = not_stalled) then
                        activate_minimum_inst_comp_r <= '1';
                    end if;
                elsif (enable_core1_i = '0' or enable_core2_i = '0') then
                    activate_minimum_inst_comp_r <= '0';
                end if;
            end if;
        end if;
    end process;

    n_min_inst_diff <= (fill_zeros & instruction_difference) when (r_min_inst_diff(15 downto 0) > instruction_difference) and activate_minimum_inst_comp_r = '1' else
                       r_min_inst_diff;

    ------------------------------------------------------ COUNTER TO ASSERT ERROR SIGNAL --------------------------------------------------------------------------------------------- 
    -- Error signal is asserted if one core enables lockstep and the other doesn't enable the lockstep within a ceratain time

    -- Counter is enalbe as long as only one core is enalbed
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


    ------------------------------------------------------ FSM TO CONTROL SIGNALS STALL1 AND STALL2 --------------------------------------------------------------------------------- 
    -- FSM controls if core1 or core2 should be asserted dependeing on the difference of instructions and the max and min slack configured
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                stall_fsm <= not_stalled;        
                r_stall1  <= '0';                     
                r_stall2  <= '0';                     
       	    else
                stall_fsm <= n_stall_fsm;                  -- FMS state
                r_stall1  <= stall1;                             -- Used to detect the flank 
                r_stall2  <= stall2;                             -- Used to detect the flank 
            end if;
        end if;   
    end process;

    -- Flanks detectors to detect when each core is stalled
    f_stall1 <= stall1 and (not r_stall1);                 -- When core1 is stalled this signal is '1' for one cycle

    f_stall2 <= stall2 and (not r_stall2);                 -- When core2 is stalled this signal is '1' for one cycle
    
    
    process(core1_ahead_core2, stall_fsm, instruction_difference, enable, enable_core1_i, enable_core2_i, max_slack_i, min_slack_i)
    begin
        n_stall_fsm <= stall_fsm;
        stall1 <= '0';
        stall2 <= '0';
        case stall_fsm is
            when not_stalled =>
                if enable = '1' then
                    if instruction_difference > unsigned(max_slack_i) then
                        if core1_ahead_core2 = '1' then
                            if enable_core1_i = '1' then
                                n_stall_fsm <= stalled1;
                                stall1 <= '1';
                            end if;
                        else
                            if enable_core2_i = '1' then
                                n_stall_fsm <= stalled2;
                                stall2 <= '1';
                            end if;
                        end if;
                    end if;
                    if instruction_difference < unsigned(min_slack_i) then
                        if core1_ahead_core2 = '1' then
                            if enable_core2_i = '1' then
                                n_stall_fsm <= stalled2;
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
            when stalled1  =>
                stall1 <= '1';
                if ((instruction_difference < unsigned(min_slack_i) and core1_ahead_core2 = '1') or (instruction_difference > unsigned(max_slack_i) and core1_ahead_core2 = '0')) and enable = '1'  then
                    n_stall_fsm <= stalled2;
                    stall1 <= '0';
                    stall2 <= '1';
                elsif (instruction_difference <= unsigned(max_slack_i) and core1_ahead_core2 = '1') or (instruction_difference >= unsigned(min_slack_i) and core1_ahead_core2 = '0') or enable = '0'  then
                    n_stall_fsm <= not_stalled;
                    stall1 <= '0';
                end if;
            when stalled2  =>
                stall2 <= '1';
                if ((instruction_difference < unsigned(min_slack_i) and core1_ahead_core2 = '0') or (instruction_difference > unsigned(max_slack_i) and core1_ahead_core2 = '1')) and enable = '1' then
                    n_stall_fsm <= stalled1;
                    stall1 <= '1';
                    stall2 <= '0';
                elsif (instruction_difference <= unsigned(max_slack_i) and core1_ahead_core2 = '0') or (instruction_difference >= unsigned(min_slack_i) and core1_ahead_core2 = '1') or enable = '0'  then
                    n_stall_fsm <= not_stalled;
                    stall2 <= '0';
                end if;
        end case;
    end process;


    -- OUTPUTS--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    -- assign stalls to its outputs
    stall1_o <= stall1;
    stall2_o <= stall2;

    c1_ahead_c2_o <= core1_ahead_core2; 

    -- pass back the value of the internal registers
    process(regs_in, n_times_stalled_core1, n_times_stalled_core2, n_cycles_stalled_core1, n_cycles_stalled_core2, n_total_cycles, n_executed_inst1, n_executed_inst2, n_max_inst_diff, n_accumulated_inst_diff, n_min_inst_diff)
    begin
        regs_out(3) <= std_logic_vector(n_total_cycles);
        regs_out(4) <= std_logic_vector(n_executed_inst1(31 downto 0));  -- TODO: maybe use several registers if values are too large
        regs_out(5) <= std_logic_vector(n_executed_inst2(31 downto 0));
        regs_out(6) <= std_logic_vector(n_times_stalled_core1);
        regs_out(7) <= std_logic_vector(n_times_stalled_core2);
        regs_out(8) <= std_logic_vector(n_cycles_stalled_core1);
        regs_out(9) <= std_logic_vector(n_cycles_stalled_core2);
        regs_out(10) <= std_logic_vector(n_max_inst_diff);
        regs_out(11) <= std_logic_vector(n_accumulated_inst_diff);
        regs_out(12) <= std_logic_vector(n_min_inst_diff);
    end process;

end;


