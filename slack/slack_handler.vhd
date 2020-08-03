-- -----------------------------------------------
-- Project Name   : De-RISC
-- File           : comparator.vhd
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

entity slack_handler is
  generic(
    min_slack  : integer := 1000;  -- Number of cycles that the core is going to be stalled
    max_slack  : integer := 1000   -- When one core is 'max_instructions_differece" instrucctions ahead of the other, it is stalled.
    );  
  port(
    clk            : in  std_logic;                      
    rstn           : in  std_logic;
    enable         : in  std_logic;                       -- Enables the module
    icnt1          : in  std_logic_vector(1 downto 0);    -- Instruction counter from the first core
    icnt2          : in  std_logic_vector(1 downto 0);    -- Instruction counter from the second core
    stall1         : out std_logic;                       -- Signal to stall the first core
    stall2         : out std_logic                        -- Signal to stall the second core
    );  
end;

architecture rtl of slack_handler is
    -- Signals to calculate the number of executed instructions/2 (each PC is two instructions)
    signal executed_inst1, n_executed_inst1 : unsigned(128 downto 0);
    signal executed_inst2, n_executed_inst2 : unsigned(128 downto 0);
    signal next_icnt1, next_icnt2           : std_logic_vector(1 downto 0);
    signal increment1, increment2           : unsigned (1 downto 0);

    -- Signals to compare executed instructions of both cores
    signal   core1_ahead_core2             : std_logic;
    constant instruction_difference_length : integer := integer(ceil(log2(real(max_slack))));
    signal   instructions_difference       : unsigned(instruction_difference_length downto 0);   -- It should be intruction_difference_length-1 but this value can be bigger than the limit


    -- Signals to implement the FSM to stall a core
    type state_type is (stalled1, stalled2, not_stalled);          -- Define the states 
    signal next_stall_state, stall_state : state_type;

    -- Signals to count the number of times that one core is stalled
    signal start_stall, start_stall_reg, f_start_stall : std_logic;
    signal counter_times_stalled : unsigned(31 downto 0);

    -- Signals to count the number of cycles that one core has been stalled
    signal cycles_stalled : unsigned(127 downto 0); 

begin 

    -- Calculate the number o executed instructions for both cores using the insturction counter
    -- intruction counter icnt has a bit per lane (both bits)
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
	        executed_inst1 <= (others => '0');
	        executed_inst2 <= (others => '0');
               	next_icnt1     <= "00";
	        next_icnt2     <= "00";
       	    else
                next_icnt1  <= icnt1; 
                next_icnt2  <= icnt2; 
                executed_inst1 <= n_executed_inst1;
                executed_inst2 <= n_executed_inst2;
            end if;
        end if;   
    end process;

    -- If both bits are equal instruction counter does not increment if both are different it increments
    -- by two, if only one is different it increments by one.
    increment1 <= to_unsigned(0, 2) when icnt1 = next_icnt1 else
                  to_unsigned(2, 2) when icnt1(0) /= next_icnt1(0) and icnt1(1) /= next_icnt1(1) else
                  to_unsigned(1, 2);

    increment2 <= to_unsigned(0, 2) when icnt2 = next_icnt2 else
                  to_unsigned(2, 2) when icnt2(0) /= next_icnt2(0) and icnt2(1) /= next_icnt2(1) else
                  to_unsigned(1, 2);

    n_executed_inst1 <= executed_inst1 + increment1 when enable = '1' else
                        (others => '0');

    n_executed_inst2 <= executed_inst2 + increment2 when enable = '1' else
                        (others => '0');


    -- It compares the instructions and if the difference is bigger than 'instructions_difference' then
    -- the core with more instructions is stalled

    core1_ahead_core2 <= '1' when executed_inst1 >= executed_inst2 else
                          '0';

    instructions_difference <= executed_inst1(instruction_difference_length downto 0) - executed_inst2(instruction_difference_length downto 0) when core1_ahead_core2 = '1' else
                               executed_inst2(instruction_difference_length downto 0) - executed_inst1(instruction_difference_length downto 0); 



    -- finite state machine to stall the core in the lead during 'stalled_cycles' cycles
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                stall_state    <= not_stalled;        
                cycles_stalled <= (others => '0');          
                start_stall_reg <= '0';                     
                counter_times_stalled <= (others => '0'); 
       	    else
                stall_state  <= next_stall_state;                           -- FMS state
                start_stall_reg <= start_stall;                             -- Used to detect the flank 
                if stall_state /= not_stalled then
                    cycles_stalled <= cycles_stalled + 1;                   -- Counts the total of cycles that core1 and core2 have been stalled
                end if;
                if f_start_stall = '1' then
                    counter_times_stalled <= counter_times_stalled + 1;     -- Counts how many times a core has been stalled
                end if;
            end if;
        end if;   
    end process;

    start_stall <= '1' when stall_state /= not_stalled else
                   '0';
    f_start_stall <= start_stall and (not start_stall_reg);                 -- When any core is stalled this signal is '1' for one cycle

    
    process(core1_ahead_core2, stall_state, instructions_difference, enable)
    begin
        next_stall_state <= stall_state;
        stall1 <= '0';
        stall2 <= '0';
        case stall_state is
            when not_stalled =>
                if enable = '1' then
                    if instructions_difference >= max_slack then
                        if core1_ahead_core2 = '1' then
                            next_stall_state <= stalled1;
                        else
                            next_stall_state <= stalled2;
                        end if;
                    end if;
                    if instructions_difference <= min_slack then
                        if core1_ahead_core2 = '1' then
                            next_stall_state <= stalled2;
                        else
                            next_stall_state <= stalled1;
                        end if;
                    end if;
                end if;
            when stalled1  =>
                stall1 <= '1';
                if (instructions_difference < max_slack and instructions_difference > min_slack) or enable = '0'  then
                    next_stall_state <= not_stalled;
                end if;
            when stalled2  =>
                stall2 <= '1';
                if (instructions_difference < max_slack and instructions_difference > min_slack) or enable = '0'  then
                    next_stall_state <= not_stalled;
                end if;
        end case;
    end process;
    

end;

-------------------------------------------------------------------------------------------------------------------
----------------------------------- old implementation ------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------
	    	
--library ieee;
--use ieee.std_logic_1164.all;
--use ieee.numeric_std.all;
--use ieee.math_real.all;
--
--entity comparator is
--  generic(
--    cycles_delay            : integer := 5;     -- Number of cycles o delay of the one core from the anotherone
--    stalled_cycles          : integer := 2000;  -- Number of cycles that the core is going to be stalled
--    cycles_being_different  : integer := 1000;  -- Number of cycles in which the PC is not equal in both cores
--    PC_RST                  : integer := 0      -- PC value at reset
--    );  
--  port(
--    clk            : in  std_logic;                      
--    rstn           : in  std_logic;
--    enable         : in  std_logic;                       -- Enables the module ('cycles_delay' cycles later)
--    alu1           : in  std_logic_vector(15 downto 0);   -- Result form the first core ALU 
--    alu2           : in  std_logic_vector(15 downto 0);   -- Result form the second core ALU 
--    pc1            : in  std_logic_vector(63 downto 0);   -- Current PC of the first core
--    pc2            : in  std_logic_vector(63 downto 0);   -- Current PC of the second core
--    stall1         : out std_logic;                       -- Signal to stall the first core
--    stall2         : out std_logic;                       -- Signal to stall the second core
--    reset_program  : out std_logic                        -- Reset the program if the result of both ALUs does not match
--    );  
--end;
--
--architecture rtl of comparator is
--    -- Signals to calculate the number of executed instructions/2 (each PC is two instructions)
--    signal executed_inst1 : unsigned(128 downto 0);
--    signal executed_inst2 : unsigned(128 downto 0);
--    signal next_pc1       : std_logic_vector(63 downto 0);
--    signal next_pc2       : std_logic_vector(63 downto 0);
--
--    -- Signals to delay the PC/ALU "stalled_cyles" cycles
--    type alu_array    is array (cycles_delay downto 0) of std_logic_vector(15 downto 0);
--    type PC_array     is array (cycles_delay downto 0) of std_logic_vector(63 downto 0);
--    type enable_array is array (cycles_delay downto 0) of std_logic;
--    signal delayed_alu    : alu_array; 
--    signal delayed_PC     : PC_array; 
--    signal delayed_enable : enable_array;
--
--
--    -- Signals to implement a counter that counts how many cycles the PC of both cores have been different 
--    constant counter_cyclesdiff_bits : integer := integer(ceil(log2(real(cycles_being_different))));
--    signal next_diffPC_count, diffPC_count : unsigned(counter_cyclesdiff_bits-1 downto 0);
--
--    -- Signals to implement the counter that counts the number of cycles the core have to be stalled
--    constant stall_counter_bits      : integer := integer(ceil(log2(real(stalled_cycles))));
--    signal stall_count   : unsigned(stall_counter_bits-1 downto 0);
--
--    -- Signals to implement the FSM to stall a core
--    signal stall  : std_logic;
--    type state_type is (stalled, not_stalled);  -- Define the states 
--    signal next_stall_state, stall_state : state_type;
--
--    -- When core its enable it has to wait 'cycles_delay' cycles to start working
--   signal enable_comparation : std_logic;
--begin 
--
--    -- Calculate the number o executed instructions/2 for both cores
--    process(clk)
--    begin
--        if rising_edge(clk) then
--            if rstn = '0' then
--	        executed_inst1 <= (others => '0');
--	        executed_inst2 <= (others => '0');
--               	next_pc1 <= std_logic_vector(to_unsigned(PC_RST, 64));
--	        next_pc2 <= std_logic_vector(to_unsigned(PC_RST, 64));
--       	    else
--                next_pc1 <= pc1; 
--                next_pc2 <= pc2; 
--                if next_pc1 /= pc1 and enable_comparation = '1' then
--                    executed_inst1 <= executed_inst1 + 1;
--                end if;
--                if next_pc2 /= pc2 and enable_comparation = '1' then
--                    executed_inst2 <= executed_inst2 + 1;
--                end if;
--            end if;
--        end if;   
--    end process;
--
--
--    -- A pipeline of flip-flops with "cycles_delay" flip-flops is generated.
--    -- This is useful if first processor is "cycles_delay" cycles ahead from the 
--    -- seconod processor. 
--
--    process(clk)
--    begin
--        if (cycles_delay /= 0) then    -- If there are no delays it is unnecesary
--            if rising_edge(clk) then
--                if rstn = '0' then
--                    for n in 0 to cycles_delay-1 loop
--                        delayed_PC(n)     <= std_logic_vector(to_unsigned(PC_RST, 64));
--                        delayed_alu(n)    <= (others => '0'); 
--                        delayed_enable(n) <= '0';
--                    end loop;
--                else 
--                    for n in 0 to cycles_delay-1 loop
--                        if n = 0 then
--                            delayed_PC(0)     <= pc1;
--                            delayed_alu(0)    <= alu1;
--                            delayed_enable(0) <= enable;
--                        else
--                            delayed_PC(n)     <= delayed_PC(n-1);  
--                            delayed_alu(n)    <= delayed_alu(n-1);  
--                            delayed_enable(n) <= delayed_enable(n-1);
--                        end if;
--                    end loop;
--                end if;
--            end if;
--        end if;
--    end process;
--
--    -- It compares the alus and PCs when cores are delayed  and stalls one core or restart
--    -- the program if needed
--    delay: if (cycles_delay /= 0) generate
--        reset_program <= '0' when alu2 = delayed_alu(cycles_delay-1) or stall = '1' or enable_comparation = '0' else
--                      '1';
--
--        next_diffPC_count <= (others => '0') when pc2 = delayed_PC(cycles_delay-1) or stall = '1' or enable_comparation = '0' else
--                             diffPC_count + 1;  
--
--        enable_comparation <= delayed_enable(cycles_delay-1);
--    end generate delay;
--        
--
--    -- It compares the alus and PCs when cores are not delayed  and stalls one core or restart
--    -- the program if needed
--    no_delay: if (cycles_delay = 0) generate
--
--        reset_program <= '0' when alu1 = alu2 or stall = '1' or enable_comparation = '0'  else
--                      '1';
--
--        next_diffPC_count <= (others => '0') when pc1 = pc2 or stall = '1' or enable_comparation = '0' else
--                              diffPC_count + 1;  
--
--        enable_comparation <= enable;
--    end generate no_delay;
--
--
--
--    -- finite state machine to stall the core and PC being different cycles counter
--    process(clk)
--    begin
--        if rising_edge(clk) then
--            if rstn = '0' then
--                stall_state  <= not_stalled;
--                diffPC_count <= (others => '0');
--                stall_count  <= (others => '0');
--       	    else
--                stall_state  <= next_stall_state;
--                diffPC_count <= next_diffPC_count;
--                if stall_state /= not_stalled then
--                    stall_count <= stall_count + 1;
--                else 
--                    stall_count <= (others => '0');
--                end if;
--            end if;
--        end if;   
--    end process;
--
--    process(stall_count, diffPC_count, stall_state)
--    begin
--        next_stall_state <= stall_state;
--        case stall_state is
--            when not_stalled =>
--                stall <= '0';
--                if to_integer(diffPC_count) = cycles_being_different then
--                    next_stall_state <= stalled;
--                end if;
--            when stalled  =>
--                stall <= '1';
--                if stall_count = stalled_cycles then
--                    next_stall_state <= not_stalled;
--                end if;
--        end case;
--    end process;
--   
--    stall1 <= stall when executed_inst1 >= executed_inst2 else '0';
--    stall2 <= stall when executed_inst2 >= executed_inst1 else '0';
--    
--
--end;
--
--
	    	
