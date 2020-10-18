-- -----------------------------------------------
-- Project Name   : De-RISC
-- File           : testbench.vhd
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


entity lockstep_testbench is
    port(
        rst                  : in  std_ulogic;
        clk                  : in  std_ulogic;
        stall2_i             : in  std_logic;
        -- pc1_i                : in  std_logic_vector(63 downto 0);
        icnt2_o              : out std_logic_vector(1 downto 0)
        -- stall1_o             : out std_logic
    );
end;

architecture rtl of lockstep_testbench is

    signal random_number, random_number2 : unsigned(7 downto 0);

    signal icnt2: std_logic_vector(1 downto 0);

    signal counter, counter_limit : unsigned(2 downto 0) := "000";
    
    constant BURDEN : integer := 0;

begin
  
    
    process (clk) is
        variable next_instruction : std_logic := '0';
    begin
        if (rising_edge(clk)) then
            if counter = counter_limit + BURDEN then
                next_instruction := '1';
                counter <= (others => '0');
            else
                counter <= counter + 1;
            end if;

            if stall2_i = '0' and next_instruction = '1' then
                next_instruction := '0';
                if random_number2 < 60 then
                    icnt2 <= "11";
                else
                    icnt2 <= "10";
                end if;
            else
                icnt2 <= "00";
            end if;
        end if;
    end process;

    icnt2_o <= icnt2;

    speed_select : process
    begin
        if random_number <= 25 then
            counter_limit <= to_unsigned(0, 3);
        elsif random_number > 25 and random_number <= 70 then
            counter_limit <= to_unsigned(1, 3);
        elsif random_number > 70 and random_number <= 130 then
            counter_limit <= to_unsigned(2, 3);
        elsif random_number < (260 - 70)  and random_number > 130 then
            counter_limit <= to_unsigned(3, 3);
        elsif random_number < (260 - 25)  and random_number >= (260 - 70) then
            counter_limit <= to_unsigned(4, 3);
        elsif random_number >= (260 - 25) then
            counter_limit <= to_unsigned(5, 3);
        end if;
        wait on icnt2;
    end process speed_select;
   
   -- assert (abs(to_integer(unsigned(pc1)) - to_integer(unsigned(pc2))) < (max_slack+3)*8) report "difference is bigger than max_slack "& integer'image(abs(to_integer(unsigned(pc1)) - to_integer(unsigned(pc2)))) severity failure;
    --assert (abs(to_integer(unsigned(pc1)) - to_integer(unsigned(pc2))) > min_slack*8) report "difference is smaller than min_slack "& integer'image(abs(to_integer(unsigned(pc1)) - to_integer(unsigned(pc2)))) severity failure;


    process is
        -- variable seed1, seed2 : positive;
        variable seed1 : positive := 1;
        variable seed2 : positive := 9;
        variable x : real;
        variable y : integer;
    begin
        uniform(seed1, seed2, x); 
        y := integer(floor(x * 260.0));
        random_number <= to_unsigned(y, 8);
        wait for 10 ns;
    end process;

    process is
        -- variable seed1, seed2 : positive;
        variable seed1 : positive := 4;
        variable seed2 : positive := 48;
        variable x : real;
        variable y : integer;
    begin
        uniform(seed1, seed2, x); 
        y := integer(floor(x * 260.0));
        random_number2 <= to_unsigned(y, 8);
        wait for 10 ns;
    end process;




end;


