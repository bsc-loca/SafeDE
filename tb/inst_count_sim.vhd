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


entity inst_count_sim is
    port(
        clk                  : in  std_ulogic;
        stall2_i             : in  std_logic;
        stall1_i             : in  std_logic;
        icnt1_o              : out std_logic_vector(1 downto 0);
        icnt2_o              : out std_logic_vector(1 downto 0)
    );
end;

architecture rtl of inst_count_sim is

    signal random_number1, random_number2 : unsigned(1 downto 0);

begin
    
    process 
    begin
        if stall1_i = '0' then
            icnt1_o <= std_logic_vector(random_number1);
        else
            icnt1_o <= "00";
        end if;
        if stall2_i = '0' then
            icnt2_o <= std_logic_vector(random_number2); 
        else
            icnt2_o <= "00";
        end if;
        wait for 1 ns;
    end process;


    process is
        -- variable seed1, seed2 : positive;
        variable seed1 : positive := 4;
        variable seed2 : positive := 7;
        variable x : real;
        variable y : integer;
    begin
        uniform(seed1, seed2, x); 
        y := integer(floor(x * 4.0));
        random_number1 <= to_unsigned(y, 2);
        wait for 10 ns;
    end process;

    process is
        -- variable seed1, seed2 : positive;
        variable seed1 : positive := 3;
        variable seed2 : positive := 43;
        variable x : real;
        variable y : integer;
    begin
        uniform(seed1, seed2, x); 
        y := integer(floor(x * 4.0));
        random_number2 <= to_unsigned(y, 2);
        wait for 10 ns;
    end process;

end;

