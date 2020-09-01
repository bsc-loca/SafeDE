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

    signal random_number :unsigned(7 downto 0);

    signal speed : unsigned(2 downto 0) := to_unsigned(4, 3);   -- It makes the false core go slower "1" or faster "2" than the real core
    signal icnt2_int: std_logic_vector(1 downto 0);

    -- constant fifo_length : integer := 500;
    -- signal false_pc2 : std_logic_vector(63 downto 0);
    -- type pc_vector is array (integer range <>) of std_logic_vector(63 downto 0);

    -- signal stall2_from_test : std_logic;

begin
  
  -- It copies the same PC as the core in a fifo

  --  pc2_o <= pc1_i when comparator_enabled = '1' else 
  --           false_pc2;
    --stall1_o <= '0';
    --stall2_from_test <= '0';
    --fifo : process (clk) is 
    --    variable pc_fifo : pc_vector(0 to fifo_length);
    --    variable fifo_write_pointer : integer := 0;
    --    variable fifo_read_pointer  : integer := 0;
    --begin
    --    if (rising_edge(clk)) then
    --        pc_fifo(fifo_write_pointer) := pc1_i;
    --        if fifo_write_pointer < fifo_length -1 then
    --            fifo_write_pointer := fifo_write_pointer + 1;
    --        else
    --            fifo_write_pointer := 0;
    --        end if;

    --        if stall2_i = '0' and stall2_from_test = '0'  then
    --            pc2_o  <= pc_fifo(fifo_read_pointer);
    --            if fifo_read_pointer < fifo_length-1 then
    --                fifo_read_pointer := fifo_read_pointer + 1;
    --            else
    --                fifo_read_pointer := 0;
    --            end if;
    --        end if;
    --    end if;
    --end process fifo;
    
    fifo : process (clk) is 
        variable false_icnt : std_logic_vector(1 downto 0) := "01";
        variable delay    : unsigned(5 downto 0) := (others => '0');
    begin
        if (rising_edge(clk)) then
            delay := delay + speed;
            if stall2_i = '0' then
                if delay = 0 or (delay = 1 and speed = 2) then
                    false_icnt := not false_icnt ;
                end if;
                icnt2_int  <= false_icnt;
            else
                icnt2_int <= "00";
            end if;
        end if;
    end process fifo;
    process
    begin
        wait on icnt2_int;
        icnt2_o <= icnt2_int;
        wait for 10 ns;
        icnt2_o <= "00";
    end process;



    speed_select : process
    begin
        if random_number > 253 then
            if speed = 1 then
                speed <= to_unsigned(2, 3);
            else
                speed <= to_unsigned(1, 3);
            end if;
        end if;
        wait on icnt2_int;
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




end;


