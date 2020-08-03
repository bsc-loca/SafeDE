library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity comparator is
  port(
    clk            : in  std_logic;    
    rstn           : in  std_logic;
    enable         : in  std_logic;                       -- Enables the module
    alu1           : in  std_logic_vector(63 downto 0);   -- Result form the first core ALU 
    alu2           : in  std_logic_vector(63 downto 0);   -- Result form the second core ALU 
    pc1            : in  std_logic_vector(63 downto 0);   -- Current PC of the first core
    pc2            : in  std_logic_vector(63 downto 0);   -- Current PC of the second core
    reset_program  : out std_logic                        -- Reset the program if the result of both ALUs does not match
    );  
end;


architecture rtl of comparator is
begin
end;
