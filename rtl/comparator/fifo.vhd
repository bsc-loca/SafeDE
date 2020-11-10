library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity fifo is
    generic (
        RAM_LENGTH : integer := 16;     
        BUS_LENGTH : integer := 128
    );
    port(
        clk            : in  std_logic;
        rstn           : in  std_logic;
        write_i        : in  std_logic;
        read_i         : in  std_logic;
        wdata_i        : in  std_logic_vector(BUS_LENGTH-1 downto 0);
        rdata_o        : out std_logic_vector(BUS_LENGTH-1 downto 0)
    );  
end;


architecture rtl of fifo is
    constant RAM_INDEX : integer := integer(ceil(log2(real(RAM_LENGTH))));
    type ram_type is array (RAM_LENGTH-1 downto 0) of std_logic_vector(BUS_LENGTH-1 downto 0);
    signal ram : ram_type;
    signal addr, r_write_addr, r_read_addr : unsigned(RAM_INDEX-1 downto 0);
begin
    RAM_PROC: process(clk)
    begin
        if rising_edge(clk) then
            -- RAM
            if write_i = '1' then        
                ram(to_integer(addr)) <= wdata_i;
            end if;
            rdata_o <= ram(to_integer(addr));

            -- FIFO
            if rstn = '0' then
                r_write_addr <= (others => '0');
                r_read_addr <=  (others => '0');
            else
                if write_i = '1' then
                    r_write_addr <= r_write_addr + 1;
                end if;
                if read_i = '1' then
                    r_read_addr <= r_read_addr + 1;
                end if;
            end if;
        end if;
    end process;

    addr <= r_write_addr when write_i = '1' else
            r_read_addr;

end;
