library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.lockstep_pkg.all;



entity comparator is
    generic (
        WRITE_ENTRIES : integer := 256;
        BUS_LENGTH    : integer := 128
    );
    port(
        clk            : in  std_logic;
        rstn           : in  std_logic;
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
        --------------------------------
        enable_core1_i : in  std_logic;
        enable_core2_i : in  std_logic;
        c1_ahead_c2_i  : in  std_logic;
        error_o        : out std_logic
    );  
end;


architecture rtl of comparator is
    signal valid_resp : std_logic;
    signal write_fifo, compare_writes, r_compare_writes : std_logic;    
    signal r_access_trans, access_trans : std_logic_vector(1 downto 0);    
    signal fifo_wdata, fifo_rdata, delayed_core_wdata, r_delayed_core_wdata : std_logic_vector(BUS_LENGTH-1 downto 0);
begin

    valid_resp <= not(ahbmi_hresp_i(0) or ahbmi_hresp_i(1));
    

    write_fifo <= '1' when (valid_resp = '1' and ahbmi_ready_i = '1') and 
                  ((enable_core1_i = '1' and c1_ahead_c2_i = '1' and r_access_trans(0) = '1' and ahbmo1_write_i = '1') 
                  or 
                  (enable_core2_i = '1' and c1_ahead_c2_i = '0' and r_access_trans(1) = '1' and ahbmo2_write_i = '1')) 
                  else '0';
    
    compare_writes <=  '1' when (valid_resp = '1' and ahbmi_ready_i = '1') and 
                  ((enable_core1_i = '1' and c1_ahead_c2_i = '0' and r_access_trans(0) = '1' and ahbmo1_write_i = '1') 
                  or 
                  (enable_core2_i = '1' and c1_ahead_c2_i = '1' and r_access_trans(1) = '1' and ahbmo2_write_i = '1')) 
                  else '0';
    
    
    fifo_wdata <= ahbmo1_wdata_i when c1_ahead_c2_i = '1' else
                  ahbmo2_wdata_i;
    
    delayed_core_wdata <= ahbmo1_wdata_i when c1_ahead_c2_i = '0' else
                          ahbmo2_wdata_i;

    access_trans(0) <= ahbmo1_trans_i and ahb_access_i(0);
    access_trans(1) <= ahbmo2_trans_i and ahb_access_i(1);
    ff: process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                r_access_trans <= "00";
                r_compare_writes <= '0';
                r_delayed_core_wdata <= (others => '0');
            else
                if ahbmi_ready_i = '1' then
                    r_access_trans <= access_trans(1) & access_trans(0);
                end if;
                r_compare_writes <= compare_writes;
                r_delayed_core_wdata <= delayed_core_wdata; 
            end if;
        end if;
    end process;


    fifo_inst : fifo
    generic map(
        RAM_LENGTH => WRITE_ENTRIES,
        BUS_LENGTH => BUS_LENGTH
        )
    port map(
        clk      => clk, 
        rstn     => rstn,
        write_i  => write_fifo,
        read_i   => compare_writes, 
        wdata_i  => fifo_wdata, 
        rdata_o  => fifo_rdata
    );

    error_o <= '1' when r_compare_writes = '1' and (not (fifo_rdata = r_delayed_core_wdata)) else
               '0';

end;
