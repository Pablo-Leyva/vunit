-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2018, Lars Asplund lars.anders.asplund@gmail.com
-- Author Slawomir Siluk slaweksiluk@gazeta.pl

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

context work.vunit_context;
context work.com_context;
use work.memory_pkg.all;
use work.wishbone_pkg.all;
use work.bus_master_pkg.all;

library osvvm;
use osvvm.RandomPkg.all;

entity tb_litebus_master is
    generic (
        runner_cfg : string;
        encoded_tb_cfg : string
    );
end entity;

architecture a of tb_litebus_master is

    type tb_cfg_t is record
        dat_width : positive;
        adr_width : positive;
        lat_answr : positive;
    end record tb_cfg_t;

    impure function decode(encoded_tb_cfg : string) return tb_cfg_t is
    begin
        return (dat_width => positive'value(get(encoded_tb_cfg, "dat_width")),
                adr_width => positive'value(get(encoded_tb_cfg, "adr_width")),
                lat_answr => positive'value(get(encoded_tb_cfg, "lat_answr")));
    end function decode;

    constant tb_cfg : tb_cfg_t := decode(encoded_tb_cfg);

    signal lb_clk   : std_logic := '0';
    signal lb_rst   : std_logic := '0';
    signal lb_adr   : std_logic_vector(tb_cfg.adr_width-1 downto 0);
    signal lb_dat_i : std_logic_vector(tb_cfg.dat_width-1 downto 0) := (others => '0');
    signal lb_dat_o : std_logic_vector(tb_cfg.dat_width-1 downto 0);
    signal lb_be    : std_logic_vector(tb_cfg.dat_width/8 -1 downto 0);
    signal lb_we    : std_logic := '0';
    signal lb_re    : std_logic := '0';
    signal lb_ack   : std_logic := '0';
    signal lb_nack  : std_logic := '0';

    constant master_logger : logger_t := get_logger("master");
    constant tb_logger : logger_t := get_logger("tb");
    constant bus_handle : bus_master_t := new_bus(data_length => tb_cfg.dat_width,
                                                address_length => tb_cfg.adr_width,
                                                logger => master_logger);

begin

    main : process

        variable tmp : std_logic_vector(lb_dat_i'range);
        variable value : std_logic_vector(lb_dat_i'range) := (others => '1');

    begin

        test_runner_setup(runner, runner_cfg);
        set_format(display_handler, verbose, true);
        show(tb_logger, display_handler, verbose);
        show(default_logger, display_handler, verbose);
        show(master_logger, display_handler, verbose);
        show(com_logger, display_handler, verbose);

        wait until rising_edge(lb_clk);

        if run("wr single") then
            info(tb_logger, "Writing...");
            write_bus(net, bus_handle, 0, value);

            wait until lb_ack='1';

        elsif run("rd single") then
            info(tb_logger, "Reading...");
            read_bus(net, bus_handle, 0, tmp);

        end if;

        info(tb_logger, "Done, quit...");

        wait for 50 ns;
        test_runner_cleanup(runner);
    end process;

    test_runner_watchdog(runner, 100 us);

    lb_clk <= not lb_clk after 5 ns;

    dut : entity work.litebus_master
    generic map (
        bus_handle => bus_handle)
    port map (
        clk    => lb_clk,
        rst    => lb_rst,
        addr   => lb_adr,
        re     => lb_re,
        we     => lb_we,
        be     => lb_be,
        data_i => lb_dat_i,
        data_o => lb_dat_o,
        ack    => lb_ack,
        nack   => lb_nack
    );

    p_slace : process
    begin
        wait until rising_edge(lb_clk);
        if(lb_re = '1' or lb_we = '1') then
            for i in 0 to tb_cfg.lat_answr-1 loop
                wait until rising_edge(lb_clk);
            end loop;
            lb_ack <= '1';
            wait until rising_edge(lb_clk);
            lb_ack <= '0';
        end if;
    end process;

end architecture;
