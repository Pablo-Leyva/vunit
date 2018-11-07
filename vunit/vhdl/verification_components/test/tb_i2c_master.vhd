-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2018, Lars Asplund lars.anders.asplund@gmail.com

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

context work.vunit_context;
context work.com_context;
context work.data_types_context;
use work.i2c_pkg.all;
use work.sync_pkg.all;
use work.stream_master_pkg.all;
use work.stream_slave_pkg.all;

entity tb_i2c_master is
  generic (runner_cfg : string);
end entity;

architecture a of tb_i2c_master is

  constant i2c_master : i2c_master_t := new_i2c_master;

  signal sda : std_logic := 'Z';
  signal scl : std_logic := 'Z';

  signal sda_in : std_logic;
  signal sda_oe : std_logic;
  signal scl_in : std_logic;
  signal scl_oe : std_logic;

  signal rst : std_ulogic := '0';

begin

  sda <= 'H'; --Pull-Up resistor
  scl <= 'H'; --Pull-Up resistor

  main : process

    variable data_buff : queue_t := new_queue;
    variable i2c_ack   : boolean := true;

  begin

    test_runner_setup(runner, runner_cfg);

    rst <= '0';  wait for get_i2c_period(i2c_master);
    rst <= '1';  wait for get_i2c_period(i2c_master);
    rst <= '0';  wait for get_i2c_period(i2c_master);

    if run("i2c Test 01") then
      wait for 100 us;
      push_std_ulogic_vector(data_buff, std_logic_vector(to_unsigned(1,8)));
      push_std_ulogic_vector(data_buff, std_logic_vector(to_unsigned(1,8)));
      push_std_ulogic_vector(data_buff, std_logic_vector(to_unsigned(1,8)));

      write_i2c_buff(net, i2c_master, "1010010", "11111111", 3, data_buff, i2c_ack);

      if(i2c_ack) then
          info("Transfer ACK! :) ");
        else
          info("Transfer NACK! :(");
      end if;

    elsif run("Test 02") then
      info("Test 02");
      wait for 100 us;
      i2c_ack := true;
      read_i2c_buff(net, i2c_master, "1010010", "11111111", 2, data_buff, i2c_ack);
      for i in 0 to 2-1 loop
        info("Data Readed:"&to_string(pop_std_ulogic_vector(data_buff)));
      end loop;
    end if;

    test_runner_cleanup(runner);

  end process;

  test_runner_watchdog(runner, 20 ms);

  i_i2c_master: entity work.i2c_master
    generic map (
      master => i2c_master)
    port map (
      sda => sda,
      scl => scl
    );

    sda <= 'Z' when sda_oe = '0' else '0';
    sda_in <= '0' when sda = '0' else '1';

    scl <= 'Z' when scl_oe = '0' else '0';
    scl_in <= '0' when scl = '0' else '1';

  i_i2c_master : entity work.i2c_slave
    generic map (
      slave => i2c_slave)
    port map (
      sda => sda,
      scl => scl
    );


end architecture;
