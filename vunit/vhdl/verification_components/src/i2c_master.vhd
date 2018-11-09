-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2018, Lars Asplund lars.anders.asplund@gmail.com

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

context work.vunit_context;
context work.com_context;
use work.i2c_pkg.all;
use work.queue_pkg.all;
use work.sync_pkg.all;

entity i2c_master is
  generic (
    master : i2c_master_t);
  port (
    sda : inout std_logic := 'Z';
    scl : inout std_logic := 'Z'
  );
end entity;

architecture behavioural of i2c_master is

  constant byte_actor : actor_t := new_actor("byte actor", inbox_size => 1);

  constant I2C_PERIOD_C : time := get_i2c_period(master);

  signal sda_o : std_logic := '1';
  signal sda_i : std_logic;
  signal scl_o : std_logic := '1';
  signal scl_i : std_logic;

  --procedure write_byte(variable byte_v : in std_logic_vector(7 downto 0); variable ack : out boolean);

begin

  sda <= 'Z' when sda_o = '1' else '0';
  sda_i <= '0' when sda = '0' else '1';

  scl <= 'Z' when scl_o = '1' else '0';
  scl_i <= '0' when scl = '0' else '1';

  p_msg_decoding : process
    variable request_msg, reply_msg : msg_t;
    variable type_msg : msg_type_t;
    variable ack : boolean := false;
    variable i2c_address : std_logic_vector(7 downto 0) := (others => '0');
    variable byte_v : std_logic_vector(7 downto 0);
    variable index_v : integer := 0;

    procedure write_byte(variable byte_v : in  std_logic_vector(7 downto 0);
                         variable ack  : out boolean) is
    begin
      for i in 0 to 7 loop
        scl_o <= '0';           wait for 0 ns;
        sda_o <= byte_v(7 - i); wait for I2C_PERIOD_C/2;
        scl_o <= '1';           wait for I2C_PERIOD_C/2;if scl_i /= '1' then wait until scl_i = '1'; end if;
      end loop;
      scl_o <= '0';             wait for 0 ns;
      sda_o <= '1';             wait for I2C_PERIOD_C/2;

      -- Clock stretching
      scl_o <= '1'; if scl_i /= '1' then wait until scl_i = '1'; end if;

      ack := (sda_i = '0');
      wait for I2C_PERIOD_C/2;
      scl_o <= '0';
    end procedure write_byte;

    procedure read_byte( variable byte_v : out  std_logic_vector(7 downto 0);
                         variable ack  : in boolean) is

    begin

      for i in 0 to 7 loop
        scl_o <= '0'; wait for I2C_PERIOD_C/2;
        scl_o <= '1'; if scl_i /= '1' then wait until scl_i = '1'; end if;
        byte_v(7 - i) := sda_i;
        wait for I2C_PERIOD_C/2;
      end loop;

      scl_o <= '0'; wait for 0 ns;
      sda_o <= '0' when ack else '1'; wait for I2C_PERIOD_C/2;

      scl_o <= '1'; if scl_i /= '1' then wait until scl_i = '1'; end if;

      wait for I2C_PERIOD_C/2;
      scl_o <= '0'; wait for 0 ns;
      sda_o <= '1';

    end procedure read_byte;

  begin

    receive(net, master.p_actor, request_msg);
    reply_msg := new_msg(sender => master.p_actor);

    type_msg := message_type(request_msg);
    handle_sync_message(net, type_msg, request_msg);

    if type_msg = i2c_start_msg then

      scl_o <= '0'; wait for I2C_PERIOD_C/4;
      sda_o <= '1'; wait for I2C_PERIOD_C/4;
      scl_o <= '1'; wait for I2C_PERIOD_C/4;
      sda_o <= '0'; wait for I2C_PERIOD_C/4;

    elsif type_msg = i2c_stop_msg then
      scl_o <= '0'; wait for I2C_PERIOD_C/4;
      sda_o <= '0'; wait for I2C_PERIOD_C/4;
      scl_o <= '1'; wait for I2C_PERIOD_C/4;
      sda_o <= '1'; wait for I2C_PERIOD_C/4;

    elsif type_msg = i2c_write_msg then
      -- I2C ADDRESS
      i2c_address := pop_std_ulogic_vector(request_msg)&'0'; --write
      write_byte(i2c_address, ack);

      -- REG ADDRESS
      if (ack) then
        write_byte(pop_std_ulogic_vector(request_msg), ack);
      end if;

      -- DATA
      if (ack) then
       for i in 0 to pop_integer(request_msg)-1 loop
         write_byte(pop_std_ulogic_vector(request_msg), ack);
       end loop;
      end if;
      acknowledge(net, request_msg, ack);

    elsif type_msg = i2c_read_msg then
      -- I2C ADDRESS
      i2c_address := pop_std_ulogic_vector(request_msg)&'1'; --read
      write_byte(i2c_address, ack);

      -- DATA
      push_boolean(reply_msg, ack);
      if (ack) then
        index_v := pop_integer(request_msg);
        for i in 0 to index_v-1 loop
          ack := i/=(index_v-1);
          read_byte(byte_v, ack);
          push_std_ulogic_vector(reply_msg, byte_v);
        end loop;
      end if;
      reply(net, request_msg, reply_msg);

    else
        unexpected_msg_type(type_msg);
    end if;
  end process p_msg_decoding;

end architecture;
