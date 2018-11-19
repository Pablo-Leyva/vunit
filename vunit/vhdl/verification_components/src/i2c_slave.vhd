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

entity i2c_slave is
  generic (
    slave : i2c_slave_t);
  port (
    sda : inout std_logic := 'Z';
    scl : inout std_logic := 'Z'
  );
end entity;

architecture behavioural of i2c_slave is

  constant I2C_PERIOD_C : time := get_i2c_period(slave);

  signal sda_o : std_logic := '1';
  signal sda_i : std_logic;
  signal scl_o : std_logic := '1';
  signal scl_i : std_logic;


  type state_t is (IDLE, I2C_ADDR, REG_ADDR, WRITE_DATA, READ_DATA);


  signal stop_condition_s,
         start_condition_s : std_logic := '0';
  --procedure write_byte(variable byte_v : in std_logic_vector(7 downto 0); variable ack : out boolean);

begin

  sda <= 'Z' when sda_o = '1' else '0';
  sda_i <= '0' when sda = '0' else '1';

  scl <= 'Z' when scl_o = '1' else '0';
  scl_i <= '0' when scl = '0' else '1';

  p_msg_decoding : process
    variable request_msg, reply_msg : msg_t;
    variable type_msg : msg_type_t;

  begin

    receive(net, slave.p_actor, request_msg);
    reply_msg := new_msg(sender => slave.p_actor);

    type_msg := message_type(request_msg);
    handle_sync_message(net, type_msg, request_msg);

    if type_msg = i2c_start_msg then

    elsif type_msg = i2c_stop_msg then

    elsif type_msg = i2c_write_msg then

    elsif type_msg = i2c_read_msg then

    else
        unexpected_msg_type(type_msg);
    end if;
  end process p_msg_decoding;

  p_start_stop : process (sda_i, scl_i)
    variable sda_v, scl_v : std_logic;
    variable case_var_v : std_logic_vector(3 downto 0);
  begin
    case_var_v := sda_v & sda_i & scl_v & scl_i;
    stop_condition_s    <= '0';
    start_condition_s   <= '0';

    case( case_var_v ) is
      when "0111" =>
        stop_condition_s  <= '1';
        info("stop condition detected");
      when "1011" =>
        start_condition_s <= '1';
        info("start condition detected");
      when others =>
    end case;
    sda_v := sda_i;
    scl_v := scl_i;
  end process p_start_stop;

  p_i2c_fsm : process
    variable state : state_t := IDLE;

    variable i2c_byte    : std_logic_vector(7 downto 0);
    variable i2c_address : std_logic_vector(6 downto 0);
    variable i2c_cmd     : std_logic;
    variable i2c_ack     : boolean;

    variable i2c_reg     : std_logic_vector(slave.p_reg_address'range);

    procedure write_byte(variable byte_v : in  std_logic_vector(7 downto 0)) is
    begin
      sda_o <= byte_v(7);
      wait until rising_edge(scl_i);
      for i in 0 to 6 loop
        wait until falling_edge(scl_i); wait for get_hold_time(slave);
        sda_o <= byte_v(6 - i);
        wait until rising_edge(scl_i);
      end loop;
      wait until falling_edge(scl_i); wait for get_hold_time(slave);
      sda_o <= '1'; --release the bus
    end procedure write_byte;

    procedure write_ack( variable ack : in  boolean ) is
      begin
        wait until falling_edge(scl_i); wait for get_hold_time(slave);
        scl_o <= '0'; wait for get_hold_time(slave);
        sda_o <= '0' when ack else '1';
        wait for I2C_PERIOD_C/2;
        scl_o <= '1';
        wait until falling_edge(scl_i); wait for get_hold_time(slave);
        sda_o <= '1' ;  --release the bus
      end procedure write_ack;

    procedure read_byte( variable byte_v : out  std_logic_vector(7 downto 0) ) is
    begin
      for i in 0 to 7 loop
        wait until rising_edge(scl_i) or rising_edge(stop_condition_s) or rising_edge(start_condition_s);
        if( (stop_condition_s or start_condition_s) = '1') then exit; end if;
        byte_v(7 - i) := sda_i;
      end loop;
    end procedure read_byte;

    procedure read_ack( variable ack : out  boolean ) is
    begin
      wait until rising_edge(scl_i);
      ack := (sda_i='0');
    end procedure read_ack;

  begin

    case( state ) is

      when IDLE =>
        wait until rising_edge(start_condition_s);
        state := I2C_ADDR;

      when I2C_ADDR =>
        read_byte( i2c_byte );
        i2c_address := i2c_byte(7 downto 1);
        i2c_cmd := i2c_byte(0);
        i2c_ack := ( i2c_address = slave.p_i2c_address(7 downto 1) );
        --TODO: clock stretching based on read/write(?)
        write_ack( i2c_ack );

        if(i2c_cmd)then --Read command
          state := READ_DATA;
        else --Write command
          state := REG_ADDR;
        end if;

      when REG_ADDR =>
        for i in 1 to slave.p_i2c_address'length/8 loop
          read_byte( i2c_byte );
          i2c_reg( i2c_reg'high-7*(i-1) downto i2c_reg'high-7*(i) ) := i2c_byte;
          i2c_ack := true; --TODO: check of supported address + clock stretching
          write_ack( i2c_ack ); --let's assume we support all addresses
        end loop;
        state := WRITE_DATA;

      when WRITE_DATA =>
        while(i2c_ack) loop
          read_byte( i2c_byte );
          if( (stop_condition_s or start_condition_s) = '1') then exit; end if;
          i2c_ack := true; --TODO: Push data to queue + clock stretching
          write_ack( i2c_ack ); --we store all we can :)
        end loop;
        if( start_condition_s = '1' ) then
          state := I2C_ADDR;
        else
          state := IDLE;
        end if;

      when READ_DATA =>
        i2c_ack := true;
        while(i2c_ack) loop
          -- TODO: pop data from TX queue matching the previously i2c_reg value
          write_byte( i2c_byte );
          read_ack( i2c_ack ); --Wants the master more data?
        end loop;
        state := IDLE;

      when others =>
        state := IDLE;

    end case;

  end process p_i2c_fsm;

end architecture;
