-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2018, Lars Asplund lars.anders.asplund@gmail.com

library ieee;
use ieee.std_logic_1164.all;

context work.vunit_context;
context work.com_context;
use work.stream_master_pkg.all;
use work.stream_slave_pkg.all;
use work.sync_pkg.all;
use work.integer_vector_ptr_pkg.all;
use work.queue_pkg.all;

package i2c_pkg is
  type i2c_master_t is record
    p_actor : actor_t;
    p_bus_freq : integer;
  end record;

  type i2c_slave_t is record
    p_actor : actor_t;
    p_i2c_address : std_logic_vector(7 downto 0);
    p_bus_freq : integer;
  end record;

  type i2c_transfer_t is record
    p_i2c_address : std_logic_vector(7 downto 0);
    p_reg_address : std_logic_vector(7 downto 0);
    p_data_length : integer;
    p_data        : queue_t;
  end record;

  type i2c_answer_t is record
    p_ack         : boolean;
    p_data_length : integer;
    p_data        : queue_t;
  end record;

  constant i2c_write_msg : msg_type_t := new_msg_type("write i2c transfer");
  constant i2c_read_msg  : msg_type_t := new_msg_type("read i2c transfer");
  constant i2c_start_msg : msg_type_t := new_msg_type("start condition");
  constant i2c_stop_msg  : msg_type_t := new_msg_type("stop condition");

  --constant i2c_master_logger : logger_t := get_logger("vunit_lib:i2c_master_pkg");
  impure function new_i2c_transfer return i2c_transfer_t;
  impure function new_i2c_master(p_bus_freq : integer := 100000) return i2c_master_t;
  impure function new_i2c_slave(p_i2c_address : std_ulogic_vector(7 downto 0); p_bus_freq : integer := 100000) return i2c_slave_t;
  impure function get_i2c_period(i2c_master : i2c_master_t) return time;

  procedure write_i2c_buff(signal   net         : inout network_t;
                                    i2c_master  : in    i2c_master_t;
                                    i2c_address : in    std_logic_vector;
                                    reg_address : in    std_logic_vector;
                                    data_length : in    integer;
                                    data        : in    queue_t;
                           variable ack         : out   boolean);

  procedure read_i2c_buff( signal   net         : inout network_t;
                                    i2c_master  : in    i2c_master_t;
                                    i2c_address : in    std_logic_vector;
                                    reg_address : in    std_logic_vector;
                                    data_length : in    integer;
                                    data        : out   queue_t;
                           variable ack         : out   boolean);

end package;

package body i2c_pkg is

  impure function new_i2c_transfer
    return i2c_transfer_t is
  begin
    return (p_i2c_address => (others => '0'),
            p_reg_address => (others => '0'),
            p_data_length => 0,
            p_data => new_queue
        );
  end;

  impure function new_i2c_master(p_bus_freq : integer := 100000)
    return i2c_master_t is
  begin
    return (p_actor => new_actor,
            p_bus_freq         => p_bus_freq
        );
  end;

  impure function new_i2c_slave(p_i2c_address : std_ulogic_vector(7 downto 0); p_bus_freq : integer := 100000)
    return i2c_slave_t is
  begin
    return (p_actor => new_actor,
            p_i2c_address => p_i2c_address,
            p_bus_freq => p_bus_freq
        );
  end;

  impure function get_i2c_period(i2c_master : i2c_master_t)
    return time is
  begin
    return natural( real(1.0)/real(i2c_master.p_bus_freq)*real(10e6) ) * (1 us);
  end;

  procedure write_i2c_buff(signal   net         : inout network_t;
                                    i2c_master  : in    i2c_master_t;
                                    i2c_address : in    std_logic_vector;
                                    reg_address : in    std_logic_vector;
                                    data_length : in    integer;
                                    data        : in    queue_t;
                           variable ack         : out   boolean) is
    variable i2c_transfer_r : i2c_transfer_t := new_i2c_transfer;
    variable msg : msg_t ;--:= new_msg(i2c_write_msg);

  begin
    -- Start Condition
    msg := new_msg(i2c_start_msg);
    send(net, i2c_master.p_actor, msg);

    wait for 20*get_i2c_period(i2c_master);

    -- Write reg address
    msg := new_msg(i2c_write_msg);
    push_std_ulogic_vector(msg, i2c_address);
    push_std_ulogic_vector(msg, reg_address);
    push_integer(msg, data_length);
    while(not is_empty(data)) loop
      push_std_ulogic_vector(msg, pop_std_ulogic_vector(data));
    end loop;
    request(net, i2c_master.p_actor, msg, ack);

    wait for 20*get_i2c_period(i2c_master);

    -- Stop Condition
    msg := new_msg(i2c_stop_msg);
    send(net, i2c_master.p_actor, msg);
  end;

  procedure read_i2c_buff(signal    net         : inout network_t;
                                    i2c_master  : in    i2c_master_t;
                                    i2c_address : in    std_logic_vector;
                                    reg_address : in    std_logic_vector;
                                    data_length : in    integer;
                                    data        : out   queue_t;
                           variable ack         : out   boolean) is
    variable i2c_transfer_r : i2c_transfer_t := new_i2c_transfer;
    variable msg,
             reply_msg: msg_t;--:= new_msg(i2c_write_msg);

  begin
    data := new_queue;
    -- Start Condition
    msg := new_msg(i2c_start_msg);
    send(net, i2c_master.p_actor, msg);

    -- Write reg address
    msg := new_msg(i2c_write_msg);
    push_std_ulogic_vector(msg, i2c_address);
    push_std_ulogic_vector(msg, reg_address);
    push_integer(msg, 0);
    wait for 20*get_i2c_period(i2c_master);
    send(net, i2c_master.p_actor, msg);

    -- Start Condition
    msg := new_msg(i2c_start_msg);
    wait for 20*get_i2c_period(i2c_master);
    send(net, i2c_master.p_actor, msg);

    -- Read data
    msg := new_msg(i2c_read_msg);
    push_std_ulogic_vector(msg, i2c_address);
    push_integer(msg, data_length);
    wait for 20*get_i2c_period(i2c_master);
    send(net, i2c_master.p_actor, msg);

    receive_reply(net, msg, reply_msg);
    ack := pop_boolean(reply_msg);
    if(ack) then
      for i in 0 to data_length-1 loop
        push_std_ulogic_vector(data,pop_std_ulogic_vector(reply_msg));
      end loop;
    end if;
    msg := new_msg(i2c_stop_msg);
    wait for 20*get_i2c_period(i2c_master);
    send(net, i2c_master.p_actor, msg);

    -- Stop Condition
    msg := new_msg(i2c_stop_msg);
    send(net, i2c_master.p_actor, msg);
  end;

end package body;
