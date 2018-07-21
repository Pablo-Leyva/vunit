-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2018, Lars Asplund lars.anders.asplund@gmail.com

library ieee;
use ieee.std_logic_1164.all;

use work.logger_pkg.all;
use work.stream_master_pkg.all;
use work.stream_slave_pkg.all;
use work.sync_pkg.all;
context work.com_context;
context work.data_types_context;

package axi_stream_pkg is

  type axi_stream_master_t is record
    p_actor : actor_t;
    p_data_length : natural;
    p_user_length : natural;
    p_logger : logger_t;
  end record;

  type axi_stream_slave_t is record
    p_actor : actor_t;
    p_data_length : natural;
    p_user_length : natural;
    p_logger : logger_t;
  end record;

  constant axi_stream_logger : logger_t := get_logger("vunit_lib:axi_stream_pkg");
  impure function new_axi_stream_master(data_length : natural;
                                        logger : logger_t := axi_stream_logger;
                                        actor : actor_t := null_actor;
                                        user_length : natural := 0) return axi_stream_master_t;
  impure function new_axi_stream_slave(data_length : natural;
                                       logger : logger_t := axi_stream_logger;
                                       actor : actor_t := null_actor;
                                       user_length : natural := 0) return axi_stream_slave_t;
  impure function data_length(master : axi_stream_master_t) return natural;
  impure function user_length(master : axi_stream_master_t) return natural;
  impure function data_length(slave : axi_stream_slave_t) return natural;
  impure function user_length(slave : axi_stream_slave_t) return natural;
  impure function as_stream(master : axi_stream_master_t) return stream_master_t;
  impure function as_stream(slave : axi_stream_slave_t) return stream_slave_t;
  impure function as_sync(master : axi_stream_master_t) return sync_handle_t;

  constant push_axi_stream_msg : msg_type_t := new_msg_type("push axi stream");
  constant pop_axi_stream_msg : msg_type_t := new_msg_type("pop axi stream");

  -- Reference to future stream result
  alias stream_reference_t is msg_t;

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_master_t;
                            tdata : std_logic_vector;
                            tlast : std_logic;
                            tkeep : std_logic_vector;
                            tuser : std_logic_vector);

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_master_t;
                            tdata : std_logic_vector;
                            tlast : boolean;
                            tkeep : std_logic_vector;
                            tuser : std_logic_vector);

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_master_t;
                            tdata : std_logic_vector;
                            tlast : std_logic;
                            tkeep : std_logic_vector);

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_master_t;
                            tdata : std_logic_vector;
                            tlast : boolean;
                            tkeep : std_logic_vector);

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_master_t;
                            tdata : std_logic_vector;
                            tlast : std_logic);

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_master_t;
                            tdata : std_logic_vector;
                            tlast : boolean);

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_master_t;
                            tdata : std_logic_vector);

  procedure pop_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_slave_t;
                            variable tdata : out std_logic_vector;
                            variable tlast : out std_logic;
                            variable tkeep : out std_logic_vector;
                            variable tuser : out std_logic_vector);

  procedure pop_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_slave_t;
                            variable tdata : out std_logic_vector;
                            variable tlast : out std_logic;
                            variable tkeep : out std_logic_vector);

  procedure pop_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_slave_t;
                            variable tdata : out std_logic_vector;
                            variable tlast : out std_logic);

  procedure pop_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_slave_t;
                            variable tdata : out std_logic_vector);

  -- Non-blocking: pop a value from the stream to be read in the future
  procedure pop_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_slave_t;
                            variable reference : inout stream_reference_t);


  -- Blocking: Wait for reply to non-blocking pop
  procedure await_pop_axi_stream_reply(signal net : inout network_t;
                                       variable reference : inout stream_reference_t;
                                       variable tdata : out std_logic_vector;
                                       variable tlast : out std_logic;
                                       variable tkeep : out std_logic_vector;
                                       variable tuser : out std_logic_vector);

  procedure await_pop_axi_stream_reply(signal net : inout network_t;
                                       variable reference : inout stream_reference_t;
                                       variable tdata : out std_logic_vector;
                                       variable tlast : out std_logic;
                                       variable tkeep : out std_logic_vector);

  procedure await_pop_axi_stream_reply(signal net : inout network_t;
                                       variable reference : inout stream_reference_t;
                                       variable tdata : out std_logic_vector;
                                       variable tlast : out std_logic);

  procedure await_pop_axi_stream_reply(signal net : inout network_t;
                                       variable reference : inout stream_reference_t;
                                       variable tdata : out std_logic_vector);

end package;

package body axi_stream_pkg is

  impure function new_axi_stream_master(data_length : natural;
                                        logger : logger_t := axi_stream_logger;
                                        actor : actor_t := null_actor;
                                        user_length : natural := 0) return axi_stream_master_t is
    variable p_actor : actor_t;
  begin
    p_actor := actor when actor /= null_actor else new_actor;

    return (p_actor => p_actor,
            p_data_length => data_length,
            p_user_length => user_length,
            p_logger => logger);
  end;

  impure function new_axi_stream_slave(data_length : natural;
                                       logger : logger_t := axi_stream_logger;
                                       actor : actor_t := null_actor;
                                       user_length : natural := 0) return axi_stream_slave_t is
    variable p_actor : actor_t;
  begin
    p_actor := actor when actor /= null_actor else new_actor;

    return (p_actor => p_actor,
            p_data_length => data_length,
            p_user_length => user_length,
            p_logger => logger);
  end;

  impure function data_length(master : axi_stream_master_t) return natural is
  begin
    return master.p_data_length;
  end;

  impure function user_length(master : axi_stream_master_t) return natural is
  begin
    return master.p_user_length;
  end;

  impure function data_length(slave : axi_stream_slave_t) return natural is
  begin
    return slave.p_data_length;
  end;

  impure function user_length(slave : axi_stream_slave_t) return natural is
  begin
    return slave.p_user_length;
  end;

  impure function as_stream(master : axi_stream_master_t) return stream_master_t is
  begin
    return (p_actor => master.p_actor);
  end;

  impure function as_stream(slave : axi_stream_slave_t) return stream_slave_t is
  begin
    return (p_actor => slave.p_actor);
  end;

  impure function as_sync(master : axi_stream_master_t) return sync_handle_t is
  begin
    return master.p_actor;
  end;

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_master_t;
                            tdata : std_logic_vector;
                            tlast : std_logic;
                            tkeep : std_logic_vector;
                            tuser : std_logic_vector) is
    variable msg : msg_t := new_msg(push_axi_stream_msg);
    constant normalized_data : std_logic_vector(data_length(axi_stream)-1 downto 0) := tdata;
    variable normalized_keep : std_logic_vector(data_length(axi_stream)/8-1 downto 0) := tkeep;
    constant normalized_user : std_logic_vector(user_length(axi_stream)-1 downto 0) := tuser;
  begin
    push_std_ulogic_vector(msg, normalized_data);
    push_std_ulogic(msg, tlast);
    push_std_ulogic_vector(msg, normalized_keep);
    push_std_ulogic_vector(msg, normalized_user);
    send(net, axi_stream.p_actor, msg);
  end;

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_master_t;
                            tdata : std_logic_vector;
                            tlast : boolean;
                            tkeep : std_logic_vector;
                            tuser : std_logic_vector) is
  begin
    if (tlast) then
      push_axi_stream(net, axi_stream, tdata, '1', tkeep, tuser);
    else
      push_axi_stream(net, axi_stream, tdata, '0', tkeep, tuser);
    end if;
  end;

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_master_t;
                            tdata : std_logic_vector;
                            tlast : std_logic;
                            tkeep : std_logic_vector) is
    constant normalized_user : std_logic_vector(user_length(axi_stream)-1 downto 0) := (others => '0');
  begin
    push_axi_stream(net, axi_stream, tdata, tlast, tkeep, normalized_user);
  end;

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_master_t;
                            tdata : std_logic_vector;
                            tlast : boolean;
                            tkeep : std_logic_vector) is
  begin
    if (tlast) then
      push_axi_stream(net, axi_stream, tdata, '1', tkeep);
    else
      push_axi_stream(net, axi_stream, tdata, '0', tkeep);
    end if;
  end;

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_master_t;
                            tdata : std_logic_vector;
                            tlast : std_logic) is
    variable normalized_keep : std_logic_vector(data_length(axi_stream)/8-1 downto 0) := (others => '1');
  begin
    push_axi_stream(net, axi_stream, tdata, tlast, normalized_keep);
  end;

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_master_t;
                            tdata : std_logic_vector;
                            tlast : boolean) is
  begin
    if (tlast) then
      push_axi_stream(net, axi_stream, tdata, '1');
    else
      push_axi_stream(net, axi_stream, tdata, '0');
    end if;
  end;

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream : axi_stream_master_t;
                            tdata : std_logic_vector) is
  begin
    push_axi_stream(net, axi_stream, tdata, '1');
  end;

  procedure await_pop_axi_stream_reply(signal net : inout network_t;
                                      variable reference : inout stream_reference_t;
                                      variable tdata : out std_logic_vector;
                                      variable tlast : out std_logic;
                                      variable tkeep : out std_logic_vector;
                                      variable tuser : out std_logic_vector) is
    variable reply_msg : msg_t;
  begin
    receive_reply(net, reference, reply_msg);
    tdata := pop_std_ulogic_vector(reply_msg);
    tlast := pop_std_ulogic(reply_msg);
    tkeep := pop_std_ulogic_vector(reply_msg);
    tuser := pop_std_ulogic_vector(reply_msg);
    delete(reference);
    delete(reply_msg);
  end;

  procedure await_pop_axi_stream_reply(signal net : inout network_t;
                                      variable reference : inout stream_reference_t;
                                      variable tdata : out std_logic_vector;
                                      variable tlast : out std_logic;
                                      variable tkeep : out std_logic_vector) is
    variable reply_msg : msg_t;
  begin
    receive_reply(net, reference, reply_msg);
    tdata := pop_std_ulogic_vector(reply_msg);
    tlast := pop_std_ulogic(reply_msg);
    tkeep := pop_std_ulogic_vector(reply_msg);
    delete(reference);
    delete(reply_msg);
  end;

  procedure await_pop_axi_stream_reply(signal net : inout network_t;
                                      variable reference : inout stream_reference_t;
                                      variable tdata : out std_logic_vector;
                                      variable tlast : out std_logic) is
    variable reply_msg : msg_t;
  begin
    receive_reply(net, reference, reply_msg);
    tdata := pop_std_ulogic_vector(reply_msg);
    tlast := pop_std_ulogic(reply_msg);
    delete(reference);
    delete(reply_msg);
  end;

  procedure await_pop_axi_stream_reply(signal net : inout network_t;
                                      variable reference : inout stream_reference_t;
                                      variable tdata : out std_logic_vector) is
    variable reply_msg : msg_t;
  begin
    receive_reply(net, reference, reply_msg);
    tdata := pop_std_ulogic_vector(reply_msg);
    delete(reference);
    delete(reply_msg);
  end;

  procedure pop_axi_stream(signal net : inout network_t;
                           axi_stream : axi_stream_slave_t;
                           variable reference : inout stream_reference_t) is
  begin
    reference := new_msg(pop_axi_stream_msg);
    send(net, axi_stream.p_actor, reference);
  end;

  procedure pop_axi_stream(signal net : inout network_t;
                          axi_stream : axi_stream_slave_t;
                          variable tdata : out std_logic_vector;
                          variable tlast : out std_logic;
                          variable tkeep : out std_logic_vector;
                          variable tuser : out std_logic_vector) is
    variable reference : stream_reference_t;
  begin
    pop_axi_stream(net, axi_stream, reference);
    await_pop_axi_stream_reply(net, reference, tdata, tlast, tkeep, tuser);
  end;

  procedure pop_axi_stream(signal net : inout network_t;
                          axi_stream : axi_stream_slave_t;
                          variable tdata : out std_logic_vector;
                          variable tlast : out std_logic;
                          variable tkeep : out std_logic_vector) is
    variable reference : stream_reference_t;
  begin
    pop_axi_stream(net, axi_stream, reference);
    await_pop_axi_stream_reply(net, reference, tdata, tlast, tkeep);
  end;

  procedure pop_axi_stream(signal net : inout network_t;
                          axi_stream : axi_stream_slave_t;
                          variable tdata : out std_logic_vector;
                          variable tlast : out std_logic) is
    variable reference : stream_reference_t;
  begin
    pop_axi_stream(net, axi_stream, reference);
    await_pop_axi_stream_reply(net, reference, tdata, tlast);
  end;

  procedure pop_axi_stream(signal net : inout network_t;
                          axi_stream : axi_stream_slave_t;
                          variable tdata : out std_logic_vector) is
    variable reference : stream_reference_t;
  begin
    pop_axi_stream(net, axi_stream, reference);
    await_pop_axi_stream_reply(net, reference, tdata);
  end;

end package body;
