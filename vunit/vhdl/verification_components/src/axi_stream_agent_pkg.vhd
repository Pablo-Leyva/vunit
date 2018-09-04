-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2018, Lars Asplund lars.anders.asplund@gmail.com

library ieee;
use ieee.std_logic_1164.all;

use work.logger_pkg.all;
--use work.stream_master_pkg.all;
--use work.stream_slave_pkg.all;
--use work.sync_pkg.all;
use work.axi_stream_pkg.all;
context work.com_context;
context work.data_types_context;

package axi_stream_agent_pkg is

  type axi_stream_master_agent_t is record
    p_actor : actor_t;
    p_data_length : natural;
    p_user_length : natural;
    p_axi_stream_master : axi_stream_master_t;
  end record;

  type axi_stream_slave_agent_t is record
    p_actor : actor_t;
    p_data_length : natural;
    p_user_length : natural;
    p_axi_stream_slave : axi_stream_slave_t;
  end record;

  impure function new_axi_stream_master_agent(data_length : natural;
                                              axi_stream_master : axi_stream_master_t;
                                              actor : actor_t := null_actor;
                                              user_length : natural := 1) return axi_stream_master_agent_t;

  impure function data_length(master : axi_stream_master_agent_t) return natural;
  impure function user_length(master : axi_stream_master_agent_t) return natural;
  impure function get_axi_stream(master : axi_stream_master_agent_t) return axi_stream_master_t;

  constant config_axi_stream_msg : msg_type_t := new_msg_type("config axi stream");

  type operation_mode_t is (NO_DELAY,
                            DELAY_BETWEEN_PACKETS,
                            DELAY_BETWEEN_BEATS);

  pure function to_string(operation_mode : operation_mode_t) return string;
  pure function to_operation_mode_t(operation_string : string) return operation_mode_t;

  -- Reference to future stream result
  --alias stream_reference_t is msg_t;
  procedure config_axi_stream(signal net : inout network_t;
                              axi_stream_agent : axi_stream_master_agent_t;
                              operation_mode : operation_mode_t);

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream_agent : axi_stream_master_agent_t;
                            tdata : std_logic_vector;
                            tlast : std_logic);

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream_agent : axi_stream_master_agent_t;
                            tdata : std_logic_vector;
                            tlast : boolean);

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream_agent : axi_stream_master_agent_t;
                            tdata : std_logic_vector);

end package;

package body axi_stream_agent_pkg is

  pure function to_string(operation_mode : operation_mode_t) return string is
  begin
    case( operation_mode ) is
        when NO_DELAY =>
            return "NO_DELAY";
        when DELAY_BETWEEN_PACKETS =>
            return "DELAY_BETWEEN_PACKETS";
        when DELAY_BETWEEN_BEATS =>
            return "DELAY_BETWEEN_BEATS";
        when others =>
            return "INVALID_MODE";
    end case;
  end;

  pure function to_operation_mode_t(operation_string : string) return operation_mode_t is
    variable operation_mode : operation_mode_t := NO_DELAY;
  begin
    if (operation_string = "NO_DELAY") then
      operation_mode := NO_DELAY;
      --return operation_mode;
    elsif (operation_string = "DELAY_BETWEEN_PACKETS") then
      operation_mode := DELAY_BETWEEN_PACKETS;
      --return operation_mode;
    elsif (operation_string = "DELAY_BETWEEN_BEATS") then
      operation_mode := DELAY_BETWEEN_BEATS;
      --return operation_mode;
    else
      -- TODO: ERROR!
      operation_mode := NO_DELAY;
      --return operation_mode;
    end if;
    return operation_mode;
  end;

  impure function new_axi_stream_master_agent(data_length : natural;
                                              axi_stream_master : axi_stream_master_t;
                                              actor : actor_t := null_actor;
                                              user_length : natural := 1) return axi_stream_master_agent_t is
    variable p_actor : actor_t;
  begin
    p_actor := actor when actor /= null_actor else new_actor;

    return (p_actor => p_actor,
            p_user_length => user_length,
            p_data_length => data_length,
            p_axi_stream_master => axi_stream_master);
  end;

  impure function data_length(master : axi_stream_master_agent_t) return natural is
  begin
    return master.p_data_length;
  end;

  impure function user_length(master : axi_stream_master_agent_t) return natural is
  begin
    return master.p_user_length;
  end;

  impure function get_axi_stream(master : axi_stream_master_agent_t) return axi_stream_master_t is
  begin
    return master.p_axi_stream_master;
  end;

  -- PUSH PULL PROCEDURES / config
  procedure config_axi_stream(signal net : inout network_t;
                              axi_stream_agent : axi_stream_master_agent_t;
                              operation_mode : operation_mode_t) is
    variable msg : msg_t := new_msg(config_axi_stream_msg);
  begin
    push_string(msg, to_string(operation_mode));
    send(net, axi_stream_agent.p_actor, msg);
  end;

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream_agent : axi_stream_master_agent_t;
                            tdata : std_logic_vector;
                            tlast : std_logic) is
    variable msg : msg_t := new_msg(push_axi_stream_msg);
    constant normalized_data : std_logic_vector(data_length(axi_stream_agent)-1 downto 0) := tdata;
  begin
    info("Sending AXI Stream transfer - AGENT");
    push_std_ulogic_vector(msg, normalized_data);
    push_std_ulogic(msg, tlast);
    send(net, axi_stream_agent.p_actor, msg);
  end;

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream_agent : axi_stream_master_agent_t;
                            tdata : std_logic_vector;
                            tlast : boolean) is
  begin
    if (tlast) then
      push_axi_stream(net, axi_stream_agent, tdata, '1');
    else
      push_axi_stream(net, axi_stream_agent, tdata, '0');
    end if;
  end;

  procedure push_axi_stream(signal net : inout network_t;
                            axi_stream_agent : axi_stream_master_agent_t;
                            tdata : std_logic_vector) is
  begin
    push_axi_stream(net, axi_stream_agent, tdata, '1');
  end;

end package body;
