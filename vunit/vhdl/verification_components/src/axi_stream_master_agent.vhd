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
use work.axi_stream_pkg.all;
use work.axi_stream_agent_pkg.all;
use work.queue_pkg.all;
use work.sync_pkg.all;

entity axi_stream_master_agent is
  generic (
    master : axi_stream_master_agent_t);
  port (
    aclk : in std_logic;
    tvalid : out std_logic := '0';
    tready : in std_logic := '0';
    tdata : out std_logic_vector(data_length(master)-1 downto 0) := (others => '0');
    tlast : out std_logic := '0';
    tkeep : out std_logic_vector(data_length(master)/8-1 downto 0) := (others => '1');
    tuser : out std_logic_vector(user_length(master)-1 downto 0) := (others => '0'));
end entity;

architecture behavioural of axi_stream_master_agent is

--  constant p_actor : actor_t := new_actor("axi stream agent driver");

  shared variable pop_new_msg : boolean := True;
  shared variable configuration_active : operation_mode_t := NO_DELAY;
begin

  p_msg_decoding : process
    variable msg      : msg_t;
    variable msg_type : msg_type_t;

    variable valid_bytes    : integer := 0;
    variable operation_mode : operation_mode_t := NO_DELAY;
    variable operation_string : string(1 to 256);

    variable vdata : std_logic_vector(data_length(get_axi_stream(master))-1   downto 0) := (others => '0');
    variable vlast : std_logic                                          := '0';
    variable vkeep : std_logic_vector(data_length(get_axi_stream(master))/8-1 downto 0) := (others => '0');

  begin
      while (True) loop
          info("Waiting for pop_new_msg = True");
          --wait until (pop_new_msg = true);
          info("Waiting for agent msg");
          receive(net, master.p_actor, msg);
          info("Decoding msg");
          msg_type := message_type(msg);
          handle_sync_message(net, msg_type, msg);

          if msg_type = config_axi_stream_msg then
            info("CONFIG MSG");
          --case( msg_type ) is
              --when config_axi_stream_msg =>
                  operation_string := pop_string(msg);
                  operation_mode   := to_operation_mode_t(operation_string);
                  case( operation_mode ) is
                      when NO_DELAY              =>
                          configuration_active := NO_DELAY;
                      when DELAY_BETWEEN_PACKETS =>
                          configuration_active := DELAY_BETWEEN_PACKETS;
                          --TODO: RANDOM NUMBER CALCULATIONS!
                      when DELAY_BETWEEN_BEATS   =>
                          configuration_active := DELAY_BETWEEN_BEATS;
                          --TODO: RANDOM NUMBER CALCULATIONS!
                      when others =>
                          -- TODO: ERROR!
                  end case;
          elsif msg_type = push_axi_stream_msg or msg_type = stream_push_msg then
            info("push_axi_stream_msg OR stream_push_msg MSG");
              --when push_axi_stream_msg or stream_push_msg =>
                  vdata(valid_bytes*8 + 7 downto valid_bytes*8) := pop_std_ulogic_vector(msg);
                  vlast := pop_std_ulogic(msg);
                  valid_bytes := valid_bytes + 1;
                  if(vlast = '1' or valid_bytes = data_length(get_axi_stream(master))/8) then
                      vkeep := (valid_bytes-1 downto 0 => '1');
                      --msg := new_msg(push_axi_stream_msg);
                      info("pushing data to BFM");
                      push_axi_stream(net, master.p_axi_stream_master, vdata, vlast, vkeep);
                      info("pushed data to BFM");
                  end if;
                  valid_bytes := 0;
                  vlast       := '0';
                  vkeep       := (others => '0');
          else
              --when others =>
                  unexpected_msg_type(msg_type);
          end if;
      end loop;
  end process;

  p_transfer_delay : process
  begin
      while (True) loop
          case( configuration_active ) is
              when NO_DELAY =>
                  pop_new_msg := True;
                  wait until configuration_active /= NO_DELAY;
              when DELAY_BETWEEN_PACKETS =>
                  wait until (tvalid = '1' and tready = '1' and tlast = '1');
                  pop_new_msg := False;
                  --wait for PACKET_DELAY;
                  wait for 100 ns;
                  pop_new_msg := True;
              when DELAY_BETWEEN_BEATS =>
                  wait until (tvalid = '1' and tready = '1');
                  pop_new_msg := False;
                  --wait for BEAT_DELAY;
                  wait for 50 ns;
                  pop_new_msg := True;
              when others =>
                  --TODO: ERROR!
          end case;
      end loop;
  end process;

end architecture;
