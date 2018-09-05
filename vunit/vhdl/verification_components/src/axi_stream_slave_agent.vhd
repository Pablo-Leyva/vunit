-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2018, Lars Asplund lars.anders.asplund@gmail.com

library ieee;
use ieee.std_logic_1164.all;

context work.vunit_context;
context work.com_context;
use work.stream_slave_pkg.all;
use work.axi_stream_pkg.all;
use work.axi_stream_agent_pkg.all;
use work.queue_pkg.all;
use work.sync_pkg.all;

entity axi_stream_slave_agent is
  generic (
    slave : axi_stream_slave_agent_t);
  port (
    aclk : in std_logic;
    tvalid : in std_logic := '0';
    tready : out std_logic := '0';
    tdata : in std_logic_vector(data_length(slave)-1 downto 0) := (others => '0');
    tlast : in std_logic := '1';
    tkeep : in std_logic_vector(data_length(slave)/8-1 downto 0) := (others => '1');
    tuser : in std_logic_vector(user_length(slave)-1 downto 0) := (others => '0'));
end entity;

architecture behavioural of axi_stream_slave_agent is

  shared variable PACKET_DELAY : integer := 0;
  shared variable BEAT_DELAY   : integer := 0;

  signal pop_new_msg : boolean := True;
  signal configuration_active : operation_mode_t := NO_DELAY;

begin
  p_msg_decoding : process
    variable reply_msg, msg, copy_msg : msg_t;
    variable msg_type : msg_type_t;

    variable operation_mode : operation_mode_t := NO_DELAY;

  begin

    if(not pop_new_msg) then wait until (pop_new_msg); end if;

    receive(net, slave.p_actor, msg);
    msg_type := message_type(msg);

    if msg_type = config_axi_stream_msg then
      operation_mode   := to_operation_mode_t(pop_string(msg));
      case( operation_mode ) is
        when NO_DELAY              =>
             configuration_active <= NO_DELAY;

        when DELAY_BETWEEN_PACKETS =>
             configuration_active <= DELAY_BETWEEN_PACKETS;
             --TODO: RANDOM NUMBER CALCULATIONS!
             PACKET_DELAY := 125;

        when DELAY_BETWEEN_BEATS   =>
             configuration_active <= DELAY_BETWEEN_BEATS;
             --TODO: RANDOM NUMBER CALCULATIONS!
             BEAT_DELAY := 50;

        when INVALID   =>
             warning("Invalid operation mode selected!, selecting default config NO_DELAY");
             configuration_active <= NO_DELAY;

        when others =>
             error("Unknown operation mode, this error should NEVER appear");
             configuration_active <= NO_DELAY;
      end case;

    elsif msg_type = stream_pop_msg or msg_type = pop_axi_stream_msg then
      copy_msg := copy(msg);

      send(net, slave.p_axi_stream_slave.p_actor, msg);
      receive_reply(net, msg, reply_msg);
      reply(net, copy_msg, reply_msg);
    else
      unexpected_msg_type(msg_type);
    end if;
  end process;

  p_transfer_delay : process
  begin
      case( configuration_active ) is
          when NO_DELAY =>
              pop_new_msg <= True;
              wait until (configuration_active /= NO_DELAY);

          when DELAY_BETWEEN_PACKETS =>
              wait until (tvalid = '1' and tready = '1' and tlast = '1');
              pop_new_msg <= False;
              wait for PACKET_DELAY * 1 ns;
              pop_new_msg <= True;

          when DELAY_BETWEEN_BEATS =>
              wait until (tvalid = '1' and tready = '1');
              pop_new_msg <= False;
              wait for BEAT_DELAY * 1 ns;
              pop_new_msg <= True;

          when others =>
              error("Unknown operation mode, this error should NEVER appear");
      end case;
  end process;

  axi_stream_slave_inst : entity work.axi_stream_slave
    generic map (
      slave => slave.p_axi_stream_slave)
    port map (
      aclk   => aclk,
      tvalid => tvalid,
      tready => tready,
      tdata  => tdata,
      tlast  => tlast,
      tkeep  => tkeep,
      tuser  => tuser);

end architecture;
