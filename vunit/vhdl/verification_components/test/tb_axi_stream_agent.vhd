-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2018, Lars Asplund lars.anders.asplund@gmail.com

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library osvvm;
use osvvm.RandomPkg.all;

context work.vunit_context;
context work.com_context;
context work.data_types_context;
use work.axi_stream_pkg.all;
use work.stream_slave_pkg.all;
use work.axi_stream_agent_pkg.all;
--use work.stream_master_pkg.all;

entity tb_axi_stream_agent is
  generic (
  encoded_tb_cfg : string;
  runner_cfg : string);
end entity;

architecture a of tb_axi_stream_agent is

  type tb_cfg_t is record
    random_seed_g   : positive;
    axi_data_width_g: positive;
    axi_user_width_g: positive;
  end record tb_cfg_t;

  impure function decode(encoded_tb_cfg : string) return tb_cfg_t is
  begin
    return (random_seed_g    => positive'value(get(encoded_tb_cfg, "random_seed_g")),
            axi_data_width_g => positive'value(get(encoded_tb_cfg, "axi_data_width_g")),
            axi_user_width_g => positive'value(get(encoded_tb_cfg, "axi_user_width_g")));
  end function decode;

  constant tb_cfg : tb_cfg_t := decode(encoded_tb_cfg);

  constant master_axi_stream : axi_stream_master_t := new_axi_stream_master(data_length => tb_cfg.axi_data_width_g, user_length => tb_cfg.axi_user_width_g);
  constant master_axi_stream_agent : axi_stream_master_agent_t := new_axi_stream_master_agent(axi_stream_master => master_axi_stream,
                                                                                              data_length => tb_cfg.axi_data_width_g,
                                                                                              user_length => tb_cfg.axi_user_width_g);
  --constant master_stream : stream_master_t := as_stream(master_axi_stream);

  constant slave_axi_stream : axi_stream_slave_t := new_axi_stream_slave(data_length => tb_cfg.axi_data_width_g, user_length => tb_cfg.axi_user_width_g);
  --constant slave_stream : stream_slave_t := as_stream(slave_axi_stream);

  shared variable rnd_stimuli, rnd_expected, rnd_number : RandomPType;

  constant axi_data_width : natural := data_length(slave_axi_stream);
  constant axi_user_width : natural := user_length(slave_axi_stream);
  constant random_seed    : integer := tb_cfg.random_seed_g;

  signal aclk   : std_logic := '0';
  signal tvalid : std_logic;
  signal tready : std_logic;
  signal tdata  : std_logic_vector(data_length(slave_axi_stream)-1 downto 0);
  signal tlast  : std_logic;
  signal tkeep  : std_logic_vector(data_length(slave_axi_stream)/8-1 downto 0);
  signal tuser  : std_logic_vector(user_length(slave_axi_stream)-1 downto 0);

  impure function to_valid_bytes(byte_en : std_logic_vector) return natural is
    variable valid_byte_mask : std_logic_vector(byte_en'length * 8 - 1 downto 0) := (others=>'0');
    variable valid_bytes : natural := 0;
  begin
    for i in 0 to byte_en'length-1 loop
      if(byte_en(i) = '0') then
        exit;
      else
        valid_bytes := valid_bytes + 1;
      end if;
    end loop;
    check(valid_bytes /= 0, "Invalid tkeep value!, tkeep must be /= 0");
    return valid_bytes;
  end;

  impure function to_byte_mask(byte_en : std_logic_vector) return std_logic_vector is
    variable valid_byte_mask : std_logic_vector(byte_en'length * 8 - 1 downto 0) := (others=>'0');
    variable valid_bytes : natural := 0;
  begin
    valid_bytes := to_valid_bytes(byte_en);
    valid_byte_mask(8*valid_bytes-1 downto  0) := (others => '1');
    return valid_byte_mask;
  end;

begin

  main : process
    variable data : std_logic_vector(axi_data_width-1 downto 0);
    variable last : boolean; variable tlast : std_logic;
    variable keep : std_logic_vector(axi_data_width/8-1 downto 0);
    variable user : std_logic_vector(axi_user_width-1 downto 0);

    variable bytes_to_send : natural;
    variable number_of_full_transfers : natural;
    variable active_bytes_last_transfer : natural;
    variable counter :natural := 0;

    variable packet_to_send : integer := 10;
    variable packet_length  : integer := 10;

    variable reference_queue : queue_t := new_queue;
    variable reference : stream_reference_t;
  begin
    test_runner_setup(runner, runner_cfg);

    rnd_stimuli.InitSeed(random_seed);
    rnd_expected.InitSeed(random_seed);
    rnd_number.InitSeed(random_seed);

    wait for 100 ns;

    if run("Check parameter configuration") then
      check_equal((axi_data_width mod 8 ), 0, "axis stream width must be an integer multiple of 8");

    elsif run("test single axi push and axi pop") then
      push_axi_stream(net, master_axi_stream_agent, rnd_stimuli.RandSlv(axi_data_width), tlast => '1');
      pop_axi_stream(net, slave_axi_stream, data, tlast);
      check_equal(data, rnd_expected.RandSlv(axi_data_width), "pop stream data");
      check_equal(tlast, true, "pop stream last");

    elsif run("test multiple axi push and axi pop") then
      for i in 0 to 10 loop
        tlast := '1' when (i = 10) else '0';
        push_axi_stream(net, master_axi_stream_agent, rnd_stimuli.RandSlv(axi_data_width), tlast);
        pop_axi_stream(net, slave_axi_stream, data, tlast);
        check_equal(data, rnd_expected.RandSlv(axi_data_width), "pop stream data");
        if ( i=10 ) then
          check_equal(tlast, true, "pop stream last");
        else
          check_equal(tlast, false, "pop stream last");
        end if;
      end loop;

    elsif run("test multiple axi push and axi pop - NO DELAY") then

      config_axi_stream(net, master_axi_stream_agent, to_operation_mode_t("NO_DELAY"));

      for i in 0 to 10 loop
        tlast := '1' when (i = 10) else '0';
        push_axi_stream(net, master_axi_stream_agent, rnd_stimuli.RandSlv(axi_data_width), tlast);

        pop_axi_stream(net, slave_axi_stream, data, tlast);
        check_equal(data, rnd_expected.RandSlv(axi_data_width), "pop stream data");
        if ( i=10 ) then
          check_equal(tlast, true, "pop stream last");
        else
          check_equal(tlast, false, "pop stream last");
        end if;
      end loop;

    elsif run("test multiple axi push and axi pop - DELAY_BETWEEN_BEATS") then
      config_axi_stream(net, master_axi_stream_agent, to_operation_mode_t("DELAY_BETWEEN_BEATS"));

      for i in 0 to 10 loop
        tlast := '1' when (i = 10) else '0';
        push_axi_stream(net, master_axi_stream_agent, rnd_stimuli.RandSlv(axi_data_width), tlast);
      end loop;

      for i in 0 to 10 loop
        pop_axi_stream(net, slave_axi_stream, data, tlast);
        check_equal(data, rnd_expected.RandSlv(axi_data_width), "pop stream data");
        if ( i=10 ) then
          check_equal(tlast, true, "pop stream last");
        else
          check_equal(tlast, false, "pop stream last");
        end if;
      end loop;

    elsif run("test multiple axi push and axi pop - DELAY_BETWEEN_PACKETS") then
      packet_to_send := 10;
      packet_length  := 10;

      config_axi_stream(net, master_axi_stream_agent, to_operation_mode_t("DELAY_BETWEEN_PACKETS"));

      for j in 0 to packet_to_send loop
        for i in 0 to packet_length loop
          tlast := '1' when (i = packet_length) else '0';
          push_axi_stream(net, master_axi_stream_agent, rnd_stimuli.RandSlv(axi_data_width), tlast);
        end loop;
      end loop;

      for packet_to_send in 0 to 10 loop
        while (tlast /= '1') loop
          pop_axi_stream(net, slave_axi_stream, data, tlast);
          check_equal(data, rnd_expected.RandSlv(axi_data_width), "pop stream data");
        end loop;
        tlast := '0';
      end loop;
    end if;

    wait for 100 ns;
    test_runner_cleanup(runner);
  end process;

  test_runner_watchdog(runner, 1 ms);

  axi_stream_master_agent_inst : entity work.axi_stream_master_agent
    generic map (
      master => master_axi_stream_agent)
    port map (
      aclk   => aclk,
      tvalid => tvalid,
      tready => tready,
      tdata  => tdata,
      tlast  => tlast,
      tkeep  => tkeep,
      tuser  => tuser);

  axi_stream_slave_inst : entity work.axi_stream_slave
    generic map (
      slave => slave_axi_stream)
    port map (
      aclk   => aclk,
      tvalid => tvalid,
      tready => tready,
      tdata  => tdata,
      tlast  => tlast,
      tkeep  => tkeep,
      tuser  => tuser);

  aclk <= not aclk after 5 ns;
end architecture;
