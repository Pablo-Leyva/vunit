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
use work.stream_master_pkg.all;
use work.stream_slave_pkg.all;

entity tb_axi_stream is
  generic (runner_cfg : string);
end entity;

architecture a of tb_axi_stream is
  constant AXI_DATA_WIDTH_C : natural := 32;
  constant AXI_USER_WIDTH_C : natural := 16;
  constant master_axi_stream : axi_stream_master_t := new_axi_stream_master(data_length => AXI_DATA_WIDTH_C, user_length => AXI_USER_WIDTH_C);
  constant master_stream : stream_master_t := as_stream(master_axi_stream);

  constant slave_axi_stream : axi_stream_slave_t := new_axi_stream_slave(data_length => AXI_DATA_WIDTH_C, user_length => AXI_USER_WIDTH_C);
  constant slave_stream : stream_slave_t := as_stream(slave_axi_stream);

  shared variable rnd_stimuli, rnd_expected, rnd_number : RandomPType;

  constant axi_data_width : natural := data_length(slave_axi_stream);
  constant axi_user_width : natural := user_length(slave_axi_stream);

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

    variable reference_queue : queue_t := new_queue;
    variable reference : stream_reference_t;
  begin
    test_runner_setup(runner, runner_cfg);

    rnd_stimuli.InitSeed(rnd_stimuli'instance_name);
    rnd_expected.InitSeed(rnd_stimuli'instance_name);
    rnd_number.InitSeed(rnd_number'instance_name);

    if run("Check parameter configuration") then
      check_equal((axi_data_width mod 8 ), 0, "axis stream width must be an integer multiple of 8");

    elsif run("test single push and pop") then
      push_stream(net, master_stream, rnd_stimuli.RandSlv(axi_data_width));
      pop_stream(net, slave_stream, data);
      check_equal(data, rnd_expected.RandSlv(axi_data_width), "pop stream data");

    elsif run("test single push and pop with tlast") then
      push_stream(net, master_stream, rnd_stimuli.RandSlv(axi_data_width), true);
      pop_stream(net, slave_stream, data, last);
      check_equal(data, rnd_expected.RandSlv(axi_data_width), "pop stream data");
      check_equal(last, true, "pop stream last");

    elsif run("test single axi push and pop") then
      push_axi_stream(net, master_axi_stream, rnd_stimuli.RandSlv(axi_data_width));--, tlast => '1');
      pop_stream(net, slave_stream, data);--, last);
      check_equal(data, rnd_expected.RandSlv(axi_data_width), "pop stream data");
      --check_equal(last, true, "pop stream last");

    elsif run("test single axi push and axi pop") then
      push_axi_stream(net, master_axi_stream, rnd_stimuli.RandSlv(axi_data_width), tlast => '1');
      pop_axi_stream(net, slave_axi_stream, data, tlast);
      check_equal(data, rnd_expected.RandSlv(axi_data_width), "pop stream data");
      check_equal(tlast, true, "pop stream last");

    elsif run("test pop before push") then
      for i in 0 to 7 loop
        pop_stream(net, slave_stream, reference);
        push(reference_queue, reference);
      end loop;

      for i in 0 to 7 loop
        push_stream(net, master_stream,
                    std_logic_vector(to_unsigned(i+1, data'length)));
      end loop;

      for i in 0 to 7 loop
        reference := pop(reference_queue);
        await_pop_stream_reply(net, reference, data);
        check_equal(data, to_unsigned(i+1, data'length));
      end loop;

    elsif run("test multiple axi push and axi pop with tlast") then
      bytes_to_send := rnd_number.RandInt(1,512);
      number_of_full_transfers   := bytes_to_send/(axi_data_width/8);
      active_bytes_last_transfer := bytes_to_send mod (axi_data_width/8);

      tlast := '0';
      keep := (others => '1');
      info("Sending axi stream packet... "& to_string(bytes_to_send) & " Bytes");

      for i in 1 to number_of_full_transfers-1 loop
        push_axi_stream(net, master_axi_stream, rnd_stimuli.RandSlv(axi_data_width) and to_byte_mask(keep), tlast, keep);
        counter := counter + to_valid_bytes(keep);
      end loop;

      if (active_bytes_last_transfer = 0) then
        tlast := '1';
        push_axi_stream(net, master_axi_stream, rnd_stimuli.RandSlv(axi_data_width) and to_byte_mask(keep), tlast, keep);
        counter := counter + to_valid_bytes(keep);
      else
        tlast := '0';
        push_axi_stream(net, master_axi_stream, rnd_stimuli.RandSlv(axi_data_width) and to_byte_mask(keep), tlast, keep);
        counter := counter + to_valid_bytes(keep);
        tlast := '1';
        keep  := (others => '0');
        keep(active_bytes_last_transfer-1 downto 0) := (others => '1');
        push_axi_stream(net, master_axi_stream, rnd_stimuli.RandSlv(axi_data_width) and to_byte_mask(keep), tlast, keep);
        counter := counter + to_valid_bytes(keep);
      end if;

      info("Reading axi stream packet... "& to_string(counter) & " Bytes");
      tlast := '0';
      while (tlast /= '1') loop
        pop_axi_stream(net, slave_axi_stream, data, tlast, keep);
        check_equal(data, rnd_expected.RandSlv(axi_data_width) and to_byte_mask(keep), "tdata axi pop stream data");
        counter := counter - to_valid_bytes(keep);
      end loop;
      check_equal(counter, 0, "Different read/write counter");

    elsif run("test multiple axi push and axi pop with tlast tkeep and tuser") then
      bytes_to_send := rnd_number.RandInt(1,512);
      number_of_full_transfers   := bytes_to_send/(axi_data_width/8);
      active_bytes_last_transfer := bytes_to_send mod (axi_data_width/8);

      tlast := '0';
      keep := (others => '1');
      info("Sending axi stream packet... "& to_string(bytes_to_send) & " Bytes");

      for i in 1 to number_of_full_transfers-1 loop
        push_axi_stream(net, master_axi_stream, rnd_stimuli.RandSlv(axi_data_width) and to_byte_mask(keep), tlast, keep, rnd_stimuli.RandSlv(axi_user_width));
        counter := counter + to_valid_bytes(keep);
      end loop;

      if (active_bytes_last_transfer = 0) then
        tlast := '1';
        push_axi_stream(net, master_axi_stream, rnd_stimuli.RandSlv(axi_data_width) and to_byte_mask(keep), tlast, keep, rnd_stimuli.RandSlv(axi_user_width));
        counter := counter + to_valid_bytes(keep);
      else
        tlast := '0';
        push_axi_stream(net, master_axi_stream, rnd_stimuli.RandSlv(axi_data_width) and to_byte_mask(keep), tlast, keep, rnd_stimuli.RandSlv(axi_user_width));
        counter := counter + to_valid_bytes(keep);
        tlast := '1';
        keep  := (others => '0');
        keep(active_bytes_last_transfer-1 downto 0) := (others => '1');
        push_axi_stream(net, master_axi_stream, rnd_stimuli.RandSlv(axi_data_width) and to_byte_mask(keep), tlast, keep, rnd_stimuli.RandSlv(axi_user_width));
        counter := counter + to_valid_bytes(keep);
      end if;

      info("Reading axi stream packet... "& to_string(counter) & " Bytes");
      tlast := '0';
      while (tlast /= '1') loop
        pop_axi_stream(net, slave_axi_stream, data, tlast, keep, user);
        check_equal(data, rnd_expected.RandSlv(axi_data_width) and to_byte_mask(keep), "tdata axi pop stream data");
        check_equal(user, rnd_expected.RandSlv(axi_user_width), "tuser axi pop stream data");
        counter := counter - to_valid_bytes(keep);
      end loop;
--
      check_equal(counter, 0, "Different read/write counter");

    end if;
    test_runner_cleanup(runner);
  end process;

  test_runner_watchdog(runner, 10 ms);

  axi_stream_master_inst : entity work.axi_stream_master
    generic map (
      master => master_axi_stream)
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
