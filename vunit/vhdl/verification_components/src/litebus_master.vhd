-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2018, Lars Asplund lars.anders.asplund@gmail.com


library ieee;
use ieee.std_logic_1164.all;

use work.queue_pkg.all;
use work.bus_master_pkg.all;
use work.sync_pkg.all;
context work.com_context;
context work.vunit_context;

entity litebus_master is
  generic (
    bus_handle : bus_master_t
    );
  port (
    clk    : in  std_logic;
    rst    : in  std_logic;

    addr   : out std_logic_vector ( address_length(bus_handle) - 1 downto 0 );
    re     : out std_logic := '0';
    we     : out std_logic := '0';
    be     : out std_logic_vector ( byte_enable_length(bus_handle)-1 downto 0 );
    data_i : in  std_logic_vector ( data_length(bus_handle)-1 downto 0 );
    data_o : out std_logic_vector ( data_length(bus_handle)-1 downto 0 );
    ack    : in  std_logic;
    nack   : in  std_logic
);
end entity;

architecture a of litebus_master is
begin

    main : process
        variable request_msg, reply_msg : msg_t;
        variable msg_type : msg_type_t;
        variable w_done, aw_done : boolean;
    begin
        receive(net, bus_handle.p_actor, request_msg);
        msg_type := message_type(request_msg);

        if msg_type = bus_read_msg then

            addr <= pop_std_ulogic_vector(request_msg);
            re <= '1';
            wait until rising_edge(clk);
            re <= '0';

            if( (ack or nack) = '0' ) then
                wait until (ack or nack) = '1' and rising_edge(clk);
            end if;

            if is_visible(bus_handle.p_logger, debug) then
                debug(bus_handle.p_logger, "Read 0x" & to_hstring(data_i) &" from address 0x" & to_hstring(addr));
            end if;

            reply_msg := new_msg;
            push_std_ulogic_vector(reply_msg, data_i);
            reply(net, request_msg, reply_msg);
            delete(request_msg);

        elsif msg_type = bus_write_msg then

            addr <= pop_std_ulogic_vector(request_msg);
            data_o <= pop_std_ulogic_vector(request_msg);
            be <= pop_std_ulogic_vector(request_msg);
            we <= '1';
            wait until rising_edge(clk);
            we <= '0';

            if( (ack or nack) = '0' ) then
                wait until (ack or nack) = '1' and rising_edge(clk);
            end if;

            if is_visible(bus_handle.p_logger, debug) then
                debug(bus_handle.p_logger,
                "Wrote 0x" & to_hstring(data_o) &
                " to address 0x" & to_hstring(data_o));
            end if;

        else
            unexpected_msg_type(msg_type);
        end if;
    end process;
end architecture;
