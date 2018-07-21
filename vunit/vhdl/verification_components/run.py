# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Copyright (c) 2014-2018, Lars Asplund lars.anders.asplund@gmail.com

from os.path import join, dirname
from vunit import VUnit
from itertools import product

from random import randint

root = dirname(__file__)

ui = VUnit.from_argv()
ui.add_random()
ui.add_verification_components()
lib = ui.library("vunit_lib")
lib.add_source_files(join(root, "test", "*.vhd"))


def encode(tb_cfg):
    return ",".join(["%s:%s" % (key, str(tb_cfg[key])) for key in tb_cfg])


def gen_wb_tests(obj, dat_width, num_cycles, ack_prob, stall_prob):
    for dat_width, num_cycles, ack_prob, stall_prob in product(dat_width, num_cycles, ack_prob, stall_prob):
        tb_cfg = dict(
            dat_width=dat_width,
            adr_width=32,
            ack_prob=ack_prob,
            stall_prob=stall_prob,
            num_cycles=num_cycles)
        config_name = encode(tb_cfg)
        obj.add_config(name=config_name,
                       generics=dict(encoded_tb_cfg=encode(tb_cfg)))


tb_wishbone_slave = lib.test_bench("tb_wishbone_slave")

for test in tb_wishbone_slave.get_tests():
    gen_wb_tests(test, [8, 32], [1, 64], [0.3, 1.0], [0.4, 0.0])


tb_wishbone_master = lib.test_bench("tb_wishbone_master")

for test in tb_wishbone_master.get_tests():
    gen_wb_tests(test, [8, 32], [1, 64], [0.3, 1.0], [0.4, 0.0])

tb_axi_stream = lib.test_bench("tb_axi_stream")

for test in tb_axi_stream.get_tests():
    axis_tb_cfg = dict(
        random_seed_g=(randint(1,1024)),
        axi_data_width_g=(randint(1, 32)*8),
        axi_user_width_g=(randint(1, 32)*8))
    test.add_config(name='bus_width',
                    generics=dict(encoded_tb_cfg=encode(axis_tb_cfg)))

ui.main()
