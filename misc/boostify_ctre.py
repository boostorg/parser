#!/usr/bin/python3

# Copyright (c) 2024 T. Zachary Laine
#
# Distributed under the Boost Software License, Version 1.0. (See accompanying
# file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

import argparse
import os

parser = argparse.ArgumentParser(description='Adapts the single-header form of the CTRE library for use as a header in Boost; rewrites this given file in place.')
parser.add_argument('source', type=str, help='The CTRE header to adapt.')
args = parser.parse_args()

lines = open(args.source, 'r').readlines()

tmp_file_name = 'ctre_header.temp.hpp'

tmp_file = open(tmp_file_name, 'w')

tmp_file.write('''// Copyright (C) 2024 Hana Dusíková, T. Zachary Laine
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

// Original work by Hana Dusíková; modified for Boost macro and namespace
// conventions by T. Zachary Laine.

''')

for line in lines:
    line = line
    line = line.replace('CTRE', 'BOOST_PARSER_CTRE')
    line = line.replace('CTLL', 'BOOST_PARSER_CTLL')
    namespaces = ['ctll', 'ctre', 'literals', 'test_literals', 'uni', 'uni::detail']
    for ns in namespaces:
        if line.startswith('namespace'):
            line = line.replace(f'namespace {ns}', f'namespace boost::parser::{ns}')
    tmp_file.write(line)
    #print(line[:-1])

os.rename(tmp_file_name, args.source)
