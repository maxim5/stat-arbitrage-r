#! /usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = "maxim"


import csv
import itertools


START = 0
STOP = 100


def read_csv_rows(file_name, callback=lambda row: row):
    with open(file_name, "rb") as file_:
        reader = csv.reader(file_, delimiter=',')
        return [callback(row) for row in itertools.islice(reader, 1, None)]


def export_to_py(data):
    def write(indent, s):
        indent_str = "    " * indent
        print indent_str + s

    write(1, "pairs = (")
    for row in data:
        symbols = tuple([row[i] for i in [0, 1]])
        gamma, mean, sd = tuple(float(row[i]) for i in [2, 5, 6])
        delta = eps = 2 * sd
        write(2, "Pair(symbols=symbols('%s', '%s'), gamma=%.6f, mean=%.6f, sd=%.4f, delta=%.2f, eps=-%.2f, "
                 "max_shares=30, max_spread_cost=100)," % (symbols + (gamma, mean, sd, delta, eps)))
    write(1, ")")


def main():
    pairs_data = read_csv_rows("result-cointegrated-pairs.csv", callback=lambda row: row[1:])
    export_to_py(itertools.islice(pairs_data, START, STOP))


if __name__ == '__main__':
    main()
