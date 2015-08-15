#! /usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = "maxim"


import csv
import itertools


def read_csv_rows(file_name, callback=lambda row: row):
    with open(file_name, "rb") as file_:
        reader = csv.reader(file_, delimiter=',')
        return [callback(row) for row in itertools.islice(reader, 1, None)]


def export_to_py(data):
    print "DATA = ["
    print "  # symb1, symb2, coeff1, coeff2, spread mean, eps, delta, cost"
    for row in data:
        print "  ('%s', '%s', %2s, %2s, %s, %s, %s, %s)," % \
              tuple([row[i] for i in [4, 5, 8, 7, 13, 2, 3, 10]])
    print "]"


def main():
    pairs_data = read_csv_rows("result-tradable-pairs.csv", callback=lambda row: row[1:])
    strategies = read_csv_rows("result-strategies.csv")
    index = { ".".join(pair[0:2]): pair for pair in pairs_data }
    merged = [strategy + index.get(strategy[0]) for strategy in itertools.islice(strategies, 0, 5)]
    export_to_py(merged)


if __name__ == '__main__':
    main()
