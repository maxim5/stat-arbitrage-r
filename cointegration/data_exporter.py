#! /usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = "maxim"


import csv
import itertools


MAX_PAIRS = 50


def read_csv_rows(file_name, callback=lambda row: row):
    with open(file_name, "rb") as file_:
        reader = csv.reader(file_, delimiter=',')
        return [callback(row) for row in itertools.islice(reader, 1, None)]


def export_to_py(data):
    print "context.pairs = ("
    for row in data:
        print "    Pair(symbols=symbols('%s', '%s'), gamma=%.6f, mean=%.6f, sd=%.6f, delta=%.6f, eps=0)," % \
              tuple([row[i] for i in [0, 1]] + [float(row[i]) for i in [2, 5, 6, 6]])
    print ")"


def main():
    pairs_data = read_csv_rows("result-cointegrated-pairs.csv", callback=lambda row: row[1:])
    export_to_py(itertools.islice(pairs_data, 0, MAX_PAIRS))


if __name__ == '__main__':
    main()
