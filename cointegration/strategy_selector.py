#! /usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = "maxim"


import csv
import imp
import numpy

bootstrap = imp.load_source("bootstrap", "stationary_block_bootstrap.py")


TRANSACTION_COST = 0.02
SLIPPAGE = 1 - TRANSACTION_COST


def time_series_boot(series):
    # The choice of p is same as R's tsbootstrap
    # See http://www.inside-r.org/packages/cran/tseries/docs/tsbootstrap
    const = 3.15
    n = len(series)
    p = 1 / (const * (n ** (1.0 / 3)))
    resample, _1, _2 = bootstrap.resample(series, p)
    return resample


def calc_profit(series, strategy):
    eps = strategy[0]
    delta = strategy[1]

    profit = 0
    cur_position = None
    prev = None
    for item in series:
        if cur_position is not None:
            if abs(item) < eps or numpy.sign(item) != numpy.sign(prev):
                profit += abs(item - cur_position) * SLIPPAGE
                cur_position = None

        if cur_position is None:
            if abs(item) > delta:
                cur_position = item

        prev = item

    return profit


def bootstrap_profit(spread, strategy, bootstraps=None, num=None):
    numpy.random.seed(0)

    if bootstraps is None:
        assert num is not None
        bootstraps = [time_series_boot(spread) for i in xrange(0, num)]

    profits = []
    for resample in bootstraps:
        profit = calc_profit(resample, strategy)
        profits.append(profit)

    return numpy.mean(profits)


def next_generation(strategy, limit):
    numpy.random.seed()
    eps_sd = limit / 10.0
    delta_sd = limit / 3.0
    eps_mutation, delta_mutation = numpy.random.standard_normal(2)

    eps = strategy[0]
    delta = strategy[1]
    eps2 = min(max(0, eps + eps_mutation * eps_sd), limit)
    delta2 = min(max(0, delta + delta_mutation * delta_sd), limit)

    generation = [(eps, delta2), (eps2, delta), (eps2, delta2)]
    ordered_generation = [(min(eps, delta), max(eps, delta)) for eps, delta in generation]

    return ordered_generation


def guess_best_strategy(spread, gen_num=100, boot_num=200):
    mean = numpy.mean(spread)
    stdev = numpy.std(spread)
    spread = [element - mean for element in spread]

    bootstraps = [spread] if boot_num <= 1 else [time_series_boot(spread) for i in xrange(0, boot_num)]

    current_strategy = (0, stdev)
    current_profit = bootstrap_profit(spread, current_strategy, bootstraps)

    limit = max(spread)
    for i in xrange(0, gen_num):
        max_profit = current_profit
        max_strategy = current_strategy

        for mutated_strategy in next_generation(current_strategy, limit):
            profit = bootstrap_profit(spread, mutated_strategy, bootstraps)
            if profit > max_profit:
                max_profit = profit
                max_strategy = mutated_strategy

        current_profit = max_profit
        current_strategy = max_strategy

    return current_profit, current_strategy


def get_spreads(file_name):
    spreads = []
    with open(file_name, "rb") as file_:
        reader = csv.reader(file_, delimiter=',')
        header = False
        for row in reader:
            data = row[1:]
            if not header:
                for key in data:
                    spreads.append((key, []))
                header = True
            else:
                for i in xrange(0, len(spreads)):
                    value = data[i]
                    spread = spreads[i]
                    spread[1].append(float(value))
    return spreads


def main():
    pairs = get_spreads("result-spreads.csv")

    results = []
    for pair in pairs:
        name = pair[0]
        spread = pair[1]
        print "Pair: ", name

        profit, strategy = guess_best_strategy(spread, gen_num=200, boot_num=100)
        print "Best return=%0.1f%%, strategy: OPEN at %0.8f, CLOSE at %0.8f" % (profit * 100, strategy[1], strategy[0])
        print

        results.append((name, profit, "%0.10f" % strategy[0], "%0.10f" % strategy[1]))

    # Sort and save results.
    results = sorted(results, key=lambda tup: -tup[1])
    with open("result-strategies.csv", "wb") as file_:
        writer = csv.writer(file_, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)
        writer.writerow(["Pair", "Profit", "Eps", "Delta"])
        for row in results:
            writer.writerow(row)


if __name__ == '__main__':
    main()
