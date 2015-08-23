# -*- coding: utf-8 -*-
__author__ = "maxim"


import math
import numpy


########################################################################################################################
# Main
########################################################################################################################


def initialize(context):
    set_symbol_lookup_date("2014-01-01")
    context.pairs_trader = PairsTrader(context)


def handle_data(context, data):
    try:
        context.pairs_trader.handle(data)
    except Exception as e:
        log.error(e)


########################################################################################################################
# Trader
########################################################################################################################


# Order constants
ORDER_OPEN = 0
ORDER_FILLED = 1
ORDER_CANCELLED = 2


class PairsTrader:
    def __init__(self, context):
        self.context = context
        self._current = None
        self.pairs = (
            Pair(symbols=symbols('FDIS', 'FAD'), gamma=1.240485, mean=-1.452239, sd=0.012269, delta=0.02, eps=0),
        )


    def handle(self, data):
        for pair in self.pairs:
            prices = (data[pair.symbols[0]].price, data[pair.symbols[1]].price)
            self.handle_pair(pair, prices)


    def handle_pair(self, pair, prices):
        spread = pair.spread(prices)
        self._current = { "pair": pair, "prices": prices, "spread": spread }

        if pair.orders:
            # We are in the middle of something?
            order1 = get_order(pair.orders[0])
            order2 = get_order(pair.orders[1])

            if order1.status == ORDER_OPEN or order2.status == ORDER_OPEN:
                # Open orders. Check if the market went against us.
                # Indicates the put on / unwind values are too close (intraday volatility is too high).
                if pair.state == PUT_ON and pair.is_good_to_unwind(spread):
                    self.vlog("bad put on")
                if pair.state == UNWIND and pair.is_good_to_put_on(spread) and numpy.sign(spread) == pair.direction:
                    self.vlog("bad unwind")

            if order1.status == ORDER_FILLED and order2.status == ORDER_FILLED:
                # Both filled: all normal
                pair.orders_filled()
                self.vlog("filled ok")

        if not pair.orders:
            # Put on or unwind
            if pair.state == READY and pair.is_good_to_put_on(spread):
                pair.put_on(prices, spread)
                self.vlog("PUT ON")
            elif pair.state == PUT_ON and pair.is_good_to_unwind(spread):
                pair.unwind()
                self.vlog("UNWIND")

        # Add a record
        self.add_record()


    def add_record(self):
        if len(self.pairs) <= 5:
            pair = self._current.get("pair")
            spread = self._current.get("spread")
            record(**{pair.name(): spread * 100})
        else:
            record(leverage=self.context.account.leverage)


    def vlog(self, msg):
        pair = self._current.get("pair")
        prices = self._current.get("prices")
        spread = self._current.get("spread")
        price_info = "[%s: price=%.2f log=%.3f] [%s: price=%.2f log=%.3f] [spread=%+.4f]" % \
                     (pair.symbols[0].symbol, prices[0], math.log(prices[0]),
                      pair.symbols[1].symbol, prices[1], math.log(prices[1]),
                      spread)

        portf = self.context.portfolio
        portfolio_info = "[portfolio: value=%.2f cash=%.2f pos=%d pos-value=%.2f]" % \
                         (portf.portfolio_value, portf.cash, len(portf.positions), portf.positions_value)

        print "%9s %15s %s %s" % (pair.name(), msg, price_info, portfolio_info)


########################################################################################################################
# Pair
########################################################################################################################


READY = 0
PUT_ON = 1
UNWIND = 2


class Pair:
    def __init__(self, symbols, gamma, mean, sd, delta, eps):
        assert symbols[0], symbols[1]
        assert eps < delta

        self.symbols = symbols
        self.gamma = gamma
        self.mean = mean
        self.sd = sd
        self.delta = delta
        self.eps = eps

        self.state = READY
        self.direction = 0
        self.orders = ()
        self.shares = ()


    def name(self):
        return "%s/%s" % (self.symbols[0].symbol, self.symbols[1].symbol)


    def spread(self, prices):
        return math.log(prices[0]) - self.gamma * math.log(prices[1]) - self.mean


    def is_good_to_put_on(self, spread):
        return abs(spread) > self.delta


    def is_good_to_unwind(self, spread):
        return spread * self.direction < self.eps


    def put_on(self, prices, spread):
        share_ratio = abs(self.gamma * prices[0] / prices[1])
        num, den, error = approx_rational(share_ratio, limit=50)

        direction = numpy.sign(spread)
        shares1 = -den * direction
        shares2 = num * direction * numpy.sign(self.gamma)

        # Allowed cost should depend on the riskiness of the pair, but it's fixed right now.
        allowed_cost = 500
        spread_cost = abs(shares1 * prices[0] + shares2 * prices[1])
        if spread_cost < allowed_cost:
            mult = math.floor(allowed_cost / spread_cost)
            shares1 *= mult
            shares2 *= mult

        order_id1 = order(self.symbols[0], shares1)
        order_id2 = order(self.symbols[1], shares2)
        assert order_id1 and order_id2

        self.state = PUT_ON
        self.direction = direction
        self.orders = (order_id1, order_id2)
        self.shares = (shares1, shares2)


    def unwind(self):
        order_id1 = order(self.symbols[0], -self.shares[0])
        order_id2 = order(self.symbols[1], -self.shares[1])
        self.state = UNWIND
        self.orders = (order_id1, order_id2)
        assert order_id1 and order_id2


    def orders_filled(self):
        self.orders = ()
        if self.state == UNWIND:
            self.state = READY
            self.direction = 0


########################################################################################################################
# Utils
########################################################################################################################


def approx_rational(val, limit):
    """
    Approximates the float value by a ratio of two integers.
    Works with positive values only.

    :param val: a positive float value
    :param limit: a limit for both numerator and denominator
    :return: a triple of numerator, denominator and an error
    """
    last_num, num, last_den, den = 1, int(val), 0, 1
    rest, quot = val, int(val)
    while abs(rest - quot) > 0.00001:
        rest = 1.0 / (rest - quot)
        quot = int(rest)
        next_num = quot * num + last_num
        next_den = quot * den + last_den
        if abs(next_num) > limit or abs(next_den) > limit:
            break
        last_num, num, last_den, den = (num, next_num, den, next_den)
    return num, den, val - float(num) / den
