# -*- coding: utf-8 -*-
__author__ = "maxim"


import math
import numpy


def initialize(context):
    set_symbol_lookup_date("2014-01-01")

    context.pairs = (
    )

    schedule_function(
        func=report_returns,
        date_rule=date_rules.month_end(days_offset=2),
        time_rule=time_rules.market_close(minutes=1),
        half_days=True
      )


def handle_data(context, data):
    for pair in context.pairs:
        price1 = data[pair.symbol1].price
        price2 = data[pair.symbol2].price
        spread = pair.spread(price1, price2)

        context.price = (price1, price2)

        if pair.orders:
            # We are in the middle of something?
            order1 = get_order(pair.orders[0])
            order2 = get_order(pair.orders[1])

            if order1.status == OPEN or order2.status == OPEN:
                # Open orders. Check if the market went against us
                if pair.state == PUT_ON and pair.is_good_to_unwind(spread):
                    vlog("bad put on", pair, context)
                if pair.state == UNWIND and pair.is_good_to_put_on(spread) and numpy.sign(spread) == pair.direction:
                    vlog("bad unwind", pair, context)

            if order1.status == FILLED and order2.status == FILLED:
                # Both filled: all normal
                pair.orders_filled(spread)
                vlog("filled ok", pair, context)

        if not pair.orders:
            # Put on or unwind
            if pair.state == READY and pair.is_good_to_put_on(spread):
                pair.put_on(price1, price2)
                vlog("PUT ON", pair, context)
            elif pair.state == PUT_ON and pair.is_good_to_unwind(spread):
                pair.unwind()
                vlog("UNWIND", pair, context)

        # Add a record
        if len(context.pairs) < 5:
            record(**{pair.name(): spread})
        else:
            record(leverage=context.account.leverage, total_positions_value=context.account.total_positions_value)


def report_returns(context, data):
    for pair in context.pairs:
        log.info("%s: %.3f" % (pair.name(), pair.total_spread))


########################################################################################################################
# Pair
########################################################################################################################


OPEN = 0
FILLED = 1
CANCELLED = 2

READY = 0
PUT_ON = 1
UNWIND = 2


class Pair:
    def __init__(self, symbols, gamma, mean, sd, delta, eps):
        assert symbols[0], symbols[1]
        assert eps < delta

        self.symbol1, self.symbol2 = symbols
        self.gamma = gamma
        self.mean = mean
        self.sd = sd
        self.delta = delta
        self.eps = eps

        self.state = READY
        self.direction = 0
        self.orders = ()
        self.put_on_spread = 0      # spread when filled
        self.total_spread = 0


    def name(self):
        return "%s/%s" % (self.symbol1.symbol, self.symbol2.symbol)


    def spread(self, price1, price2):
        return math.log(price1) - self.gamma * math.log(price2) - self.mean


    def is_good_to_put_on(self, spread):
        return abs(spread) > self.delta


    def is_good_to_unwind(self, spread):
        return spread * self.direction < self.eps


    def put_on(self, price1, price2):
        share_ratio = self.gamma * price1 / price2
        shares2, shares1, error = approx_rational(share_ratio, limit=50)

        spread = self.spread(price1, price2)
        direction = numpy.sign(spread)

        order_id1 = order_target(self.symbol1, -shares1 * direction)
        order_id2 = order_target(self.symbol2, shares2 * direction)
        assert order_id1 and order_id2

        self.state = PUT_ON
        self.direction = direction
        self.orders = (order_id1, order_id2)


    def unwind(self):
        order_id1 = order_target(self.symbol1, 0)
        order_id2 = order_target(self.symbol2, 0)
        self.state = UNWIND
        self.orders = (order_id1, order_id2)
        assert order_id1 and order_id2


    def orders_filled(self, spread):
        self.orders = ()
        if self.state == UNWIND:
            self.state = READY
            self.direction = 0
            self.total_spread += abs(self.put_on_spread - spread) if self.put_on_spread else 0
            self.put_on_spread = 0
        else:
            self.put_on_spread = spread


########################################################################################################################
# Utils
########################################################################################################################


def approx_rational(val, limit):
    val, last_num, num = ((-val, -1, int(-val)) if val < 0 else (val, 1, int(val)))
    last_den, den = 0, 1
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


def vlog(msg, pair, context):
    price_info = "[%s: price=%.2f log=%.3f] [%s: price=%.2f log=%.3f] [spread=%+.4f]" % \
                 (pair.symbol1.symbol, context.price[0], math.log(context.price[0]),
                  pair.symbol2.symbol, context.price[1], math.log(context.price[1]),
                  pair.spread(context.price[0], context.price[1]))

    portfolio_info = "[portfolio: value=%.2f cash=%.2f pos=%d pos-value=%.2f]" % \
                     (context.portfolio.portfolio_value, context.portfolio.cash,
                      len(context.portfolio.positions), context.portfolio.positions_value)

    print "%9s %15s %s %s" % (pair.name(), msg, price_info, portfolio_info)
