# -*- coding: utf-8 -*-
__author__ = "maxim"


import math
import numpy


OPEN = 0
FILLED = 1
CANCELLED = 2

READY = 0
PUT_ON = 1
UNWIND = 2


class Pair:
    def __init__(self, symbols, num1, num2, mean, eps, delta):
        self.symbol1, self.symbol2 = symbols
        self.num1 = num1
        self.num2 = num2
        self.mean = mean
        self.eps = eps
        self.delta = delta

        self.state = READY
        self.direction = 0
        self.orders = ()


    def name(self):
        return "%s/%s" % (self.symbol1.symbol, self.symbol2.symbol)


    def spread(self, price1, price2):
        return self.num1 * math.log(price1) - self.num2 * math.log(price2) - self.mean


    def is_wide(self, spread):
        return abs(spread) > self.delta


    def is_narrow(self, spread):
        return abs(spread) < self.eps or numpy.sign(spread) != self.direction


    def put_on(self, direction):
        order_id1 = order_target(self.symbol1, -self.num1 * direction)
        order_id2 = order_target(self.symbol2, self.num2 * direction)
        self.state = PUT_ON
        self.direction = direction
        self.orders = (order_id1, order_id2)
        assert order_id1 and order_id2


    def unwind(self):
        order_id1 = order_target(self.symbol1, 0)
        order_id2 = order_target(self.symbol2, 0)
        self.state = UNWIND
        self.orders = (order_id1, order_id2)
        assert order_id1 and order_id2


    def cancel(self, open1, open2):
        if open1:
            cancel_order(self.orders[0])
        else:
            order_target(self.symbol1, 0)
        if open2:
            cancel_order(self.orders[1])
        else:
            order_target(self.symbol2, 0)


    def complete(self):
        self.orders = ()
        if self.state == UNWIND:
            self.state = READY
            self.direction = 0


    def revert(self):
        self.orders = ()
        if self.state == PUT_ON:
            self.state = READY
            self.direction = 0


def initialize(context):
    set_symbol_lookup_date("2015-01-01")
    set_commission(commission.PerTrade(cost=0.03))
    set_slippage(slippage.FixedSlippage(spread=0.01))

    context.pairs = (
        Pair(symbols=symbols("VGIT", "SCHR"), num1=28, num2=29, mean=1.1836, eps=0.02, delta=0.06),
        Pair(symbols=symbols("IEI", "ITE"), num1=19, num2=23, mean=-2.773, eps=0.02, delta=0.06),
        Pair(symbols=symbols("QUAL", "IVV"), num1=5, num2=6, mean=-11.293, eps=0.02, delta=0.06),
    )


def handle_data(context, data):
    for pair in context.pairs:
        context.data1_ = data[pair.symbol1]
        context.data2_ = data[pair.symbol2]
        spread = pair.spread(context.data1_.price, context.data2_.price)

        if pair.orders:
            # We are in the middle of something?
            order1 = get_order(pair.orders[0])
            order2 = get_order(pair.orders[1])

            if order1.status == OPEN or order2.status == OPEN:
                # Open orders. Check if the market went against us
                if pair.state == PUT_ON and pair.is_narrow(spread):
                    pair.cancel(order1.status == OPEN, order2.status == OPEN)
                    vlog("CANCEL PUT ON", pair, context)
                if pair.state == UNWIND and pair.is_wide(spread):
                    vlog("bad unwind", pair, context)

            if order1.status == FILLED and order2.status == FILLED:
                # Both filled: all normal
                pair.complete()
                vlog("filled ok", pair, context)

            if (order1.status == CANCELLED or order2.status == CANCELLED) and (not context.portfolio.positions):
                # Both cancelled: revert
                pair.revert()
                vlog("cancelled ok", pair, context)

        if not pair.orders:
            # Put on or unwind
            if pair.state == READY and pair.is_wide(spread):
                pair.put_on(direction=numpy.sign(spread))
                vlog("PUT ON", pair, context)
            elif pair.state == PUT_ON and pair.is_narrow(spread):
                pair.unwind()
                vlog("UNWIND", pair, context)

        # Add a record
        record(**{pair.name(): spread * 100})


def vlog(msg, pair, context):
    price_info = "[%s: price=%.2f log=%.3f] [%s: price=%.2f log=%.3f] [spread=%+.4f]" % \
                 (pair.symbol1.symbol, context.data1_.price, math.log(context.data1_.price),
                  pair.symbol2.symbol, context.data2_.price, math.log(context.data2_.price),
                  pair.spread(context.data1_.price, context.data2_.price))

    portfolio_info = "[portfolio: value=%.2f cash=%.2f pos=%d pos-value=%.2f]" % \
                     (context.portfolio.portfolio_value, context.portfolio.cash,
                      len(context.portfolio.positions), context.portfolio.positions_value)

    print "%9s %15s %s %s" % (pair.name(), msg, price_info, portfolio_info)
