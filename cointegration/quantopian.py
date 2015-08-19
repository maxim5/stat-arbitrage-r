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
    set_commission(commission.PerTrade(cost=0.01))
    set_slippage(slippage.FixedSlippage(spread=0.00))

    context.pair = Pair(symbols=symbols("VGIT", "SCHR"), num1=28, num2=29,
                        mean=1.1836, eps=0.02, delta=0.06)


def handle_data(context, data):
    pair = context.pair

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
                vlog("CANCEL PUT ON", context)
            if pair.state == UNWIND and pair.is_wide(spread):
                vlog("bad unwind", context)

        if order1.status == FILLED and order2.status == FILLED:
            # Both filled: all normal
            pair.complete()
            vlog("filled ok", context)

        if (order1.status == CANCELLED or order2.status == CANCELLED) and (not context.portfolio.positions):
            # Both cancelled: revert
            pair.revert()
            vlog("cancelled ok", context)

    if not pair.orders:
        if pair.state == READY and pair.is_wide(spread):
            pair.put_on(direction=numpy.sign(spread))
            vlog("PUT ON", context)
        elif pair.state == PUT_ON and pair.is_narrow(spread):
            pair.unwind()
            vlog("UNWIND", context)

    record(spread=spread*100,
           leverage=context.account.leverage,
           exposure=context.account.net_leverage)


def vlog(msg, context):
    price_info = "[%s: price=%.2f log=%.3f] [%s: price=%.2f log=%.3f] [spread=%+.4f]" % \
                 (context.pair.symbol1.symbol, context.data1_.price, math.log(context.data1_.price),
                  context.pair.symbol2.symbol, context.data1_.price, math.log(context.data1_.price),
                  context.pair.spread(context.data1_.price, context.data2_.price))

    portfolio_info = "[portfolio: value=%.2f cash=%.2f pos=%d pos-value=%.2f]" % \
                     (context.portfolio.portfolio_value, context.portfolio.cash,
                      len(context.portfolio.positions), context.portfolio.positions_value)

    print "%15s %s %s" % (msg, price_info, portfolio_info)
