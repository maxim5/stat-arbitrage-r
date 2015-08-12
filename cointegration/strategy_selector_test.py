#! /usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = "maxim"


import unittest
from strategy_selector import calc_profit


class CalcProfitTest(unittest.TestCase):
    def test_simple(self):
        self.assertEqual(calc_profit([0, 2, 4, 2, 4, 2, 4, 0], (1, 3)), 4)
        self.assertEqual(calc_profit([2, 4, 2, 4, 2, 4, 0], (1, 3)), 4)

        self.assertEqual(calc_profit([0, 3, -3, 3, -3], (1, 2)), 18)
        self.assertEqual(calc_profit([3, -3, 3, -3], (1, 2)), 18)

        self.assertEqual(calc_profit([10, -10, -5, -2, -5, 2], (1, 4)), 32)
        self.assertEqual(calc_profit([0, 10, -10, -5, -2, -5, -2, 2, -2, 2], (1, 4)), 32)


if __name__ == '__main__':
    unittest.main()
