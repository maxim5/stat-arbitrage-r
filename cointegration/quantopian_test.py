#! /usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = "maxim"


import unittest
from quantopian import approx_rational


class ApproximateRationalTest(unittest.TestCase):
    def test_exact(self):
        self.assert_ratio(approx_rational(0, limit=10), 0, 1)
        self.assert_ratio(approx_rational(0.1, limit=10), 1, 10)
        self.assert_ratio(approx_rational(0.2, limit=10), 1, 5)
        self.assert_ratio(approx_rational(0.5, limit=10), 1, 2)
        self.assert_ratio(approx_rational(0.6, limit=10), 3, 5)
        self.assert_ratio(approx_rational(0.8, limit=10), 4, 5)
        self.assert_ratio(approx_rational(1.0, limit=10), 1, 1)
        self.assert_ratio(approx_rational(1.2, limit=10), 6, 5)
        self.assert_ratio(approx_rational(1.5, limit=10), 3, 2)
        self.assert_ratio(approx_rational(2.5, limit=10), 5, 2)


    def test_simple(self):
        self.assert_ratio(approx_rational(0.3, limit=5), 1, 3, -0.033333333333333326)
        self.assert_ratio(approx_rational(0.3, limit=10), 3, 10)

        self.assert_ratio(approx_rational(1.05, limit=10), 1, 1, 0.05)
        self.assert_ratio(approx_rational(1.05, limit=20), 20, 19, -0.002631578947368318)

        self.assert_ratio(approx_rational(1.1, limit=10), 10, 9, -0.011111111111111072)
        self.assert_ratio(approx_rational(1.1, limit=20), 11, 10)

        self.assert_ratio(approx_rational(3.1415, limit=50), 22, 7, -0.0013571428571426125)


    def test_approximation_is_better_with_larger_limit(self):
        self.assert_ratio(approx_rational(5.78, limit=25), 23, 4, 0.03000000000000025)
        self.assert_ratio(approx_rational(5.78, limit=50), 29, 5, -0.019999999999999574)
        self.assert_ratio(approx_rational(5.78, limit=100), 52, 9, 0.0022222222222225696)

        self.assert_ratio(approx_rational(1.02604582809, limit=20), 1, 1, 0.02604582809)
        self.assert_ratio(approx_rational(1.02604582809, limit=50), 39, 38, -0.0002699613836842918)
        self.assert_ratio(approx_rational(1.02604582809, limit=100), 79, 77, 0.00007180211597401787)

        self.assert_ratio(approx_rational(3.1792066, limit=15), 3, 1, 0.1792066000000001)
        self.assert_ratio(approx_rational(3.1792066, limit=25), 19, 6, 0.012539933333333586)
        self.assert_ratio(approx_rational(3.1792066, limit=50), 35, 11, -0.002611581818181552)
        self.assert_ratio(approx_rational(3.1792066, limit=100), 89, 28, 0.0006351714285717236)


    def assert_ratio(self, actual, numerator, denominator, error=0.0):
        self.assertEqual(actual[0], numerator)
        self.assertEqual(actual[1], denominator)
        self.assertTrue(abs(actual[2] - error) < 0.000000001)


if __name__ == '__main__':
    unittest.main()
