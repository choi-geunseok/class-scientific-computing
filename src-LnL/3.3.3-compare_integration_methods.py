#!/usr/bin/python3

from trapezoidal import trapezoidal
from midpoint import midpoint
from math import exp

g = lambda y: exp(-y**2)
a = 0
b = 2

print('    i       n       midpoint          trapezoidal')

for i in range(1,21):
    n = 2**i
    m = midpoint(g, a, b, n)
    t = trapezoidal(g, a, b, n)
    print('%7d %7d %.16f %.16f' %(i, n, m, t))
