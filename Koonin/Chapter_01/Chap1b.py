#!/usr/bin/python3

import numpy as np
import math

# Chapter 1b Fortran to Python
# Comparing trapezodal vs Simpson rule (h**3 vs h**5 errors)

N = [4, 8, 16, 32, 64, 128]


class FDM2:
    def Trapezoidal(f, a, b, N):
        h = float(b-a) / N
        result = 0.5*f(a) + 0.5*f(b)
        for i in range(1, N):
            result += f(a + i*h)
        result *= h
        return result

    def Simpsons(f, a, b, N):
        if N % 2 == 1:
            raise ValueError("N must be an even integer.")
        h = float(b-a) / N
        x = np.linspace(a, b, N+1)
        y = f(x)
        result = h/3 * np.sum(y[0:-1:2] + 4*y[1::2] + y[2::2])
        return result

    def Bode(f, a, b, N):
        h = float(b-a) / N
        x = np.linspace(a, b, N+1)
        y = f(x)
        result = 2*h/45 * np.sum(7*y[0:-1:4] + 32*y[1::4] + 12*y[2::4] + 32*y[3::4] + 7*y[4::4])
        return result

cal_1 = FDM2.Trapezoidal
cal_2 = FDM2.Simpsons
cal_3 = FDM2.Bode

print('-------------------------------------------------------------------------')
print('  N      |   h  |    Trapezoidal   |   Simpsons   |   Bode')
print('-------------------------------------------------------------------------')



EXACT = np.exp(1.0) - 1.0
for n in N:
    DIFF1 = EXACT - cal_1(np.exp, 0, 1, n)
    DIFF2 = EXACT - cal_2(np.exp, 0, 1, n)
    DIFF3 = EXACT - cal_3(np.exp, 0, 1, n)
    print(" %4.0f     %1.6f    %1.6f       %1.6f     %1.6f" % (int(n), (1-0)/n, DIFF1, DIFF2, DIFF3)) 
print('-------------------------------------------------------------------------')
