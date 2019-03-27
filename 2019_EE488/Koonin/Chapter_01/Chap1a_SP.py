#!/usr/bin/python3

import math, struct
import numpy as np

# Chapter 1a Fortran to Python

X = 1.0
EXACT = np.float32(math.cos(X))

# Put a spacing value (H), individually
#
#H = float(input('Enter value of H (cf. 0 leads to stop):'))
#if H < 1E-15:
#    print('Enter a proper value again!')
#else:
#    pass

H = [0.50000, 0.20000, 0.10000, 0.05000, 0.02000, 0.01000, 0.00500, 0.00200, 0.00100,
     0.00050, 0.00020, 0.00010, 0.00005, 0.00002, 0.00001]


class FDM:
    def three_points_Sym(X, H):
        FPRIME = (np.float32(math.sin(X+H)) - np.float32(math.sin(X-H))) / np.float32(2*H)
        return FPRIME

    def two_points_FW(X, H):
        FPRIME = (np.float32(math.sin(X+H)) - np.float32(math.sin(X))) / np.float32(H)
        return FPRIME

    def two_points_BW(X, H):
        FPRIME = (np.float32(math.sin(X)) - np.float32(math.sin(X-H))) / np.float32(H)
        return FPRIME

    def five_points_Sym(X, H):
        FPRIME = (np.float32(math.sin(X-2*H)) - 8*np.float32(math.sin(X-H)) + 8*np.float32(math.sin(X+H)) - np.float32(math.sin(X+2*H))) / (np.float32(12*H))
        return FPRIME


cal_1 = FDM.three_points_Sym
cal_2 = FDM.two_points_FW
cal_3 = FDM.two_points_BW
cal_4 = FDM.five_points_Sym

print('---------------------------------------------------------------------------------------------------')
print('  h      |   Symmetric 3-point   |    Forward 2-point   |   Backward 2-point   |   Symmetric 5-point')
print('---------------------------------------------------------------------------------------------------')

for h in H:
    DIFF1   = EXACT - cal_1(X, h)
    DIFF2   = EXACT - cal_2(X, h)
    DIFF3   = EXACT - cal_3(X, h)
    DIFF4   = EXACT - cal_4(X, h)
    print(" %1.5f        % 2.7f             % 2.7f            % 2.7f            % 2.7f" % (h, DIFF1, DIFF2, DIFF3, DIFF4))
print('---------------------------------------------------------------------------------------------------')
