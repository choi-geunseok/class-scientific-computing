from __future__ import print_function
import numpy as np
import matplotlib.pyplot as plt
import module as md

#
# Exercise 3.5 (Koonin's Ch.3)
#
# Apply the Nuerov algorithm to the problem
#
#      d^2 y
#     ------- = -4 * pi^2 * y; y(0)=1, y'(0)=0 .
#      dx*2
#
# Integrate from x=0 to x=1 with various step sizes and compare the 
# effciency and accuracy with some of the methods discussed in previous
# lectures. 
#

#
# Example solution
#
x = np.linspace(0, 1, 101)
y = np.zeros(101)
a = np.zeros(101)
y = md.numerov(x, 1, 1, 4*np.pi*np.ones(101), a)
plt.plot(x, y)
plt.show()

