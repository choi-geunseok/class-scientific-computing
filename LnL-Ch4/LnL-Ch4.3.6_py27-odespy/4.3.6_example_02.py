import odespy
import numpy as np
import matplotlib.pyplot as plt


def f(u, t, a, b):
    return -a*u + b

a = 2
b = 1
method = odespy.Heun
solver = method(f, f_args=[a, b])
solver.set_initial_condition(2)
time_points = np.linspace(0, 4, 101)
u, t = solver.solve(time_points)

plt.plot(t,u)
plt.show()
