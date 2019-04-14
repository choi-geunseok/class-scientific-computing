import odespy
import numpy as np
import matplotlib.pyplot as plt


def f(u, t):
    return u

method = odespy.Heun
solver = method(f)
solver.set_initial_condition(2)
time_points = np.linspace(0, 4, 101)
u, t = solver.solve(time_points)

plt.plot(t,u)
plt.show()
