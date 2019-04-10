import odespy
import matplotlib.pyplot as plt


def f(u, t, omega=2):
    v, u = u
    return [-omega**2*u, v]

def compare(odespy_methods,
           omega,
           X_0,
           number_of_periods,
           time_intervals_per_period=20):

    from numpy import pi, linspace, cos
    P = 2*pi/omega
    dt = P/time_intervals_per_period
    T = number_of_periods*P
    
    # If odespy_methods is not a list, but just the name of a single Odespy solver, we wrap that name in a list
    # so we always have odespy_methods as a list
    if type(odespy_methods) != type([]):
        odespy_methods = [odespy_methods]
        
    # Make a list of solver objects
    solvers = [method(f, f_args=[omega]) for method in 
              odespy_methods]
    for solver in solvers:
        solver.set_initial_condition([0, X_0])
        
    # Compute the time points where we wnat the solution
    dt = float(dt) # avoid integer division
    N_t = int(round(T/dt))
    time_points = linspace(0, N_t*dt, N_t+1)
    
    legends = []
    for solver in solvers:
        sol, t = solver.solve(time_points)
        v = sol[:, 0]
        u = sol[:, 1]
        
        # Plot only the last p periods
        p = 6
        m = p*time_intervals_per_period # no time steps to plot
        plt.plot(t[-m:], u[-m:])
        plt.hold('on')
        legends.append(solver.name())
        plt.xlabel('t')
        # Plot exact solution too
        plt.plot(t[-m:], X_0*cos(omega*t)[-m:], 'k--')
        legends.append('exact')
        plt.legend(legends, loc='lower left')
        plt.axis([t[-m], t[-1], -2*X_0, 2*X_0])
        plt.title('Simulation of %d periods with %d intervals per period'
             % (number_of_periods, time_intervals_per_period))
        #plt.savefig('tmp.pdf'); savefig('tmg.png')
        plt.show()

compare(odespy_methods=[odespy.Heun, odespy.EulerCromer],
       omega=2, X_0=2, number_of_periods=20,
       time_intervals_per_period=20)
