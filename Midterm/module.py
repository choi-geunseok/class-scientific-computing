from __future__ import print_function
import numpy as np
import matplotlib.pyplot as plt

def numerov(xgrid, y_0, y_1, k, S):
    # initialize y
    ngrid = len(xgrid)
    h = xgrid[1] - xgrid[0]
    y = np.zeros(ngrid)
    y[0] = y_0
    y[1] = y_1

    # main loop: evaluate y[j]
    for j in np.arange(2, ngrid):

        y1 = y[j-2]; y2 = y[j-1]
        k1 = k[j-2]; k2 = k[j-1]; k3 = k[j]
        s1 = S[j-2]; s2 = S[j-1]; s3 = S[j] 

        term_S = 1/12. * h**2 * (s3 + 10*s2 + s1)
        term_3 =      (1 + 1/12. *   h**2 * k3**2)
        term_2 = -2 * (1 - 5/12. * 5*h**2 * k2**2) * y2
        term_1 =      (1 + 1/12. *   h**2 * k1**2) * y1

        y3 = (term_S - term_2 - term_1) / term_3
        y[j] = y3

    return y


#####################
# From now on, we add the functions that we already studied in EE488 class.
# The sources are from the textbook : Programming for Computations - A Gentle Introduction to Numerical Simulations with python
# You may reuse below functions to solve mid-term project
# or make the function yourself!
#
# Good luck to you!
#
#####################

####
# naive_Newton : textbook 6.2.1 (p.230) 
####
def naive_Newton(f, dfdx, x, epx):
    while abs(f(x)) > epx:
        x = x -float(f(x))/dfdx(x)
        print('% 20.10f, % 20.10f' % (x, f(x)))
    return x

####
# Newton : textbook 6.2.2 (p.233)  
####

def Newton(f, dfdx, x, eps):
    f_value = f(x)
    iteration_counter = 0
    while abs(f_value) > eps and iteration_counter < 100:
        try:
            x = x -float(f_value)/dfdx(x)
        except ZeroDivisionError:
            print("Error! - derivative zero for x = ", x)
            sys.exit(1)  # Abort with error / Sys.exit(0) means succesfully operating
            
        f_value = f(x)
        iteration_counter += 1
        
    # Here, either a solution is found, or too many iterations
    if abs(f_value) > eps:
        iteration_counter = -1
    return x, iteration_counter

####
# Secant : textbook 6.3 (p.236)
####

def secant(f, x0, x1, eps):
    f_x0 = f(x0)
    f_x1 = f(x1)
    iteration_counter = 0
    while abs(f_x1) > eps and iteration_counter < 100:
        try:
            denominator = float(f_x1 - f_x0)/(x1 - x0)
            x = x1 - float(f_x1) / denominator
            print('%d % 20.10f, % 20.10f' % (iteration_counter+1, x, f(x)))
        except ZeroDivisionError:
            print("Error! - denominator zero for x = ", x)
            sys.exit(1)  # Abort with error
        x0 = x1
        x1 = x
        f_x0 = f_x1
        f_x1 = f(x1)
        iteration_counter += 1
    # Here, either a solution is found, or too many iterations
    if abs(f_x1) > eps:
        iteration_counter = -1
    return x, iteration_counter

####
# bisection : textbook 6.4 (p.239)
####

def bisection(f, x_L, x_R, eps, return_x_list=False):
    f_L = f(x_L)
    if f_L*f(x_R) > 0:
        print("Error! Function does not have opposite \
                signs at interval endpoints")
        sys.exit(1)
    x_M = float(x_L + x_R)/2.0
    f_M = f(x_M)
    iteration_counter = 1
    if return_x_list:
        x_list = []
        
    while abs(f_M) > eps:
        if f_L*f_M > 0: # i.e. same sign
            x_L = x_M
            f_L = f_M
        else:
            x_R = x_M
        x_M = float(x_L + x_R)/2.0
        f_M = f(x_M)
        iteration_counter += 1
        print('%d % 20.10f, % 20.10f % 20.10f' % (iteration_counter, x_L, x_M, f_M))
        if return_x_list:
            x.list.append(x_M)
    if return_x_list:
        return x_list, iteration_counter
    else:
        return x_M, iteration_counter

####
# ode_FE : textbook 4.2.6 (p.140)
####

def ode_FE(f, U_0, dt, T):
    N_t = int(round(float(T)/dt))
    # Ensure that any list/tuple returned from f_ is wrapped as array
    f_ = lambda u, t: asarray(f(u,t))
    u = zeros((N_t+1, len(U_0)))
    t= linspace(0, N_t*dt, len(u))
    u[0] = U_0
    for n in range(N_t):
        u[n+1] = u[n] + dt*f_(u[n], t[n])
    return u, t

####
# rhs : textbook 5.1.3 (p.202)
####

def rhs(u, t):
    N = len(u) - 1
    rhs = zeros(N+1)
    rhs[0] = dsdt(t)
    for i in range(1, N):
        rhs[i] = (beta/dx**2)*(u[i+1] - 2*u[i] + u[i-1]) + \
                 g(x[i], t)
    i = N
    rhs[i] = (beta/dx**2)*(2*u[i-1] + 2*dx*dudx(t) -
                           2*u[i]) + g(x[N], t)
    return rhs

