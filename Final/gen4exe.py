### Generation Fortran executable ###

import os

fname = input("Type the output name:")
os.system('gfortran exmpl7.f90 -o %s' % fname)
