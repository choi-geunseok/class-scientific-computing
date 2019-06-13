#! /usr/bin/python3

### Update : 2019 / 05 / 21
# Min Jong Noh #
# starnmj@kaist.ac.kr
###

###
# This is a script for automatic graphic(PNG) generation from raw data
###

###
import os, sys, math
###

############################################
### Function Arrangement ###

def file_len(filename):
    with open(filename) as f:
        for i, l in enumerate(f):
            pass
    return i+1

def read_words(filename):
    ol = []
    TL = file_len('%s' % filename)
    f = open('%s' % filename)
    for i in range(TL):
        line = f.readline()
        words = line.split()
        ol.append(words)
    return ol

############################################

### Set simulation tag ###
if len(sys.argv) == 3 and sys.argv[1] == "--tag":
    tagname = sys.argv[2]
elif len(sys.argv) == 1:
    tagname = "tmp"
else:
    print("Check the usage: python simviz.py --tag (filename)")

### Read OUTPUT ###
out_all = read_words('OUT.dat')
out_block = out_all[11:]

NSTEP = 0
NPTS_count = []
out_time = []; out_domain = []; out_barrier = []
out_phi1 = []; out_phi2 = []

for i in range(len(out_block)):
    if out_block[i][0] == '#':
        NSTEP += 1
        NPTS_count.append(i)
    else:
        out_phi1.append(float(out_block[i][2]))

NPTS = (NPTS_count[1] - NPTS_count[0]) - 1

### Save evolution time ###
for i in range(NSTEP):
    time = out_block[(NPTS+1)*i][-1]
    time = float(time)
    out_time.append(time)

### Save domain & barrier ###
for i in range(1, NPTS+1):
    domain = out_block[i][0]; barrier = out_block[i][1]
    domain = float(domain); barrier = float(barrier)
    out_domain.append(domain); out_barrier.append(barrier)

### Save wavepacket each time evolution step ###
for i in range(NSTEP):
    line = []
    for j in range(NPTS):
        line.append(out_phi1[NPTS*i + j])
    out_phi2.append(line)

############################################

### PNG Generation ###

import matplotlib.pyplot as plt

os.system('rm -rf *.png')
os.system('rm -rf case_%s' % tagname)
os.system('mkdir case_%s' % tagname)
for i in range(len(out_time)):
    plt.clf()
    fig = plt.figure(figsize=(8,5))
    plt.axis([out_domain[0]-0.05, out_domain[-1]+0.05, -0.05, max(out_barrier)+0.05])
    plt.xlabel('Displacement', fontsize=20)
    plt.ylabel('Probability (Wavepacket)', fontsize=20)
    line2 = plt.plot(out_domain,out_barrier, label="Potential", color='red')
    line1 = plt.plot(out_domain,out_phi2[i], color='blue', label='Wave time at %5.4f sec' % (out_time[i]))
    plt.legend(loc=2)
    plt.savefig('Wave_%04d.png' % (i+1), dpi=300)
os.system('mv *.png case_%s' % tagname)
