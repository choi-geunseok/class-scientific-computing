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

exename = input("Type the executable name:")
path1 = os.getcwd()
name1 = path1 + '\%s' % exename
name1 = name1.split()
name2 = name1[0]
cmd = name2.replace('\\','/')
os.system('%s' % cmd)

### Read INPUT ###

inp_all = read_words('INP')
inp_val = []
for i in range(len(inp_all)):
    inp_val.append(inp_all[i][0])
#c = inp_val[0]
#del(inp_val[0])

val_list_int   = [0, 4, 7, 8, 9, 11]
val_list_float = [1, 2, 3, 5, 6, 10]

for i in val_list_int:
    inp_val[i] = int(inp_val[i])
for j in val_list_float:
    inp_val[j] = float(inp_val[j])

############################################

### Read OUTPUT ###

out_all = read_words('OUT.txt')
out_block = out_all[11:]
out_time = []
for i in range(inp_val[9]):
    time = out_block[(inp_val[8]+1)*i][-1]
    time = float(time)
    out_time.append(time)

out_domain = []; out_barrier = []
for i in range(1,inp_val[8]+1):
    domain = out_block[i][0]; barrier = out_block[i][1]
    domain = float(domain); barrier = float(barrier)
    out_domain.append(domain); out_barrier.append(barrier)

out_phi1 = []
out_phi2 = []
for i in range(len(out_block)):
    if out_block[i][0] == '#':
        pass
    else:
        out_phi1.append(float(out_block[i][2]))

for i in range(inp_val[9]):
    line = []
    for j in range(inp_val[8]):
        line.append(out_phi1[inp_val[8]*i + j])
    out_phi2.append(line)

############################################

### Make movie ###

import matplotlib.pyplot as plt

os.system('rm -rf *.png')

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
