#! /usr/bin/python3

### Update : 2019 / 05 / 21
# Min Jong Noh #
# starnmj@kaist.ac.kr
###

###
# This is a script for gathering all the PNG file to make animation (gif)
###

###
import os, sys

############################################

### Set simulation tag ###
tagname = input("Type in the simulation tag:")

### Run the main code: simviz.py ###

os.system('python simviz.py --tag %s' % tagname)

### Generate a movie from PNG files ###
os.chdir('case_%s' % tagname)
os.system('convert -delay 5 -loop 1000 -quality 100 -resize 500x500 *.png movie_%s.gif' % tagname)
os.system('animate movie_%s.gif' % tagname)

