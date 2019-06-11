#! /usr/bin/python3

### Update : 2019 / 05 / 21
# Min Jong Noh #
# starnmj@kaist.ac.kr
###

###
# This is a script for gathering all the PNG file to make animation (gif)
###

###
import os, sys, math

os.system('rm -rf animation')
os.system('mkdir animation')
os.system('mv *.png animation')
os.chdir('animation')
os.system('convert -delay 5 -loop 1000 -quality 100 -resize 500x500 *.png movie.gif')
os.system('animate movie.gif')

