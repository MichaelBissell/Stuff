#!/usr/bin/python
# From: http://www.michael-noll.com/tutorials/writing-an-hadoop-mapreduce-program-in-python/

import sys
import math

# input comes from STDIN (standard input)
for line in sys.stdin:
    # remove leading and trailing whitespace
    line = line.strip()
    # split the line into words
    values = line.split()

    # convert to floats
    x = float(values[0])
    y = float(values[1])

    # find lower and upper bounds of x and y rounded to 0.1
    x_hi = math.ceil(x*10)/10
    x_lo = math.floor(x*10)/10

    y_hi = math.ceil(y*10)/10
    y_lo = math.floor(y*10)/10

    # x_lo,x_hi,y_lo,y_hi,count
    print '%s,%s,%s,%s,%s' % (x_lo, x_hi, y_lo, y_hi, 1)

