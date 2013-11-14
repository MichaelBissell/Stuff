#!/usr/bin/python

import sys

# Need to get counts for each key, value pair
# i.e. counts in each histogram bin
current_key = None
current_count = 0
key = None
for line in sys.stdin:
    line = line.strip()
    key, count = line.rsplit(',', 1)
    
    try:
        count = int(count)
    except ValueError:
        continue
    if current_key == key:
        current_count += count
    else:
        if current_key:
            # write result to STDOUT
            print '%s,%s' % (current_key, current_count)
        current_count = count
        current_key = key

if current_key == key:
    print '%s,%s' % (current_key, current_count)
