# converts forbes badly formatted txt data to csv

import re

line_buffer = []
with open('billionaires_2020b.txt') as f:
    line_buffer = [line.rstrip() for line in f]

name_line = 0 
line_number = 0
for line in line_buffer:
    line_number = line_number + 1
    if re.match("\d+\.", line)  is not None:
        name = line_buffer[line_number]
        rank = line_buffer[line_number-1]
        net_worth = line_buffer[line_number+1]

        print("{},{},{}".format(rank,name,net_worth))
