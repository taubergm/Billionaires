# converts forbes badly formatted txt data to csv

import re

line_buffer = []
with open('billionaires_2020.txt') as f:
    line_buffer = [line.rstrip() for line in f]

name_line = 0 
line_number = 0
for line in line_buffer:
    line_number = line_number + 1
    if (line.isupper() and (re.match(".*\$", line) is None)) :
        name = line_buffer[line_number-1]
        rank = line_buffer[line_number-2]
        net_worth = line_buffer[line_number]
        age = line_buffer[line_number+1]
        country = line_buffer[line_number+2]
        source = line_buffer[line_number+3]
        industries = line_buffer[line_number+4]

        print("{},{},{},{},{},{},{}".format(rank,name,net_worth,age,country,source,industries))
