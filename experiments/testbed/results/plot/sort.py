#!/usr/bin/env python

import sys, argparse

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--input', type=str, action='store', dest='input', default=None, help="Input file")
    args = parser.parse_args()
    stats = dict()
    if args.input is None:
        print "Error: No input file"
    with open(args.input) as in_file:
        for line in in_file.readlines():
            time = int(line.split()[0])
            tx_bytes = int(line.split()[1])
            stats[time] = tx_bytes
    stats = sorted(stats.items())
    start_time = stats[0][0]
    prev_tx = stats[0][1]
    no_traffic_flag = True
    for time, tx_bytes in stats:
        if no_traffic_flag:
            if tx_bytes > (prev_tx+100000):
                no_traffic_flag = False
                start_time, prev_tx = time, tx_bytes
        else:
            print (time-start_time), (tx_bytes-prev_tx)
            prev_tx = tx_bytes


if __name__ == "__main__":
    main()            
