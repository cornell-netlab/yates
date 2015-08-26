#!/usr/bin/env python

import os
import socket
import argparse

BUFSIZE = 1024

def fetch(ip, port, routesfile, statsfile):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.bind((ip,port))
    s.listen(1)
    print "Listening"
    while True:
        try:
            client, address = s.accept()
        except:
            s.close()
            return
        print "Receiving data"
        recvsize = BUFSIZE
        buf = ""
        try:
            while not recvsize < BUFSIZE:
                msg = client.recv(BUFSIZE)
                buf += msg
                recvsize = len(msg)
            print buf
            msg_type = buf.split(' ', 1)[0].strip()
            if msg_type == '1':
                # Update routes
                with open(routesfile, 'w') as outfile:
                    outfile.write(buf.split(' ', 1)[1])
            elif msg_type == '2':
                # Send stats
                with open(statsfile, 'r') as stats_file:
                    stats = stats_file.read()
                    print stats
                    client.sendall(stats)
        except KeyboardInterrupt:
            client.close()
            s.close()
            return
        client.close()


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--ip', type=str, action='store', dest='ip',
                        default='127.0.0.1',
                        help='The IP address of the controller')
    parser.add_argument('-p', '--port', type=int, action='store', dest='port',
                        default=7890,
                        help='The port on the controller to connect to')
    parser.add_argument('-r', '--routes', type=str, action='store', dest='routesfile',
                        default='/proc/kulfi',
                        help='the file to write the route table to')
    parser.add_argument('-s', '--stats', type=str, action='store', dest='statsfile',
                        default='/proc/kulfi_stats',
                        help='the file to write the route table to')
    return parser.parse_args()

if __name__ == '__main__':
    if not os.getuid() == 0:
        print "WARNING: Please run as sudo to talk to Kulfi kernel module"

    args = parse_args()
    fetch(args.ip, args.port, args.routesfile, args.statsfile)
