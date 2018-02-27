#!/usr/bin/env python
import socket
import sys

with open(sys.argv[1], 'r') as content_file:
    data = content_file.read()

print data
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

try:
    sock.connect(("localhost", 7890))
    sock.sendall(data + "\n")
    print "Sent"
    received = sock.recv(1024)
finally:
    sock.close()

print "Sent:    {}".format(data)
print "Received: {}".format(received)
