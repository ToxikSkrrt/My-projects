#!/usr/bin/python3

# RPC Server (UDP)
# Author(s):

import sys
import socket
import xdr      # module provided by teacher in VPL
import rpcnet   # module provided by teacher in VPL
import rpcmsg   # module provided by teacher in VPL
import rpcbind  # module provided by teacher in VPL

###############################################
###              CONSTANTS                  ###
###############################################

TEST_PROG = 0x20000001
TEST_VERS = 1

PROC_NULL = 0
PROC_PI = 1
PROC_INC = 2
PROC_ADD = 3
PROC_ECHO = 4

###############################################
###             PROCEDURE                   ###
###############################################

# todo

###############################################
###                 MAIN                    ###
###############################################

if (len(sys.argv) != 2):
    print("Usage: server.py <port>")
    sys.exit(1)

host = ''
port = int(sys.argv[1])

sServer = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sServer.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

sServer.bind((host, port))

sServer.listen(1)

reg = rpcbind.register(536870913, TEST_PROG, TEST_VERS)

while True:
    s, addr = sServer.accept()
    while True:
        data, address = s.recvfrom(1500)
        msg1, msg2 = xdr.decode_two_int(data)
        if msg1 == PROC_NULL:
            pass
        elif msg1 == PROC_PI:
            dataAns = xdr.encode_int(3.1415926)
            s.sendto(dataAns, address)
        elif msg1 == PROC_INC:
            dataAns = xdr.encode_int(msg2 + 1)
            s.sendto(dataAns, address)
        elif msg1 == PROC_ADD:
            dataAns1, dataAns2 = xdr.decode_two_int(msg2)
            dataAns = dataAns1 + dataAns2
            s.sendto(dataAns)
        elif msg1 == PROC_ECHO:
            dataAns = xdr.encode_string(msg2)
        else:
            break

# msg = rpcnet.reply(sServer, handle??)   Utiliser surement cette fonction à un moment aussi

# EOF
