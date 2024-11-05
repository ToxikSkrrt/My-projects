#!/usr/bin/python3

# RPC Net Module
# Author(s):

import socket
import rpcmsg

###############################################
###           CONSTANTS                     ###
###############################################

MAXMSG = 1500

###############################################
###                CALL UDP                 ###
###############################################


def call(host, port, xid, prog, vers, proc, args) -> bytes:

    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    msg = rpcmsg.encode_call(xid, prog, vers, proc, args)
    s.sendto(msg, (host, port))
    data, address = s.recvfrom(MAXMSG)
    # recvfrom renvoit tuple (data,address) on ne garde que data
    result = rpcmsg.decode_reply(data)
    return result[1]  # decode reply retourne tuple (xid,data)

###############################################
###               REPLY UDP                 ###
###############################################


def reply(sserver, handle):
    data, add = sserver.recvfrom(MAXMSG)
    xid, prog, vers, proc, args = rpcmsg.decode_call(data)
    msg = handle(xid, prog, vers, proc, args)
    msg = rpcmsg.encode_reply(xid, msg)
    sserver.sendto(msg, add)
    return msg

# EOF
