#!/usr/bin/python3

# RPC Bind Module
# Author(s):

import xdrlib
import xdr      # module provided by teacher in VPL
import rpcnet   # module provided by teacher in VPL
import rpcmsg   # module provided by teacher in VPL

###############################################
###           CONSTANTS                     ###
###############################################

RPCB_HOST = "localhost"
RPCB_PORT = 111
RPCB_PROG = 100000  # rpcbind / portmap
RPCB_VERS = 4

RPCBPROC_SET = 1
RPCBPROC_UNSET = 2
RPCBPROC_GETADDR = 3

###############################################
###                GETPORT                  ###
###############################################


def getport(xid, prog, vers) -> int:
    port = -1
    print(xid)
    print(prog)
    print(vers)
    args = xdr.encode_uint(prog)
    args = xdr.encode_uint(vers)
    args = xdr.encode_string("udp")
    args = xdr.encode_string("")
    rep = rpcnet.call(RPCB_HOST, RPCB_PORT, xid, RPCB_PROG,
                      RPCB_VERS, RPCBPROC_GETADDR, args)
    port = rep

    # todo
    return port

###############################################
###                 REGISTER                ###
###############################################


def register(xid, prog, vers, port) -> bool:
    # todo
    return False

###############################################
###              UNREGISTER                 ###
###############################################


def unregister(xid, prog, vers) -> bool:
    # todo
    return False

# EOF
