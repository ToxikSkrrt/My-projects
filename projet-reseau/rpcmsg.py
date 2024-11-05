#!/usr/bin/python3

# RPC Message Module
# Author(s):

import xdrlib

###############################################
###             RPC CONSTANTS               ###
###############################################

CALL = 0
REPLY = 1
RPC_VERSION = 2
AUTH_NONE = 0
MSG_ACCEPTED = 0
MSG_DENIED = 1
SUCCESS = 0
CALL_HDR_LEN = 40
REPLY_HDR_LEN = 24

###############################################
###             ENCODE CALL                 ###
###############################################


def encode_call(xid, prog, vers, proc, data) -> bytes:
    """
    >>> encode_call(1, 1, 1, 1, b'ABCD').hex()
    '0000000100000000000000020000000100000001000000010000000000000000000000000000000041424344'
    """
    p = xdrlib.Packer()
    for i in [xid, CALL, RPC_VERSION, prog, vers, proc, AUTH_NONE, 0, AUTH_NONE, 0]:
        p.pack_uint(i)
    msg = p.get_buffer()
    msg += data
    return msg

###############################################
###             ENCODE REPLY                ###
###############################################


def encode_reply(xid, data) -> bytes:
    """
    >>> encode_reply(1, b'ABCD').hex()
    '00000001000000010000000000000000000000000000000041424344'
    """
    msg = b''
    # todo
    p = xdrlib.Packer()
    for i in [xid, REPLY, MSG_ACCEPTED, AUTH_NONE, 0, SUCCESS]:
        p.pack_uint(i)
    msg = p.get_buffer()
    msg += data
    return msg

###############################################
###            DECODE CALL                  ###
###############################################


def decode_call(msg: bytes):
    """
    >>> msg = bytes.fromhex('0000000100000000000000020000000100000001000000010000000000000000000000000000000041424344')
    >>> decode_call(msg)
    (1, 1, 1, 1, b'ABCD')
    """
    u = xdrlib.Unpacker(msg)
    datalen = len(u.get_buffer()) - CALL_HDR_LEN
    xid = u.unpack_uint()
    u.unpack_uint()
    u.unpack_uint()
    prog = u.unpack_uint()
    vers = u.unpack_uint()
    proc = u.unpack_uint()
    for i in range(4):
        u.unpack_uint()
    data = u.unpack_fstring(datalen)
    return (xid, prog, vers, proc, data)

###############################################
###             DECODE REPLY                ###
###############################################


def decode_reply(msg):
    """
    >>> msg = bytes.fromhex('00000001000000010000000000000000000000000000000041424344')
    >>> decode_reply(msg)
    (1, b'ABCD')
    """

    u = xdrlib.Unpacker(msg)
    datalen = len(u.get_buffer()) - REPLY_HDR_LEN
    xid = u.unpack_uint()
    for i in range(5):
        u.unpack_uint()
    data = u.unpack_fstring(datalen)

    return (xid, data)

###############################################
###                MAIN                     ###
###############################################


if __name__ == "__main__":
    import doctest
    doctest.testmod()

# EOF
