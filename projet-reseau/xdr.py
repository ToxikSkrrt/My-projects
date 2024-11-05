#!/usr/bin/python3

# XDR Module
# Author(s):

import xdrlib

###############################################
###               XDR ENCODE                ###
###############################################


def encode_double(val) -> bytes:
    """
    >>> encode_double(1.2).hex()
    '3ff3333333333333'
    """
    data = b''
    # todo
    p = xdrlib.Packer()
    p.pack_double(val)
    data = p.get_buffer()
    return data


def encode_int(val) -> bytes:
    """
    >>> encode_int(-1).hex()
    'ffffffff'
    """
    data = b''
    # todo
    p = xdrlib.Packer()
    p.pack_int(val)
    data = p.get_buffer()
    return data


def encode_uint(val) -> bytes:
    """
    >>> encode_uint(10).hex()
    '0000000a'
    """
    data = b''
    # todo
    p = xdrlib.Packer()
    p.pack_uint(val)
    data = p.get_buffer()
    return data


def encode_bool(val: bool) -> bytes:
    """
    >>> encode_bool(True).hex()
    '00000001'
    """
    data = b''
    # todo
    p = xdrlib.Packer()
    p.pack_bool(val)
    data = p.get_buffer()
    return data


def encode_string(val: str) -> bytes:
    """
    >>> encode_string("hello").hex()
    '0000000568656c6c6f000000'
    """
    data = b''
    # todo
    p = xdrlib.Packer()
    sval = val.encode("ASCII")
    p.pack_string(sval)
    data = p.get_buffer()
    return data


def encode_two_int(val1, val2) -> bytes:
    """
    >>> encode_two_int(-1,2).hex()
    'ffffffff00000002'
    """
    data = b''
    # todo
    p1 = xdrlib.Packer()
    p2 = xdrlib.Packer()
    p1.pack_int(val1)
    p2.pack_int(val2)
    data = p1.get_buffer()+p2.get_buffer()
    return data

###############################################
###             XDR DECODE                  ###
###############################################


def decode_double(data: bytes):
    """
    >>> msg = bytes.fromhex('3ff3333333333333')
    >>> decode_double(msg)
    1.2
    """
    # todo
    p = xdrlib.Unpacker(data)
    p = p.unpack_double()
    return p


def decode_int(data: bytes):
    """
    >>> msg = bytes.fromhex('ffffffff')
    >>> decode_int(msg)
    -1
    """
    # todo
    p = xdrlib.Unpacker(data)
    p = p.unpack_int()
    return p


def decode_uint(data: bytes):
    """
    >>> msg = bytes.fromhex('00000001')
    >>> decode_uint(msg)
    1
    """
    # todo
    p = xdrlib.Unpacker(data)
    p = p.unpack_uint()
    return p


def decode_bool(data: bytes):
    """
    >>> msg = bytes.fromhex('00000001')
    >>> decode_bool(msg)
    True
    """
    # todo
    p = xdrlib.Unpacker(data)
    p = p.unpack_bool()
    return p


def decode_string(data: bytes) -> str:
    """
    >>> msg = bytes.fromhex('0000000568656c6c6f000000')
    >>> decode_string(msg)
    'hello'
    """
    # todo
    p = xdrlib.Unpacker(data)
    p = p.unpack_string()
    p = p.decode('ASCII')
    return p


def decode_two_int(data):
    """
    >>> msg = bytes.fromhex('ffffffff00000002')
    >>> decode_two_int(msg)
    (-1, 2)
    """
    # todo
    p = xdrlib.Unpacker(data)
    p1 = p.unpack_int()
    p2 = p.unpack_int()
    sp = (p1, p2)
    return sp

###############################################
###                MAIN                     ###
###############################################


if __name__ == "__main__":
    import doctest
    doctest.testmod()

# EOF
