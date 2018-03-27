import zlib
from io import BytesIO


def portable_ieee_crc32(byts):
    return zlib.crc32(byts) & 0xffffffff


def write_varint(val, io):
    assert val >= 0
    while val > 127:
        io.write(bytes([128 | (127 & val)]))
        val >>= 7
    io.write(bytes([val]))


def read_varint(io):
    val = shift = 0
    while True:
        byts = io.read(1)
        assert len(byts) == 1
        byt = byts[0]
        val = val | ((127 & byt) << shift)
        shift += 7
        if byt & 128 == 0:
            return val


def write_bytes(byts, io):
    write_varint(len(byts), io)
    io.write(byts)


def read_bytes(io):
    nbyt = read_varint(io)
    byts = io.read(nbyt)
    assert len(byts) == nbyt
    return byts


def write_string(s, io):
    write_bytes(s.encode('utf8'), io)


def read_string(io):
    return read_bytes(io).decode('utf8')


def write_list(items, io):
    write_varint(len(items), io)
    for item in items:
        write_value(item, io)


def read_list(io):
    return [read_value(io) for _ in range(read_varint(io))]


def write_dict(items, io):
    write_varint(len(items), io)
    for key, val in sorted(items.items()):
        write_string(key, io)
        write_value(val, io)


def read_dict(io):
    return dict((read_string(io), read_value(io))
                for _ in range(read_varint(io)))


def read_value(io):
    typ = read_varint(io)
    return {0: lambda _: None,
            1: lambda _: False,
            2: lambda _: True,
            3: read_list,
            4: read_dict,
            5: read_bytes,
            6: read_string,
            7: read_varint}[typ](io)


def write_value(val, io):
    if val is None:
        write_varint(0, io)
    elif val is False:
        write_varint(1, io)
    elif val is True:
        write_varint(2, io)
    elif isinstance(val, list):
        write_varint(3, io)
        write_list(val, io)
    elif isinstance(val, dict):
        write_varint(4, io)
        write_dict(val, io)
    elif isinstance(val, bytes):
        write_varint(5, io)
        write_bytes(val, io)
    elif isinstance(val, str):
        write_varint(6, io)
        write_string(val, io)
    elif isinstance(val, int):
        write_varint(7, io)
        write_varint(val, io)
    else:
        raise TypeError("Don't know how to encode {}".format(repr(val)))


def read_toplevel(io):
    while True:
        byts = io.read(1)
        if len(byts) == 0:
            return None
        byt = byts[0]
        if byt != 255:
            print('skipping byte', byt)
            continue
        claimed_crc32 = read_varint(io)
        byts = read_bytes(io)
        computed_crc32 = portable_ieee_crc32(byts)
        if computed_crc32 != claimed_crc32:
            raise IOError('CRC32 checksum mismatch')
        return read_value(BytesIO(byts))


def write_toplevel(val, io):
    byio = BytesIO()
    write_value(val, byio)
    byts = byio.getvalue()
    io.write(b'\xff')
    write_varint(portable_ieee_crc32(byts), io)
    write_bytes(byte, io)


# gte.write_toplevel({'foo': ['bar', {'baz': 12, 'qux': True}, None]}, f)
# gte.read_toplevel(f)
