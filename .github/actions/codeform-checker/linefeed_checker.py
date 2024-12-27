from testutil import target_sources, annotate_error
from io import BufferedIOBase
from typing import Iterator

def chunked_read(stream: BufferedIOBase, buf_size: int) -> Iterator[bytes]:
    buf = bytearray(buf_size)
    while stream.readinto1(buf):
        yield buf

incorrect_line_feeds = []
for fpath in target_sources():
    with open(fpath, "rb") as fp:
        if any(b'\r' in buf for buf in chunked_read(fp, 64 * 1024)):
            incorrect_line_feeds.append(fpath)

if not incorrect_line_feeds:
    print("All files are terminated with LF")
else:
    for fpath in incorrect_line_feeds:
        annotate_error(fpath, 1, 1, f"This file contains CR character")
    exit(1)
