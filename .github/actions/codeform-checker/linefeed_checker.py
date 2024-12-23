from testutil import target_sources, annotate_error

incorrect_line_feeds = []
for fpath in target_sources():
    with open(fpath, "rb") as fp:
        buf = bytearray(64 * 1024)
        while fp.readinto1(buf):
            if b'\r' in buf:
                incorrect_line_feeds.append(fpath)
                break

if not incorrect_line_feeds:
    print("All files are terminated with LF")
else:
    for fpath in incorrect_line_feeds:
        annotate_error(fpath, 1, 1, f"This file contains CR character")
    exit(1)
