from testutil import target_sources, available_lines, annotate_error

LINE_LIMIT = 120

exceeded_lines = []
for fpath in target_sources():
    with open(fpath, encoding="utf-8") as fp:
        exceeded_lines.extend((fpath, i) for i, line in available_lines(fp) if len(line) > LINE_LIMIT + 1)

if not exceeded_lines:
    print("No exceeded lines found")
else:
    for fpath, num in exceeded_lines:
        annotate_error(fpath, num, 1, f"This line exceeds {LINE_LIMIT} characters!")
    exit(1)
