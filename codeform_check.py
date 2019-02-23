from testutil import target_sources, available_lines

exceeded_lines = []
for fpath in target_sources():
    with open(fpath, encoding="utf-8") as fp:
        exceeded_lines.extend((fpath, i) for i, line in available_lines(fp) if len(line) > 120 + 1)

if not exceeded_lines:
    print("No exceeded lines found")
else:
    for fpath, num in exceeded_lines:
        print("This line exceeds 120 characters: \033[1m" + str(fpath) + ":" + str(num) + "\033[0m")
    exit(1)
