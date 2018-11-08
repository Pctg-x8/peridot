from glob import iglob

exceeded_lines = []
for fpath in filter(lambda x: not x.startswith("extras/"), iglob("**/*.rs", recursive=True)):
    with open(fpath) as fp:
        lines = enumerate(iter(fp.readline, ""), 1)
        exceeded_lines.extend(map(lambda x: (fpath, x[0]), filter(lambda x: len(x[1]) > 120, lines)))

if not exceeded_lines:
    print("No exceeded lines found")
else:
    for fpath, num in exceeded_lines:
        print(f"This line exceeds 120 characters: \033[1m{fpath}:{num}\033[0m")
    exit(1)
