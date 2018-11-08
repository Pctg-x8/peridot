from glob import iglob
import re

pat_comment_line = re.compile(r"^\s*//")

found_unwrap_vulnerabilities = []
for fpath in filter(lambda p: not p.startswith("extras/"), iglob("**/*.rs", recursive=True)):
    with open(fpath) as fp:
        for i, line in filter(lambda x: not pat_comment_line.match(x[1]), enumerate(iter(fp.readline, ""), 1)):
            if "unwrap()" in line: found_unwrap_vulnerabilities.append((fpath, i, line.replace("unwrap", "\033[1;31munwrap\033[0m")))

if not found_unwrap_vulnerabilities:
    print("No unwraps found")
else:
    for fpath, line, text in found_unwrap_vulnerabilities:
        print(f"Unwrap Found! Use `expect` with message instead: {fpath}:{line}")
        print(text.rstrip())
    exit(1)
