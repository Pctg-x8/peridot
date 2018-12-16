from glob import iglob
import re
from testutil import target_sources, available_lines

found_unwrap_vulnerabilities = []
for fpath in target_sources():
    with open(fpath, encoding="utf-8") as fp:
        unwraps = ((i, line) for i, line in available_lines(fp) if "unwrap()" in line)
        vuls = ((fpath, i, line.replace("unwrap", "\033[1;31munwrap\033[0m")) for i, line in unwraps)
        found_unwrap_vulnerabilities.extend(vuls)

if not found_unwrap_vulnerabilities:
    print("No unwraps found")
else:
    for fpath, line, text in found_unwrap_vulnerabilities:
        print("Unwrap Found! Use `expect` with message instead: \033[1m" + fpath + ":" + str(line) + "\033[0m")
        print(text.rstrip())
    exit(1)
