from glob import iglob
import re
from testutil import target_sources, available_lines, annotate_error
from typing import Optional, List, Tuple

def find_unwrap_position_in_line(line: str) -> Optional[int]:
    result = re.search(r"^unwrap\(\)|(?<=[\s.])unwrap\(\)", line)
    return result.start() if result is not None else None

found_unwrap_vulnerabilities = [] # type: List[Tuple[str, int, int]]
for fpath in target_sources():
    with open(fpath, encoding="utf-8") as fp:
        unwraps = ((i, find_unwrap_position_in_line(line)) for i, line in available_lines(fp))
        found_unwrap_vulnerabilities.extend((fpath, i, p.start()) for i, p in unwraps if p is not None)

if not found_unwrap_vulnerabilities:
    print("No unwraps found")
else:
    for fpath, line, col in found_unwrap_vulnerabilities:
        annotate_error(fpath, line, col, "Unwrap Found! Use `expect` with message instead.")
    exit(1)
