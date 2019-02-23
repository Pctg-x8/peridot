from glob import iglob
import re
from typing import Iterable, Tuple

def target_sources():
    return (p for p in iglob("**/*.rs", recursive=True) if not p.startswith("extras/") and not "target/" in p)
def file_lines(fp) -> Iterable[Tuple[int, str]]: return enumerate(iter(fp.readline, ""), 1)

pat_comment_line = re.compile(r"^\s*//")
def not_comment_line(x: str) -> bool: return not pat_comment_line.match(x)
def available_lines(fp) -> Iterable[Tuple[int, str]]: return filter(lambda x: not_comment_line(x[1]), file_lines(fp))
