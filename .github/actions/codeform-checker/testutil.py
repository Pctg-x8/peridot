from glob import iglob
import re
from typing import Iterable, Tuple
from pathlib import Path
from os import environ

def rawbuildlog_path() -> Path:
    return Path(environ.get("GITHUB_WORKSPACE")).joinpath(".rawbuildlog")

def target_sources(first_matcher: str = "**/*.rs"):
    return (p for p in iglob("**/*.rs", recursive=True) if not p.startswith("extras/") and not "target/" in p)
def file_lines(fp) -> Iterable[Tuple[int, str]]: return enumerate(iter(fp.readline, ""), 1)

pat_comment_line = re.compile(r"^\s*//")
def not_comment_line(x: str) -> bool: return not pat_comment_line.match(x)
def available_lines(fp) -> Iterable[Tuple[int, str]]: return filter(lambda x: not_comment_line(x[1]), file_lines(fp))

def annotate_error(file: str, line: int, col: int, msg: str):
    print(f"::error file={file},line={line},col={col}::{msg}")
    with open(rawbuildlog_path(), mode="wax") as f: f.write(f"* {msg} at {file} line {line} col {col}\n")
