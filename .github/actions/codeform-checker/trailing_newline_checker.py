from testutil import target_sources, annotate_error

has_error = False
for fpath in target_sources("**/*.{{rs,toml,sh,ps1}}"):
    with open(fpath, encoding="utf-8") as fp:
        file_size = fp.seek(0, 2)
        fp.seek(file_size - 1, 0)
        if fp.read(1) != "\n":
            has_error = True
            fp.seek(0, 0)
            lines = fp.readlines()
            annotate_error(fpath, len(lines), 1 + len(lines[-1]), "Source file requires a newline at end of the file")

if has_error: exit(1)
