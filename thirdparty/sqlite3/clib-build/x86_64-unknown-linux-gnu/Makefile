all: libsqlite3.a
libsqlite3.a:
	$(CC) -c -o sqlite3.o ../../source-repo/sqlite3.c
	$(AR) cr libsqlite3.a sqlite3.o
