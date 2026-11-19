all: libsqlite3.a
libsqlite3.a:
	$(CC) --sysroot $(CARGO_NDK_SYSROOT_PATH) $(CFLAGS) -static-libstdc++ -c -o sqlite3.o ../../source-repo/sqlite3.c
	$(AR) cr libsqlite3.a sqlite3.o
