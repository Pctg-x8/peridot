all: libsqlite3.a
libsqlite3.a:
	$(CC) --sysroot $(ANDROID_NDK_SYSROOT) $(CFLAGS_aarch64_linux_android) -static-libstdc++ -c -o sqlite3.o ../../source-repo/sqlite3.c
	$(AR) cr libsqlite3.a sqlite3.o
