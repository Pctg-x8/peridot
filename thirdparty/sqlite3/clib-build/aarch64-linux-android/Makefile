all: libsqlite3.a
libsqlite3.a:
	$(CC) --sysroot $(ANDROID_NDK_SYSROOT) -target $(TARGET) -c -o sqlite3.o ../../source-repo/sqlite3.c
	$(AR) cr libsqlite3.a sqlite3.o
