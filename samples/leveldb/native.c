#include <leveldb/c.h>
#include <stdio.h>
#include <memory.h>

leveldb_t *db;
leveldb_options_t *options;
char *err;

typedef long long llong;

// keys:
char char_key = 0x7F;
short short_key = 0x7FFF;
int int_key = 0x7FFFFFFF;
llong llong_key = 0x7FFFFFFFFFFFFFFF;

void do_read() {
	char *read;
	size_t len;

	leveldb_readoptions_t *roptions;
	roptions = leveldb_readoptions_create();

#define READ(type) \
	type type##_value = 0; \
	printf("   reading data by \x1b[32m" #type "\x1b[0m key \x1b[33m0x%llx\x1b[0m... ", (long long) type##_key); \
	len = 0; \
	read = leveldb_get(db, roptions, (char*)&type##_key, sizeof(type##_key), &len, &err); \
	if (err != NULL) { \
		fprintf(stderr, "read fail (%s).\n", err); \
		leveldb_free(err); err = NULL; \
		return; \
	} \
	\
	if (read == NULL) \
		printf("no value found.\n"); \
	else \
		printf("0x%llx\n", (long long) *(type*) read); \

	// keys, values:
	READ(char);  // 8 bit key/value
	READ(short); // 16 bit key/value
	READ(int);   // 32 bit key/value
	READ(llong); // 64 bit key/value
}

void do_write() {
	leveldb_writeoptions_t *woptions;
	woptions = leveldb_writeoptions_create();

#define WRITE(type, value) \
	type type##_value = value; \
	printf("   writing data \x1b[34m0x%llx\x1b[0m by \x1b[32m" #type "\x1b[0m key \x1b[33m0x%llx\x1b[0m... ", (long long) type##_value, (long long) type##_key); \
	leveldb_put(db, woptions, (char*) &type##_key, sizeof(type##_key), (char*) &type##_value, sizeof(type##_value), &err); \
	if (err != NULL) { \
		fprintf(stderr, "write fail (%s).\n", err); \
		leveldb_free(err); err = NULL; \
		return; \
	} \
	\
	printf("ok.\n");

	// keys, values:
	WRITE(char, 0x11); // 8 bit key/value
	WRITE(short, 0x1111);   // 16 bit key/value
	WRITE(int, 0x11111111);	  // 32 bit key/value
	WRITE(llong, 0x1111111111111111); // 64 bit key/value
}

void do_delete() {
	leveldb_writeoptions_t *woptions;
	woptions = leveldb_writeoptions_create();

#define DELETE(type) \
	printf("   deleting data by \x1b[32m" #type "\x1b[0m key \x1b[33m0x%llx\x1b[0m... ", (long long) type##_key); \
	leveldb_delete(db, woptions, (char*)&type##_key, sizeof(type##_key), &err); \
	if (err != NULL) { \
		fprintf(stderr, "delete fail (%s).\n", err); \
		leveldb_free(err); err = NULL; \
		return; \
	} \
	\
	printf("ok.\n");

	DELETE(char);
	DELETE(short);
	DELETE(int);
	DELETE(llong);
}

int main(int argc, char** argv) {
	/******************************************/
	/* OPEN */
	printf("Open (or create) database... ");

	options = leveldb_options_create();
	leveldb_options_set_create_if_missing(options, 1);
	db = leveldb_open(options, "testdb", &err);

	if (err != NULL) {
		fprintf(stderr, "Open failed (%s).\n", err);
		leveldb_free(err); err = NULL;
		return 1;
	}

	/* reset error var */
	printf("ok.\n");

	/******************************************/
	printf("Reading existing values:\n");
	do_read();

	/******************************************/
	printf("Writing new values:\n");
	do_write();

	/******************************************/
	printf("Rereading existing values:\n");
	do_read();

	/******************************************/
	printf("Deleting inserted values:\n");
	if ((argc > 1 && strcmp(argv[1], "--cleanup") == 0) ||
		(argc > 2 && strcmp(argv[2], "--cleanup") == 0))
	{
		do_delete();
	}
	else {
		printf("  If you want to delete putted key/values, run './native --cleanup'.\n");
	}


	/******************************************/
	/* CLOSE */
	leveldb_close(db);

	/******************************************/
	printf("Destroy database:\n");
	if ((argc > 1 && strcmp(argv[1], "--destroy") == 0) ||
		(argc > 2 && strcmp(argv[2], "--destroy") == 0))
	{
		printf("   destroying database... ");
		leveldb_destroy_db(options, "testdb", &err);
		if (err != NULL) {
			fprintf(stderr, "Destroy fail (%s).\n", err);
			leveldb_free(err); err = NULL;
			return 1;
		}
		printf("ok.\n");
	}
	else {
		printf("  If you want to destroy the database, run './native --destroy'.\n");
	}

	return(0);
}