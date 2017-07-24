int failed = 0;
int successful = 0;
int total = 0;

{
	unsigned char* test = { 1, 2, 3, 0 };
	OL* olvm = OL_new(test, NULL);

	void* r = OL_run(olvm, argc, argv);
	OL_free(olvm);
}
