#include <ol/ol.h>

int main(int argc, char **argv)
{
	ol_t ol;
	OL_new(&ol, NULL);

	OL_eval(&ol, new_string(&ol, "(print \"hello\")"), 0);

	OL_delete(&ol);
}