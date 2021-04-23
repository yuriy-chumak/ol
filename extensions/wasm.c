#include "ol/ol.h"

ol_t ol;

void ol_eval(char* string)
{
   OL_eval(&ol, new_string(&ol, string), 0);
}

extern unsigned char binary_repl_start[];
void ol_init()
{
   OL_new(&ol, binary_repl_start);
}
