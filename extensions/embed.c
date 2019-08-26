#include "embed.h"

ol_t ol;

void ol_eval(char* string)
{
   embed_eval(&ol, new_string(&ol, string), 0);
}

extern unsigned char binary_repl_start[];
void ol_init()
{
   embed_new(&ol, binary_repl_start, 1);
}
