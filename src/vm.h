typedef struct VM VM;

VM* vm_start(unsigned char* language);
int vm_stop(VM* vm);

int vm_puts(VM* vm, char *message, int n);
int vm_gets(VM* vm, char *message, int n);
