typedef struct OL OL;

OL* vm_start(unsigned char* language);
int vm_stop(OL* vm);

int vm_puts(OL* vm, char *message, int n);
int vm_gets(OL* vm, char *message, int n);

#ifdef __cplusplus
// class OL ...
// тут игра слов OL <> 0L
//	нулевой порог вхождения
//	сокращение от OwlLisp
//	а еще в html - тег нумерованного СПИСКА (еще одна отсылка к lisp)
#endif
