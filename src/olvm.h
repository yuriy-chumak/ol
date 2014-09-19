typedef struct OL OL;

OL* vm_start(unsigned char* language); // после того, как машина закончит работу, можно просто сделать free()
int vm_alive(OL* vm); // (возможно не нужна) проверяет, что vm еще работает

int vm_puts(OL* vm, char *message, int n);
int vm_stop(OL* vm);
int vm_gets(OL* vm, char *message, int n);
int vm_feof(OL* vm);  // все ли забрали из входящего буфера

#ifdef __cplusplus
class OLvm
{
private:
	OL* ol;
public:
	OLvm(unsigned char* language) { ol = vm_start(language); }
	int stop() { vm_stop(ol); }
	bool alive() { return vm_alive(ol); }

	int puts(char *message, int n) { vm_puts(ol, message, n);
	int gets(char *message, int n) { vm_gets(ol, message, n);

};
// class OL ...
// тут игра слов OL <> 0L
//	нулевой порог вхождения
//	сокращение от OwlLisp
//	а еще в html - тег нумерованного СПИСКА (еще одна отсылка к lisp)
#endif
