//long long callback(OL* ol, int id, word* args) // win32
//long long callback(OL* ol, int id, long long* argi, double* argf, long long* others) // linux
long callback(OL* ol, int id, long* argi
#if __amd64__
		, double* argf, long* rest
#endif
		) // win64
{
// http://stackoverflow.com/questions/11270588/variadic-function-va-arg-doesnt-work-with-float
//	__asm("int $3");
	word* R = ol->R;

	ol->this = (word*)cdr (ol->R[NR + id]); // lambda для обратного вызова
//	ol->ticker = ol->bank ? ol->bank : 999; // зачем это? а не надо, так как без потоков работаем
//	ol->bank = 0;
	assert (is_reference(ol->this));
	assert (reftype (ol->this) != TTHREAD);

	// надо сохранить значения, иначе их уничтожит GC
	// todo: складывать их в память! и восстанавливать оттуда же
	R[NR + 0] = R[0]; // mcp?
//	R[NR + 1] = R[1]; // не надо
//	R[NR + 2] = R[2]; // не надо
	R[NR + 3] = R[3]; // continuation

	// вызовем колбек:
	R[0] = IFALSE;  // отключим mcp, мы пока не работаем с потоками из callback функций
	R[3] = IRETURN; // команда выхода из колбека
	ol->arity = 1;

	word types = car(ol->R[NR + id]);

	int a = 4;
//	R[a] = IFALSE;

	word* fp;
	fp = ol->heap.fp;

	int i = 0;
#if __amd64__ && __linux__
	int j = 0;
#endif
/*#if __amd64__  // !!!
	#if _WIN64
//	rest -= 4;
	#else
	rest -= 6;
	#endif
#endif*/
//	int f = 0; // linux
	while (types != INULL) {
		switch (car(types)) {
		case F(TVPTR): {
			void*
			#if __amd64__
				#if _WIN64
				value = i <= 4
				        ? *(void**) &argi[i]
				        : *(void**) &rest[i-4];
				#else
				value = i <= 6
						? *(void**) &argi[i]
						: *(void**) &rest[i-6]; // ???
				#endif
				i++;
			#else
				value =   *(void**) &argi[i++];
			#endif
			R[a] = (word) new_vptr(value);
			break;
		}
		case F(TINT): {
			int
			#if __amd64__
				#if _WIN64
				value = i <= 4
				        ? *(int*) &argi[i]
				        : *(int*) &rest[i-4];
				#else
				value = i <= 6
						? *(int*) &argi[i]
						: *(int*) &rest[i-6]; // ???
				#endif
				i++;
			#else
				value =   *(int*) &argi[i++];
			#endif
			R[a] = F(value);
			break;
		}
		case F(TFLOAT): {
			float
			#if __amd64__
				#if _WIN64
				value = i <= 4
				        ? *(float*) &argf[i]
				        : *(float*) &rest[i-6];
				i++;
				#else
				value = j <= 8
						? *(float*) &argf[j]
						: *(float*) &rest[j-8]; // ???
				j++;
				#endif
			#else
				value =   *(float*) &argi[i++];
			#endif
			long n = value * 10000; // todo: change to something like ftosn
			long d = 10000;
			// максимальная читабельность?
			R[a] = (word)new_pair(TRATIONAL, itosv(n), itouv(d));
			break;
		}
		case F(TDOUBLE): {
			double
			#if __amd64__
				#if _WIN64
				value = i <= 4
				        ? *(double*) &argf[i]
				        : *(double*) &rest[i-4];
				i++;
				#else
				value = i <= 8
						? *(double*) &argf[j]
						: *(double*) &rest[j-8]; // ???
				j++;
				#endif
			#else
				value =   *(double*) &argi[i++]; i++;
			#endif
			long n = value * 10000; // todo: change to something like ftosn
			long d = 10000;
			// максимальная читабельность?
			R[a] = (word)new_pair(TRATIONAL, itosv(n), itouv(d));
			break;
		}
		case F(TSTRING): {
			void*
			#if __amd64__
				#if _WIN64
				value = i <= 4
				        ? *(void**) &argi[i]
				        : *(void**) &rest[i-4];
				#else
				value = i <= 6
						? *(void**) &argi[i]
						: *(void**) &rest[i-6]; // ???
				#endif
				i++;
			#else
				value =   *(void**) &argi[i++];
			#endif
			R[a] = (word) new_string(value);
			break;
		}
//		case F(TVOID):
//			R[a] = IFALSE;
//			i++;
//			break;
		default:
			STDERR("unknown argument type");
			break;
		}
		a++;
		ol->arity++;
		types = cdr(types);
	}

	ol->heap.fp = fp;

   word result = runtime(ol);
	// возврат из колбека,
	// R, NR могли измениться
	R = ol->R;
	R[3] = R[NR + 3];
//	R[1] = R[NR + 2]; // не надо
//	R[1] = R[NR + 1]; // не надо
	R[0] = R[NR + 0]; // ??? может лучше IFALSE, ведь прежний R0 уже мог стать недействительным?

	// if result must be float or double,
	// do the __ASM__ with loading the result into fpu/xmm register

	return 0;
}
