#include <kore/kore.h>
#include <kore/http.h>

int		page(struct http_request *);

#include <ol/ol.h>
extern unsigned char tmp_bin[];
#include <stdio.h>

int
page(struct http_request *req)
{
	// printf("%d\n", offsetof(struct http_request, http_body));

	// olvm:
	struct olvm_t* vm;

	vm = OLVM_new(tmp_bin);
	OLVM_userdata(vm, &vm);

	uintptr_t
	r = OLVM_run(vm, 0, 0);
	int pageid = ol2int(r);

	uintptr_t args[] = {
		new_vptr((ol_t*) &vm, req)
	};
	r = OLVM_evaluate(vm,
			OLVM_deref(vm, pageid),
			1, args);
	assert (is_number(r));

	OLVM_delete (vm);
	return ol2int(r);
}
