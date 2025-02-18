"use strict";var WA = WA||{module:'ol.wasm'};(function(){

// Define print and error functions if not yet defined by the outer html file
var print = WA.print || (WA.print = msg => console.log(msg.replace(/\n$/, '')));
var error = WA.error || (WA.error = (code, msg) => print('[ERROR] ' + code + ': ' + msg + '\n'));
WA.stdin = WA.stdin || "";
// Some global memory variables/definition
var ASM, MEM, MU8, MU32, FPTS = [0,0,0], WASM_STACK_SIZE = 67108864, WASM_HEAP = 67512384, WASM_HEAP_MAX = (WA.maxmem||256*1024*1024); //default max 256MB

// A generic abort function that if called stops the execution of the program and shows an error
var STOP, abort = WA.abort = function(code, msg)
{
	STOP = true;
	error(code, msg);
	throw 'abort';
};

// Reads a string from the wasm memory heap to JavaScript (decoded as UTF8)
var MStrGet = function(ptr, length)
{
	if (length === 0 || !ptr) return '';
	if (!length) { for (length = 0; length != ptr+MU8.length && MU8[ptr+length]; length++); }
	return new TextDecoder().decode(MU8.subarray(ptr, ptr+length));
};

// Set the array views of various data types used to read/write to the wasm memory from JavaScript
var MSetViews = function()
{
	var buf = MEM.buffer;
	MU8 = new Uint8Array(buf);
	MU32 = new Uint32Array(buf);
};

// J is for JavaScript functions requested by the WASM module
var J ={};

var imports =
{
	J: J,
	env:
	{
		__syscall__newselect: () => 0, // does nothing in this wasm context
		__syscall_dup: () => 0, // does nothing in this wasm context
		__syscall_dup3: () => 0, // does nothing in this wasm context
		__syscall_fcntl64: () => 0, // does nothing in this wasm context
		__syscall_fstat64: () => 0, // does nothing in this wasm context
		__syscall_ioctl: () => 0, // does nothing in this wasm context
		__syscall_lstat64: () => 0, // does nothing in this wasm context

		// file open (can only be used to open embedded files)
		__syscall_open: function(path, flags, varargs)
		{
			let filename = MStrGet(path);
			if (filename.startsWith("./"))
				filename = filename.substring(2);
			//console.log('__sys_open: path: ' + filename + ' - flags: ' + flags + ' - mode: ' + MU32[varargs>>2]);
			var section = WebAssembly.Module.customSections(WA.wm, '|'+filename)[0];
			if (!section) return -1;
			return FPTS.push(new Uint8Array(section), 0) - 2;
		},
		__syscall_pipe: () => 0, // does nothing in this wasm context
		__syscall_stat64: () => 0, // does nothing in this wasm context
		__syscall_uname: () => 0, // does nothing in this wasm context
		__syscall_unlink: () => 0, // does nothing in this wasm context
		acos: Math.acos,
		asin: Math.asin,
		atan: Math.atan,
		atan2: Math.atan2,
		cos: Math.cos,
		dlclose: () => 0, // does nothing in this wasm context
		dlerror: () => 0, // does nothing in this wasm context
		dlopen: () => 0, // does nothing in this wasm context
		dlsym: () => 0, // does nothing in this wasm context
		exit: function(status) { abort('EXIT', 'Exit called: ' + status); },
		exp: Math.exp,
		getenv: () => 0, // does nothing in this wasm context

		// Function querying the system time
		gettimeofday: function(ptr) { var now = Date.now(); MU32[ptr>>2]=(now/1000)|0; MU32[(ptr+4)>>2]=((now % 1000)*1000)|0; },
		localtime: () => 0, // does nothing in this wasm context
		log: Math.log,
		longjmp: function() { abort('CRASH', 'Unsupported longjmp called'); },
		mkstemp: () => 0, // does nothing in this wasm context
		pow: Math.pow,
		// sbrk gets called to increase the size of the memory heap by an increment
		sbrk: function(increment)
		{
			var heapOld = WASM_HEAP, heapNew = heapOld + increment, heapGrow = heapNew - MU8.length;
			//console.log('[SBRK] Increment: ' + increment + ' - HEAP: ' + heapOld + ' -> ' + heapNew + (heapGrow > 0 ? ' - GROW BY ' + heapGrow + ' (' + ((heapGrow+65535)>>16) + ' pages)' : ''));
			if (heapNew > WASM_HEAP_MAX) abort('MEM', 'Out of memory');
			if (heapGrow > 0) { MEM.grow((heapGrow+65535)>>16); MSetViews(); }
			WASM_HEAP = heapNew;
			return heapOld;
		},
		setenv: () => 0, // does nothing in this wasm context
		signal: () => 0, // does nothing in this wasm context
		sin: Math.sin,
		strftime: () => 0, // does nothing in this wasm context
		sysinfo: () => 0, // does nothing in this wasm context
		system: () => 0, // does nothing in this wasm context
		tan: Math.tan,

		// Function querying the system time
		time: function(ptr) { var ret = (Date.now()/1000)|0; if (ptr) MU32[ptr>>2] = ret; return ret; },
		unsetenv: () => 0, // does nothing in this wasm context
	},
	wasi_snapshot_preview1:
	{

		// The fd_close clears an opened file buffer
		fd_close: function(fd)
		{
			if (!FPTS[fd]) return 1;
			//console.log('fd_close - fd: ' + fd);
			FPTS[fd] = 0;
			return 0;
		},
		fd_fdstat_get: () => 0, // IO function not emulated

		// The fd_read function can only be used to read data from embedded files in this wasm context
		fd_read: function(fd, iov, iovcnt, pOutResult)
		{
			iov >>= 2;
			if (fd == 0) {
				if (WA.stdin && WA.stdin.length > 0) {
					var char = WA.stdin.charCodeAt(0);
					WA.stdin = WA.stdin.slice(1);
					var ptr = MU32[iov++], len = MU32[iov++];
					MU8[ptr] = char;
					MU32[pOutResult>>2] = 1;
					return 0;
				}
				return 1;
			}
			var buf = FPTS[fd++], cursor = FPTS[fd]|0, ret = 0;
			if (!buf) return 1;
			for (var i = 0; i < iovcnt && cursor != buf.length; i++)
			{
				var ptr = MU32[iov++], len = MU32[iov++];
				var curr = Math.min(len, buf.length - cursor);
				//console.log('fd_read - fd: ' + fd + ' - iovcnt: ' + iovcnt + ' - ptr: ' + ptr + ' - len: ' + len + ' - reading: ' + curr + ' (from ' + cursor + ' to ' + (cursor + curr) + ')');
				MU8.set(buf.subarray(cursor, cursor + curr), ptr);
				cursor += curr;
				ret += curr;
			}
			FPTS[fd] = cursor;
			//console.log('fd_read -     ret: ' + ret);
			MU32[pOutResult>>2] = ret;
			return 0;
		},

		// The fd_seek function can only be used to seek in embedded files in this wasm context
		fd_seek: function(fd, offset_low, offset_high, whence, pOutResult) //seek in payload
		{
			var buf = FPTS[fd++], cursor = FPTS[fd]|0;
			if (!buf) return 1;
			if (whence == 0) cursor = offset_low; //set
			if (whence == 1) cursor += offset_low; //cur
			if (whence == 2) cursor = buf.length - offset_low; //end
			if (cursor < 0) cursor = 0;
			if (cursor > buf.length) cursor = buf.length;
			//console.log('fd_seek - fd: ' + fd + ' - offset_high: ' + offset_high + ' - offset_low: ' + offset_low + ' - whence: ' +whence + ' - seek to: ' + cursor);
			FPTS[fd] = MU32[pOutResult>>2] = cursor;
			MU32[(pOutResult>>2)+1] = 0; // high
			return 0;
		},
		fd_sync: () => 0, // IO function not emulated

		// The fd_write function can only be used to write strings to stdout in this wasm context
		fd_write: function(fd, iov, iovcnt, pOutResult)
		{
			iov >>= 2;
			for (var ret = 0, str = '', i = 0; i < iovcnt; i++)
			{
				// Process list of IO commands, read passed strings from heap
				var ptr = MU32[iov++], len = MU32[iov++];
				if (len < 0) return -1;
				ret += len;
				str += MStrGet(ptr, len);
				//console.log('fd_write - fd: ' + fd + ' - ['+i+'][len:'+len+']: ' + MStrGet(ptr, len).replace(/\n/g, '\\n'));
			}

			// Print the passed string and write the number of bytes read to the result pointer
			print(str);
			MU32[pOutResult>>2] = ret;
			return 0; // no error
		},
	},
};

// JavaScript functions for CORO requested by the WASM module
(function()
{
	window.addEventListener("message", (evt) => { if (evt.data===9) CoroHandler(); });
	var main_data, coro_current, coro_count = 0, coro_nums = {}, coro_asms = [0], org_started = WA.started, org_main; WA.started = ()=>{ (org_started && org_started()); WA.started = org_started; CoroHandler(); }; function CoroHandler() { for (;;) { var nptr = (coro_current>>2)+4, n = MU32[nptr], fn; if (!n) return; if (n == 3) window.requestAnimationFrame(CoroHandler); if (n == 4) window.postMessage(9, "*"); if (n > 4) setTimeout(CoroHandler, n - 5); if (n > 2) { MU32[nptr] = 2; return; } ASM.asyncify_stop_unwind(); if (n == 2) ASM.asyncify_start_rewind(coro_current); if (fn = MU32[nptr-2]) coro_asms[fn](MU32[nptr-1]); else org_main(); } } function CoroCtxSwitch(n) { if (!main_data) { org_main = (ASM.main||ASM.__main_argc_argv||ASM.__original_main||ASM.__main_void||ASM.WajicMain); var ptr = (main_data = coro_current = ASM.malloc(20+WASM_STACK_SIZE))>>2; MU32[ptr+0] = main_data + 20; MU32[ptr+1] = main_data + 20 + WASM_STACK_SIZE; MU32[ptr+2] = 0; MU32[ptr+3] = 0; MU32[ptr+4] = 0; } if (MU32[(coro_current>>2)+4] == 2) { MU32[(coro_current>>2)+4] = 0; ASM.asyncify_stop_rewind(); return false; } MU32[(coro_current>>2)+4] = n; ASM.asyncify_start_unwind(coro_current); return true; }
	J.WaCoroSleep = (ms) => { CoroCtxSwitch(5 + (ms < 0 ? 0 : ms)); };
	J.WaCoroYield = () => { CoroCtxSwitch(4); };
	J.WaCoroInitNew = (fn,fn_wa_export,user_data,stack_size) => { if (!stack_size) stack_size = WASM_STACK_SIZE; fn = coro_nums[fn_wa_export] || (coro_asms[++coro_count] = ASM[MStrGet(fn_wa_export)],coro_nums[fn_wa_export] = coro_count); var res = ASM.malloc(20+stack_size), ptr = res>>2; MU32[ptr+0] = res + 20; MU32[ptr+1] = res + 20 + stack_size; MU32[ptr+2] = fn; MU32[ptr+3] = user_data; MU32[ptr+4] = 1; return res; };
	J.WaCoroFree = (coro) => { ASM.free(coro); };
})();

// Fetch and instantiate the wasm module by passing the prepared import functions for the wasm module
fetch(document.currentScript.getAttribute('data-wasm')).then(r => r.arrayBuffer()).then(r => WebAssembly.instantiate(r, imports)).then(output =>
{
	// Store the module reference in WA.wm
	WA.wm = output.module;

	// Store the module imports and exports
	WA.imports = imports;
	WA.exports = output.instance.exports;

	// Store the list of the functions exported by the wasm module in WA.asm
	WA.asm = ASM = output.instance.exports;

	var started = WA.started;

	// Get the wasm memory object from the module (can be grown with sbrk)
	MEM = ASM.memory;

	// Set the array views of various data types used to read/write to the wasm memory from JavaScript
	MSetViews();

	// Call global constructors
	ASM.__wasm_call_ctors();

	// Allocate 10 bytes of memory to store the argument list with 1 entry to pass to main
	var argc = 1, argv = ASM.malloc(10);

	// Place executable name string "W" after the argv list
	MU8[argv+8] = 87;
	MU8[argv+9] = 0;

	// argv[0] contains the pointer to the executable name string, argv[1] has a list terminating null pointer
	MU32[(argv    )>>2] = (argv + 8)
	MU32[(argv + 4)>>2] = 0;

	ASM.__main_argc_argv(argc, argv);

	// If the outer HTML file supplied a 'started' callback, call it
	if (started) started();
})
.catch(function (err)
{
	// On an exception, if the err is 'abort' the error was already processed in the abort function above
	if (err !== 'abort') abort('BOOT', 'WASM instiantate error: ' + err + (err.stack ? "\n" + err.stack : ''));
});

})();
