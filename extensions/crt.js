  // global wasm constants
  const PAGE_SIZE = 65536;
  const decoder = new TextDecoder("utf-8");
  const encoder = new TextEncoder();

  // # environment tests
  // ## JSPI
  (async () => {
    if (typeof WebAssembly?.Suspending !== "function" || typeof WebAssembly?.promising !== "function") {
      console.log("JSPI API missing");
      return;
    }

    // Минимальный валидный WASM-модуль:
    // импортирует env.delay_add(i32) -> i32, экспортирует run(i32) -> i32: возвращает delay_add(x) + 1
    const bytes = new Uint8Array([
      0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
      // Type section (id=1, len=6): 1 type: (i32) -> i32
      0x01, 0x06, 0x01, 0x60, 0x01, 0x7f, 0x01, 0x7f,
      // Import section (id=2, len=17): 1 import: "env"."delay_add" func type 0
      0x02, 0x11, 0x01, 0x03, 0x65, 0x6e, 0x76, 0x09,
      0x64, 0x65, 0x6c, 0x61, 0x79, 0x5f, 0x61, 0x64, 0x64, 0x00, 0x00,
      // Function section (id=3, len=2): 1 func using type 0
      0x03, 0x02, 0x01, 0x00,
      // Export section (id=7, len=7): 1 export: "run" -> func 1
      0x07, 0x07, 0x01, 0x03, 0x72, 0x75, 0x6e, 0x00, 0x01,
      // Code section (id=10, len=11): 1 body (len=9): 0 locals, local.get 0, call 0, i32.const 1, i32.add, end
      0x0a, 0x0b, 0x01, 0x09, 0x00, 0x20, 0x00, 0x10, 0x00, 0x41, 0x01, 0x6a, 0x0b,
    ]);
    const asyncImport = new WebAssembly.Suspending(
      (v) => new Promise((resolve) => setTimeout(() => resolve(v * 10), 50))
    );

    const { instance } = await WebAssembly.instantiate(bytes, {
      env: { delay_add: asyncImport },
    });

    const runAsync = WebAssembly.promising(instance.exports.run);
    const res = await runAsync(4); // ожидаем (4 * 10) + 1 = 41
    console.log("JSPI suspension result:", res, res === 41 ? "OK" : "FAIL");
  })();

  // --------------------
  // external libraries
  const test_id = 1;
  const test = {
    "hello": (x) => {
        console.log("hello", x);
        return 42;
      },
  }



  // .. and functions

  // let timerId = null;
  // function armTimer() {
  //   if (timerId !== null) {
  //     clearTimeout(timerId);
  //   }
  //   timerId = setTimeout(flush, 1000);
  // }

  // function flush() {
  //   console.log("flush")
  //   if (timerId !== null) {
  //     clearTimeout(timerId);
  //     timerId = null;
  //   }
  //   if (stderr.length > 0) {
  //     const out = stderr;
  //     stderr = "";
  //     console.error(out);
  //   }
  // }

  async function preloadFile(name, url = name) {
    const res = await fetch(url, { cache: 'reload' });
    if (!res.ok) {
      throw new Error(`Failed to load ${url}: ${res.status}`);
    }
    const buf = await res.arrayBuffer();
    return { name: name, data: new Uint8Array(buf) };
  }

  function Cruntime()
  {
    let exports = null;

    let memory = null;
    let malloc = null;

    function setInt(i32, value) {
      const mem = new Int32Array(memory.buffer);
      mem[i32 >> 2] = value;
    };
    let setLong = setInt;

    function setUInt(u32, value) {
      const mem = new Uint32Array(memory.buffer);
      mem[u32 >>> 2] = value;
    };
    let setULong = setUInt;

    function readCString(ptr) {
      const bytes = new Uint8Array(memory.buffer);
      let end = ptr;
      while (bytes[end] !== 0) end++;
      return decoder.decode(bytes.subarray(ptr, end));
    }

    function setCString(ptr, value, maxLen) {
      const bytes = new Uint8Array(memory.buffer);
      const encoded = encoder.encode(String(value));
      let out = encoded;

      if (maxLen !== undefined) {
        if (maxLen <= 0) return;
        if (encoded.length >= maxLen) {
          out = encoded.subarray(0, maxLen - 1);
        }
      }

      bytes.set(out, ptr);
      bytes[ptr + out.length] = 0;
    }

    // stdio
    let stdout = "";
    let stderr = "";

    // files
    const files = new Map(); // имя файла -> Uint8Array

    function __mmap(name, data) {
      if (!data)
        throw new Error(`File not preloaded: ${name}`);

      // window.wasmPrint(`crt: __mmap ${name} (${data.length})\n`)
      const ptr = data.length > 0 ? malloc(data.length) : 0;
      if (data.length > 0 && !ptr)
        throw new Error(`malloc failed for ${name} (${data.length} bytes)`);

      // console.log(`  data.length = ${data.length}`)
      if (data.length > 0) {
        new Uint8Array(exports.memory.buffer, ptr, data.length).set(data);
      }

      let file = { ptr, data, len: data.length };
      files.set(name, file);
      return file;
    }

    function __memcpy(dst, src, n) {
      console.log("***  memcpy");
      dst = dst >>> 0;
      src = src >>> 0;

      const heap = new Uint8Array(memory.buffer);
      heap.copyWithin(dst, src, src + n);
      return dst;
    }

    let nextFd = 3;
    const fds = new Map();   // fd -> { data: Uint8Array, ptr: number, end: number, cursor: number }
    function __open(file, flags, mode) {
      let fd = nextFd++;
      fds.set(fd, {
        ptr: file.ptr,
        end: file.len,
        cursor: 0,
        data: file.data
      });
      return fd;
    }

    // -- math fallback functions -----------------------
    // these math functions are fallback functions that usually should not be called
    const mathFallbacks = 
      ["sqrt", "sin", "asin", "cos", "acos", "tan", "atan", "log", "exp", "pow", "floor", "atan2"]
        .reduce((acc, name) => {
          acc[name] = (...args) => {
              console.error(` Math fallback ${name}(${args.join(", ")})`);
              return Math[name](...args);
          };
          acc[name+"f"] = (...args) => {
              console.error(` Math fallback ${name}f(${args.join(", ")})`);
              return Math.fround(Math[name](...args));
          };
          return acc;
        }, {});

    // -- dynamic libraries -----------------------
    // these math functions are fallback functions that usually should not be called
    function findTableIndex(table, fn) {
      for (let i = 0; i < table.length; i++) {
        // console.log(`i = ${i}, -> ${table.get(i)}`)
        if (table.get(i) === fn) {
          return i;
        }
      }
      return 0;
    }
    function registerFunction(table, fn) {
      const index = table.length;
      table.grow(1, fn); // или fnTable.grow(1); fnTable.set(index, fn);
      return index;
    }

    // ??????????????????????????????????
// Кодирует массив байт в LEB128 (нужен для длин секций/строк)
function encodeULEB128(value) {
  const out = [];
  do {
    let byte = value & 0x7f;
    value >>>= 7;
    if (value !== 0) byte |= 0x80;
    out.push(byte);
  } while (value !== 0);
  return out;
}

function encodeString(str) {
  const bytes = new TextEncoder().encode(str);
  return [...encodeULEB128(bytes.length), ...bytes];
}

function section(id, content) {
  return [id, ...encodeULEB128(content.length), ...content];
}

// paramTypes и resultTypes — массивы Wasm-кодов типов, например [0x7f, 0x7f] и [0x7f]
// TODO: reuse for same arguments, по ключу paramTypes.join(',') + ':' + resultTypes.join(',')
function makeTrampolineWasm(paramTypes, resultTypes) {
  const header = [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00];

  // Type section (id = 1): 1 тип функции
  const funcType = [
    0x60,
    ...encodeULEB128(paramTypes.length), ...paramTypes,
    ...encodeULEB128(resultTypes.length), ...resultTypes,
  ];
  const typeSec = section(1, [0x01, ...funcType]);

  // Import section (id = 2): импортируем "env"."fn" как func с типом 0
  const importSec = section(2, [
    0x01,
    ...encodeString("env"),
    ...encodeString("fn"),
    0x00, // kind = func
    0x00, // typeidx = 0
  ]);

  // Export section (id = 7): экспортируем func 0 как "fn"
  const exportSec = section(7, [
    0x01,
    ...encodeString("fn"),
    0x00, // kind = func
    0x00, // funcidx = 0
  ]);

  // Code section (id = 10): 0 тел функций (функция импортирована, не определена в модуле)
  const codeSec = section(10, [0x00]);

  return new Uint8Array([
    ...header,
    ...typeSec,
    ...importSec,
    ...exportSec,
    ...codeSec,
  ]);
}

const I32 = 0x7f; // int, unsigned int, ...
const VPTR = 0x7f; // same as I32, void*, char*, etc.
const I64 = 0x7e; // long long, (u)int64_t. для i64 JS должен вернуть BigInt, а не обычный Number
const FLOAT = 0x7d;
const DOUBLE = 0x7c;
// externref = 0x6f, funcref = 0x70

function jsToWasmFunc(jsFn, paramTypes, resultTypes) {
  const bytes = makeTrampolineWasm(paramTypes, resultTypes);
  const mod = new WebAssembly.Module(bytes);
  const inst = new WebAssembly.Instance(mod, { env: { fn: jsFn } });
  return inst.exports.fn;
}

// Если таблица уже позволяет рост (--growable-table) или есть свободный слот:
function putInTable(table, wasmFn) {
  // если функции нет, можем использовать ее:
  // for (let i = 0; i < table.length; i++) {
  //   if (table.get(i) === null) {
  //     table.set(i, wasmFn);
  //     return i;
  //   }
  // }
  const idx = table.length;
  console.log("putInTable: ", table.length);
  table.grow(1, wasmFn);
  return idx;
}

// const wrapped = jsToWasmFunc((a, b) => a + b, [I32, I32], [I32]);
// const ptr = putInTable(fnTable, wrapped);
// return ptr;

// ?????????????????????????    


    // --------------------------------------------------
    // ok
    return {
      bind(instance) {
        exports = instance.exports;
        memory = instance.exports.memory; // todo: remove
        malloc = instance.exports.malloc; // todo: remove

        console.log("OLVM_ffi: ", instance.exports.OLVM_ffi);
        return instance.exports;
      },
      mmap: __mmap,
      main(...args) {
        const { memory, malloc, free, __main_argc_argv } = exports;
        const enc = new TextEncoder();

        const argc = args.length;

        const encoded = args.map((s) => enc.encode(s));
        const argvBytes = (argc + 1) * 4;
        let stringsBytes = 0;
        for (const b of encoded) {
          stringsBytes += b.length + 1;
        }

        const totalBytes = argvBytes + stringsBytes;
        const argv = malloc(totalBytes);

        let strings = argv + argvBytes;

        const mem8 = new Uint8Array(memory.buffer);
        const mem32 = new Uint32Array(memory.buffer);
        const argvIdxBase = argv >>> 2;

        for (let i = 0; i < encoded.length; i++) {
          const b = encoded[i];
          mem8.set(b, strings);
          mem8[strings + b.length] = 0;
          mem32[argvIdxBase + i] = strings;
          strings += b.length + 1;
        }
        mem32[argvIdxBase + encoded.length] = 0;

        return WebAssembly.promising(__main_argc_argv)(argc, argv);
      },

      env: {
        __wasm_grow: (pages) => {
          const prev = memory.grow(pages);
          // console.info(`__wasm_grow(${pages}) -> ${prev}`);
          if (prev === -1) return 0;
          return (prev + pages) * PAGE_SIZE;
        },

        // stdio.h
        open: new WebAssembly.Suspending((pathPtr, flags, mode) => {
          const path = readCString(pathPtr).replace(/^(?:\.\/)+/, "");
          console.log("path: ", path);

          let file = files.get(path);
          if (file)
            return __open(file, flags, mode);

          return (async () => {
            try {
              const res = await preloadFile(path);
              console.log(`loaded ${path}, ${res.data.length} bytes length`);

              const file = __mmap(path, res.data);
              console.log(`${path} mapped`)
              return __open(file, flags, mode);

            } catch (error) {
              console.error("[open] async error:", error);
              setInt(exports.errno.value, 2); // ENOENT
              return -1;
            }
          })();
        }),
        close: (fd) => {
          console.info("close", fd);

          let file = fds.get(fd);
          if (!file) {
            setInt(exports.errno.value, 2); // ENOENT
            return -1;
          }
          fds.delete(fd);
          return 0;
        },

        read: (fd, dstPtr, count) => {
          // todo: handle stdin
          console.log("read")
          let file = fds.get(fd);
          if (!file) {
            setInt(exports.errno.value, 2); // ENOENT
            return -1;
          }

          let len = Math.min(file.end - file.cursor, count);

          let begin = file.ptr + file.cursor;
          let end = begin + len;

          let mem = new Uint8Array(memory.buffer);
          mem.set(mem.subarray(begin, end), dstPtr);

          file.cursor += len;
          return len;
        },
        write: (fd, bufPtr, len) => {
          if (!memory || len <= 0) {
              return 0;
          }
          const bytes = new Uint8Array(memory.buffer, bufPtr, len);

          if (fd == 1) {
            const text = new TextDecoder("utf-8").decode(bytes);
            stdout += text;

            let i;
            while ((i = stdout.indexOf("\n")) !== -1) {
              const line = stdout.slice(0, i + 1);
              stdout = stdout.slice(i + 1);
              window.wasmPrint(line)
            }
          }
          if (fd === 2) {
            const text = new TextDecoder("utf-8").decode(bytes);
            stderr += text;

            let i;
            while ((i = stderr.indexOf("\n")) !== -1) {
              const line = stderr.slice(0, i + 1);
              stderr = stderr.slice(i + 1);
              window.wasmPrint(line); // `\x1b[31m${line}\x1b[0m`
            }
            // if (stderr.length > 0) {
            //   armTimer();
            // }
          } else {
            // todo: write to file
            const text = new TextDecoder("utf-8").decode(bytes);
            console.info(text);
          }
          return len;
        },


        fstat: (fd, stat) => {
          let file = fds.get(fd);
          if (!file) {
            setInt(exports.errno.value, 2); // ENOENT
            return -1;
          }

          setULong(stat +  0, 0); // st_dev   /* Device.  */
          setULong(stat +  4, 0); // st_ino   /* File serial number.  */
          setInt  (stat +  8, 0); // st_mode; /* File mode.  */
          setInt  (stat + 12, 0); // st_nlink;/* Link count.  */
          setInt  (stat + 16, 0); // st_uid;  /* User ID of the file's owner.  */
          setInt  (stat + 20, 0); // st_gid;  /* Group ID of the file's group. */
          setLong (stat + 24, 0); // st_rdev; /* Device number, if device.  */
          setLong (stat + 28, 0); // __pad1;
          setLong (stat + 32, file.end); // st_size; /* Size of file, in bytes.  */

          return 0;
        },

        fcntl: (fd, cmd, args) => {
          // console.error(` *** fcntl ${fd}, ${cmd}, ${args}`);

          switch (cmd) {
            default:
              return -1;
          }
        },

        // flush: () => {
        //   errno.value = 0;
        // },

        // not implemented yet
        assert: (assertion) => {
          if (!assertion)
            console.error("assert!");
        },
        // _UTSNAME_LENGTH = 65
        uname: (structPtr) => {
          setCString(structPtr + 65*0, "Wasm", 65); // sysname
          setCString(structPtr + 65*1, location.hostname, 65); // nodename
          setCString(structPtr + 65*2, "", 65); // release
          setCString(structPtr + 65*3, navigator.appVersion, 65); // version
          setCString(structPtr + 65*4, "wasm32", 65); // machine
          return 0;
        },

        memcpy: __memcpy,


        mkstemp: (x) => { console.error(" *** mkstemp"); return 0; },
        unlink: (x) => { console.error(" *** unlink"); return 0; },
        send: (x) => { console.error(" *** send"); return 0; },

        stat: (x) => { console.error(" *** stat"); return 0; },

        abort: () => {
          console.error(`*** abort()`);
          throw {};
        },
        exit: (code) => {
          console.error(`*** exit(${code})`);
          throw { exitCode: code | 0 };
        },

        signal: (x) => { return 0; },
        modf: (x) => { console.error(" *** modf"); return 0; },
        longjmp: (x) => { console.error(" *** longjmp"); return 0; },
        gettimeofday: (x) => { console.error(" *** gettimeofday"); return 0; },
        lseek: (x) => { console.error(" *** lseek"); return 0; },
        lstat: (x) => { console.error(" *** lstat"); return 0; },
        fsync: (x) => { console.error(" *** fsync"); return 0; },
        pipe: (x) => { console.error(" *** pipe"); return 0; },
        dup: (x) => { console.error(" *** dup"); return 0; },
        dup2: (x) => { console.error(" *** dup2"); return 0; },
        tcgetattr: (x) => { console.error(" *** tcgetattr"); return 0; },
        nanosleep: (x) => { console.error(" *** nanosleep"); return 0; },
        dlclose: (x) => { console.error(" *** dlclose"); return 0; },
        dlerror: (x) => { console.error(" *** dlerror"); return 0; },
        time: (x) => { console.error(" *** time"); return 0; },
        localtime: (x) => { console.error(" *** localtime"); return 0; },
        strftime: (x) => { console.error(" *** strftime"); return 0; },
        getrlimit: (x) => { console.error(" *** getrlimit"); return 0; },
        getrusage: (x) => { console.error(" *** getrusage"); return 0; },
        sysinfo: (x) => { console.error(" *** sysinfo"); return 0; },
        unsetenv: (x) => { console.error(" *** unsetenv"); return 0; },
        setenv: (x) => { console.error(" *** setenv"); return 0; },
        getenv: (x) => { console.error(" *** getenv"); return 0; },
        strchr: (x) => { console.error(" *** strchr"); return 0; },
        system: (x) => { console.error(" *** system"); return 0; },

        ...mathFallbacks,

        setjmp: (x) => { console.error(" *** setjmp"); return 0; },

        console_log: (x) => {
          console.log(readCString(x));
        },

        // ----------------------------
        dlopen: (path, flags) => {
          console.error(` *** dlopen(${path}${path ? ': "' + readCString(path) + '"' : ''}, ${flags})`);
          if (path == 0)
            return -1;
          path = readCString(path)
          if (path == "test.so")
            return 1;
        },
        dlsym: (handle, symbol) => {
          console.error(` *** dlsym(${handle}, ${readCString(symbol)}`);
          symbol = readCString(symbol);

          if (handle == -1) {
            const fn = exports[symbol];
            console.log("HANDLE -1, fn: ", fn)
            console.log(exports.__indirect_function_table)
            if (typeof fn === "function") {
              const idx = findTableIndex(exports.__indirect_function_table, fn);
              if (idx !== 0) {
                console.log("IDX: ", idx);
                return idx;
              }
              let index = registerFunction(exports.__indirect_function_table, fn);
              console.log("new index: ", index)
              return index; // no function found
            }
            console.log("fail")
            return 0;
          }

          // get table from handle
          if (handle == 1) {
            if (symbol == "my_hook") {
              const jsFn = (a, b) => {
                console.error(`my_hook called with ${a}, ${b}`);
                return a + b;
              };
              const wrapped = jsToWasmFunc(jsFn, [I32, I32], [I32]);
              console.log("wrapped: ", wrapped);
              const ptr = putInTable(exports.__indirect_function_table, wrapped);
              console.log("ptr: ", ptr);
              return ptr;
            }
          }
          // if (symbol === "my_hook") {
          //     const jsFn = (a, b) => {
          //       console.error(`my_hook called with ${a}, ${b}`);
          //       return a + b;
          //     };
          //     return registerCallback(jsFn);
          //   }

          return 0;
        },

      }
    }
  };
