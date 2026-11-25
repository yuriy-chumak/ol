/*
*/
  // global wasm constants
  const PAGE_SIZE = 65536;
  const decoder = new TextDecoder("utf-8");
  const encoder = new TextEncoder();
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

  async function preloadFile(name, url = `./${name}`) {
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

      files.set(name, { ptr, data, len: data.length });
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

    // ok
    return {
      bind(instance) {
        exports = instance.exports;
        memory = instance.exports.memory; // todo: remove
        malloc = instance.exports.malloc; // todo: remove
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

        return __main_argc_argv(argc, argv);
      },

      env: {
        __wasm_grow: (pages) => {
          const prev = memory.grow(pages);
          console.info(`__wasm_grow(${pages}) -> ${prev}`);
          if (prev === -1) return 0;
          return (prev + pages) * PAGE_SIZE;
        },

        // stdio.h
        open: (pathPtr, flags, mode) => {
          const path = readCString(pathPtr);

          let file = files.get(path);
          if (!file) {
            setInt(exports.errno.value, 2); // ENOENT
            return -1;
          }

          let fd = nextFd++;
          fds.set(fd, {
            ptr: file.ptr,
            end: file.len,
            cursor: 0,
            data: file.data
          })

          return fd;
        },
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
        dlopen: (x) => { console.error(" *** dlopen"); return 0; },
        dlclose: (x) => { console.error(" *** dlclose"); return 0; },
        dlsym: (x) => { console.error(" *** dlsym"); return 0; },
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

        sqrt: (x) => { console.error(" *** sqrt"); return 0; },
        sin: (x) => { console.error(" *** sin"); return 0; },
        cos: (x) => { console.error(" *** cos"); return 0; },
        tan: (x) => { console.error(" *** tan"); return 0; },
        atan: (x) => { console.error(" *** atan"); return 0; },
        log: (x) => { console.error(" *** log"); return 0; },
        exp: (x) => { console.error(" *** exp"); return 0; },
        asin: (x) => { console.error(" *** asin"); return 0; },
        acos: (x) => { console.error(" *** acos"); return 0; },
        floor: (x) => { console.error(" *** floor"); return 0; },
        atan2: (x, y) => { console.error(" *** atan2"); return 0; },
        pow: (x, y) => { console.error(" *** pow"); return 0; },

        setjmp: (x) => { console.error(" *** setjmp"); return 0; },

        console_log: (x) => {
          console.log(readCString(x));
        }
      }
    }
  };
