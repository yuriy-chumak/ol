(define-library (lib lua)
(export
   LUA_OK
   LUA_ERRRUN
   LUA_ERRFILE
   LUA_ERRSYNTAX
   LUA_ERRMEM
   LUA_ERRERR

   LUA_MULTRET

   luaL_newstate
   lua_close
   luaL_openlibs

   lua_CFunction

   luaL_loadstring
   lua_pcall
   lua_pcallk
   lua_settop
   lua_pop
   lua_gettop

   lua_pushinteger
   lua_setglobal
   luaL_dostring

   luaL_checkinteger
   lua_pushinteger
   lua_pushcfunction
   lua_pushcclosure

   luaL_dofile
   luaL_loadfile
   luaL_loadfilex

   lua_getglobal
   lua_isstring
   lua_tostring

   lua_type
   lua_isfunction

   lua_isnumber
   lua_tointeger
   lua_tointegerx
)

(import
   (otus lisp)
   (otus ffi))
(begin
   (setq LUA (or
      (load-dynamic-library "liblua5.2.so")
      (load-dynamic-library "liblua5.2.so.0")
      (runtime-error "Can't load lua library" #null)))

   (setq int fft-int)
   (setq void fft-void)
   (setq size_t& (fft& fft-size-t))
   (setq int& (fft& fft-int))
   (setq lua_State* type-vptr)
   
   (setq lua_Number fft-double)
   (setq lua_Integer fft-signed-long) ; ptrdiff_t

   (define LUA_OK        0)
   (define LUA_ERRRUN    1)
   (define LUA_ERRFILE   2)
   (define LUA_ERRSYNTAX 3)
   (define LUA_ERRMEM    4)
   (define LUA_ERRERR    5)

   (define LUA_MULTRET  -1)

   (define LUA_TNONE    -1)
   (define LUA_TNIL      0)
   (define LUA_TBOOLEAN  1)
   (define LUA_TLIGHTUSERDATA	2)
   (define LUA_TNUMBER   3)
   (define LUA_TSTRING   4)
   (define LUA_TTABLE    5)
   (define LUA_TFUNCTION 6)
   (define LUA_TUSERDATA 7)
   (define LUA_TTHREAD   8)
   (define LUA_NUMTAGS   9)

   (define *lua_CFunction type-callable)
   (define (lua_CFunction f)
      (make-callback (vm:pin (cons
         (cons int (list
            lua_State*))
         f))))

   (define luaL_newstate (LUA lua_State* "luaL_newstate"))
   (define lua_close (LUA void "lua_close" lua_State*))
   (define luaL_openlibs (LUA void "luaL_openlibs" lua_State*))

   (define luaL_loadstring (LUA int "luaL_loadstring" lua_State* type-string))
   (define lua_pcallk (LUA int "lua_pcallk" lua_State* int int int int *lua_CFunction))
   (define lua_settop (LUA void "lua_settop" lua_State* int))
   (define lua_gettop (LUA int "lua_gettop" lua_State*))

   (define lua_pushinteger (LUA void "lua_pushinteger" lua_State* lua_Integer))
   (define lua_setglobal (LUA void "lua_setglobal" lua_State* type-string))

   (define luaL_checkinteger (LUA lua_Integer "luaL_checkinteger" lua_State* int))
   (define lua_pushinteger (LUA void "lua_pushinteger" lua_State* lua_Integer))
   (define lua_pushcclosure (LUA void "lua_pushcclosure" lua_State* *lua_CFunction int))

   (define luaL_loadfilex (LUA int "luaL_loadfilex" lua_State* type-string type-string))

   (define lua_getglobal (LUA void "lua_getglobal" lua_State* type-string))
   (define lua_isstring (LUA int "lua_isstring" lua_State* int))
   (define lua_tolstring (LUA type-string "lua_tolstring" lua_State* int size_t&))

   (define lua_type (LUA int "lua_type" lua_State* int))1

   (define lua_isnumber (LUA int "lua_isnumber" lua_State* int))
   (define lua_tointegerx (LUA lua_Integer "lua_tointegerx" lua_State* int int&))

   (define (lua_pcall L n r f)
      (lua_pcallk L n r f 0 #false))
   (define (lua_pop L n)
      (lua_settop L (- 0 n 1)))
   (define (luaL_dostring L s)
      (define ok (luaL_loadstring L s))
      (if (zero? ok)
         (lua_pcall L 0 LUA_MULTRET 0)
         ok))
   (define (lua_pushcfunction L f)
      (lua_pushcclosure L f 0))

   (define (luaL_loadfile L f)
      (luaL_loadfilex L f #false))
   (define (luaL_dofile L fn)
      (define ok (luaL_loadfile L fn))
      (if (zero? ok)
         (lua_pcall L 0 LUA_MULTRET 0)
         ok))

   (define (lua_tostring L i)
      (lua_tolstring L i #false))

   (define (lua_isfunction L n)
      (eq? (lua_type L n) LUA_TFUNCTION))
   
   (define (lua_tointeger L i)
      (lua_tointegerx L i #false))
))
