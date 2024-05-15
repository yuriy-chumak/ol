(define-library (lib lua)
(export
   LUA_OK
   LUA_YIELD
   LUA_ERRRUN
   LUA_ERRSYNTAX
   LUA_ERRMEM
   LUA_ERRGCMM
   LUA_ERRERR

   LUA_MULTRET

   ;; state manipulation
   lua_newstate
   lua_close
   lua_newthread
   lua_atpanic
   lua_version
   ; aux
   luaL_newstate

   ;; basic stack manipulation
   lua_absindex
   lua_gettop
   lua_settop
   lua_pushvalue
   lua_remove
   lua_insert
   lua_replace
   lua_copy
   lua_checkstack

   lua_xmove

   ;; access functions (stack -> C)
   lua_isnumber
   lua_isstring
   lua_iscfunction
   lua_isuserdata
   lua_type
   lua_typename

   lua_tonumberx
   lua_tointegerx
   lua_tounsignedx
   lua_toboolean
   lua_tolstring
   lua_rawlen
   lua_tocfunction
   lua_touserdata
   lua_tothread
   lua_topointer

   lua_tonumber
   lua_tointeger
   lua_tounsigned

   ;lua_arith
   ;lua_rawequal
   ;lua_compare

   ;; push functions (C -> stack)
   lua_pushnil
   lua_pushnumber
   lua_pushinteger
   lua_pushunsigned
   lua_pushlstring
   lua_pushstring
   lua_pushvfstring

   lua_pushfstring
   lua_pushcclosure
   lua_pushboolean
   lua_pushlightuserdata
   lua_pushthread

   ;; get functions (Lua -> stack)
   lua_getglobal
   lua_gettable
   lua_getfield
   lua_rawget
   lua_rawgeti
   lua_rawgetp
   lua_createtable
   lua_newuserdata
   lua_getmetatable
   lua_getuservalue

   ;; set functions (stack -> Lua)
   lua_setglobal
   lua_settable
   lua_setfield
   lua_rawset
   lua_rawseti
   lua_rawsetp
   lua_setmetatable
   lua_setuservalue


   ; ...
   luaL_openlibs

   lua_CFunction

   luaL_loadstring
   lua_pcall
   lua_pcallk
   lua_pop

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
   lua_tostring

   lua_type
   lua_isfunction
)

(import
   (otus lisp)
   (otus ffi))

(cond-expand
   ((or Linux FreeBSD NetBSD OpenBSD Android)
      (begin
         (setq LUA (or
            (load-dynamic-library "liblua5.2.so")
            (load-dynamic-library "liblua5.2.so.0")
            (runtime-error "Can't load lua library")))))
   (Windows
      (begin
         (setq LUA (or
            (load-dynamic-library "lua52.dll")
            (load-dynamic-library "lua5.2.dll")
            (runtime-error "Can't load lua library")))))
   (else
      (begin (runtime-error "Unsupported platform OS" *uname*))))

;; ==============================================================
(begin
   (setq int fft-int)
   (setq void fft-void)
   (setq size_t fft-size_t)
   (setq size_t& (fft& size_t))
   (setq int& (fft& fft-int))

   (setq void* type-vptr)
   (setq lua_State* type-vptr)

   (setq lua_Number fft-double)
   (setq lua_Integer fft-signed-long) ; ptrdiff_t, a good choice between int or long
   (setq lua_Unsigned fft-uint32) ; signed integer with exactly 32 bits

   (define *lua_CFunction type-callable)
   (define (lua_CFunction f)
      (make-callback (vm:pin (cons
         (cons int (list
            lua_State*))
         f))))
   ; lua_Reader
   ; lua_Writer

   (define *lua_Alloc type-callable)
   (define (lua_Alloc f)
      (make-callback (vm:pin (cons
         (cons void* (list
         ;  ud    ptr   osize  nsize
            void* void* size_t size_t))
         f))))


   (define LUA_OK        0)
   (define LUA_YIELD     1)
   (define LUA_ERRRUN    2)
   (define LUA_ERRSYNTAX 3)
   (define LUA_ERRMEM    4)
   (define LUA_ERRGCMM   5)
   (define LUA_ERRERR    6)

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

   ;; state manipulation
   (define lua_newstate (LUA lua_State* "lua_newstate" *lua_Alloc void*))
   (define lua_close (LUA void "lua_close" lua_State*))
   (define lua_newthread (LUA lua_State* "lua_newthread" lua_State*))
   (define lua_atpanic (LUA *lua_CFunction "lua_atpanic" lua_State* *lua_CFunction))
   (define lua_version (LUA lua_Number "lua_version" lua_State*))

   (define luaL_newstate (LUA lua_State* "luaL_newstate"))

   ;; basic stack manipulation
   (define lua_absindex (LUA int "lua_absindex" lua_State* int))
   (define lua_gettop (LUA int "lua_gettop" lua_State*))
   (define lua_settop (LUA void "lua_settop" lua_State* int))
   (define lua_pushvalue (LUA void "lua_pushvalue" lua_State* int))
   (define lua_remove (LUA void "lua_remove" lua_State* int))
   (define lua_insert (LUA void "lua_insert" lua_State* int))
   (define lua_replace (LUA void "lua_replace" lua_State* int))
   (define lua_copy (LUA void "lua_copy" lua_State* int int))
   (define lua_checkstack (LUA int "lua_checkstack" lua_State* int))

   (define lua_xmove (LUA void "lua_xmove" lua_State* lua_State* int))

   ;; access functions (stack -> C)
   (define lua_isnumber (LUA int "lua_isnumber" lua_State* int))
   (define lua_isstring (LUA int "lua_isstring" lua_State* int))
   (define lua_iscfunction (LUA int "lua_iscfunction" lua_State* int))
   (define lua_isuserdata (LUA int "lua_isuserdata" lua_State* int))
   (define lua_type (LUA int "lua_type" lua_State* int))
   (define lua_typename (LUA type-string "lua_typename" lua_State* int))

   (define lua_tonumberx (LUA lua_Number "lua_tonumberx" lua_State* int int&))
   (define lua_tointegerx (LUA lua_Integer "lua_tointegerx" lua_State* int int&))
   (define lua_tounsignedx (LUA lua_Unsigned "lua_tounsignedx" lua_State* int int&))
   (define lua_toboolean (LUA int "lua_toboolean" lua_State* int))
   (define lua_tolstring (LUA type-string "lua_tolstring" lua_State* int size_t&))
   (define lua_rawlen (LUA size_t "lua_rawlen" lua_State* int))
   (define lua_tocfunction (LUA *lua_CFunction "lua_tocfunction" lua_State* int))
   (define lua_touserdata (LUA void "lua_touserdata" lua_State* int))
   (define lua_tothread (LUA lua_State* "lua_tothread" lua_State* int))
   (define lua_topointer (LUA void* "lua_topointer" lua_State* int))

   (define (lua_tonumber L i)
      (lua_tonumberx L i #false))
   (define (lua_tointeger L i)
      (lua_tointegerx L i #false))
   (define (lua_tounsigned L i)
      (lua_tounsignedx L i #false))

   ;; push functions (C -> stack)
   (define lua_pushnil (LUA void "lua_pushnil" lua_State*))
   (define lua_pushnumber (LUA void "lua_pushnumber" lua_State* lua_Number))
   (define lua_pushinteger (LUA void "lua_pushinteger" lua_State* lua_Integer))
   (define lua_pushunsigned (LUA void "lua_pushunsigned" lua_State* lua_Unsigned))
   (define lua_pushlstring (LUA type-string "lua_pushlstring" lua_State* type-string size_t))
   (define lua_pushstring (LUA type-string "lua_pushstring" lua_State* type-string))
   (define lua_pushvfstring (LUA type-string "lua_pushvfstring" lua_State* type-string))

   (define lua_pushfstring (LUA type-string "lua_pushfstring" lua_State* type-string)) ;...
   (define lua_pushcclosure (LUA void "lua_pushcclosure" lua_State* *lua_CFunction int))
   (define lua_pushboolean (LUA void "lua_pushboolean" lua_State* int))
   (define lua_pushlightuserdata (LUA void "lua_pushlightuserdata" lua_State* void*))
   (define lua_pushthread (LUA int "lua_pushthread" lua_State*))

   ;; get functions (Lua -> stack)
   (define lua_getglobal (LUA void "lua_getglobal" lua_State* type-string))
   (define lua_gettable (LUA void "lua_gettable" lua_State* int))
   (define lua_getfield (LUA void "lua_getfield" lua_State* int type-string))
   (define lua_rawget (LUA void "lua_rawget" lua_State* int))
   (define lua_rawgeti (LUA void "lua_rawgeti" lua_State* int int))
   (define lua_rawgetp (LUA void "lua_rawgetp" lua_State* int void*))
   (define lua_createtable (LUA void "lua_createtable" lua_State* int int))
   (define lua_newuserdata (LUA void* "lua_newuserdata" lua_State* size_t))
   (define lua_getmetatable (LUA int "lua_getmetatable" lua_State* int))
   (define lua_getuservalue (LUA void "lua_getuservalue" lua_State* int))

   ;; set functions (stack -> Lua)
   (define lua_setglobal (LUA void "lua_setglobal" lua_State* type-string))
   (define lua_settable (LUA void "lua_settable" lua_State* int))
   (define lua_setfield (LUA void "lua_setfield" lua_State* int type-string))
   (define lua_rawset (LUA void "lua_rawset" lua_State* int))
   (define lua_rawseti (LUA void "lua_rawseti" lua_State* int int))
   (define lua_rawsetp (LUA void "lua_rawsetp" lua_State* int type-string))
   (define lua_setmetatable (LUA int "lua_setmetatable" lua_State* int))
   (define lua_setuservalue (LUA void "lua_setuservalue" lua_State* int))

   ;; ...
   (define luaL_openlibs (LUA void "luaL_openlibs" lua_State*))

   (define luaL_loadstring (LUA int "luaL_loadstring" lua_State* type-string))
   (define lua_pcallk (LUA int "lua_pcallk" lua_State* int int int int *lua_CFunction))

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

))
