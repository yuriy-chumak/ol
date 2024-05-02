#!/usr/bin/env ol

(import (lib lua))

(define L (luaL_newstate))
(luaL_openlibs L)

(when (eq? (luaL_dofile L "script8.lua") LUA_OK)
   (lua_pop L (lua_gettop L)))

(lua_getglobal L "my_function")
(lua_pushinteger L 3)
(lua_pushinteger L 4)

(when (eq? (lua_pcall L 2 1 0) LUA_OK)
   (unless (zero? (lua_isnumber L -1))
      (define result (lua_tointeger L -1))
      (lua_pop L 1)

      (print "Result: " result))
   (lua_pop L (lua_gettop L)))

(lua_close L)
(print "ok.")
