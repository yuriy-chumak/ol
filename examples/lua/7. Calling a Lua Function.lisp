#!/usr/bin/env ol

(import (lib lua))

(define L (luaL_newstate))
(luaL_openlibs L)

(when (eq? (luaL_dofile L "script7.lua") LUA_OK)
   (lua_pop L (lua_gettop L)))

(lua_getglobal L "my_function")

(unless (zero? (lua_isfunction L -1))
   (when (eq? (lua_pcall L 0 1 0) LUA_OK)
      (lua_pop L (lua_gettop L))))

(lua_close L)
(print "ok.")
