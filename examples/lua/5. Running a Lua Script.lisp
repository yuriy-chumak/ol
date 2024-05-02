#!/usr/bin/env ol

(import (lib lua))

(define L (luaL_newstate))
(luaL_openlibs L)

(when (eq? (luaL_dofile L "script5.lua") LUA_OK)
   (lua_pop L (lua_gettop L)))

(lua_close L)
(print "ok.")
