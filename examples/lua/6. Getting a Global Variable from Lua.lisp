#!/usr/bin/env ol

(import (lib lua))

(define L (luaL_newstate))
(luaL_openlibs L)

(when (eq? (luaL_dofile L "script6.lua") LUA_OK)
   (lua_pop L (lua_gettop L)))

(lua_getglobal L "message")

(unless (zero? (lua_isstring L -1))
   (define message (lua_tostring L -1))
   (lua_pop L 1)
   (print "Message from lua: " message))

(lua_close L)
(print "ok.")
