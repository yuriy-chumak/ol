#!/usr/bin/env ol

(import (lib lua))

(define L (luaL_newstate))
(luaL_openlibs L)

(lua_pushinteger L 42)
(lua_setglobal L "answer")

(define code "print(answer)")
(when (eq? (luaL_dostring L code) LUA_OK)
   (lua_pop L (lua_gettop L)))

(lua_close L)
(print "ok.")
