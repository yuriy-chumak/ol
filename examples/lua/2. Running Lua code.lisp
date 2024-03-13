#!/usr/bin/env ol

(import (lib lua))

(define L (luaL_newstate))
(luaL_openlibs L)

(when (eq? (luaL_loadstring L "print('Hello, World')") LUA_OK)
   (when (eq? (lua_pcall L 0 0 0) LUA_OK)
      (lua_pop L (lua_gettop L))))

(lua_close L)
(print "ok.")
