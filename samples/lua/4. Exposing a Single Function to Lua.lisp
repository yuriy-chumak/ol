#!/usr/bin/env ol

(import (lib lua))

(define L (luaL_newstate))
(luaL_openlibs L)

(define multiplication (lua_CFunction
   (lambda (L)
      (let ((a (luaL_checkinteger L 1))
            (b (luaL_checkinteger L 2)))
         (define c (* a b))

         (lua_pushinteger L c)
         ; The number of returned values
         1))))

(lua_pushcfunction L multiplication)
(lua_setglobal L "mul")

(define code "print(mul(7, 8))")

(when (eq? (luaL_dostring L code) LUA_OK)
   (lua_pop L (lua_gettop L)))

(lua_close L)
(print "ok.")
