---
layout: page
title:  "Встраивание"
date:   2016-03-30 17:34:00
categories: ru
---

   Otus Lisp можно встроить в свой проект. Для этого надо подключить к проекту olvm.c файл и выставить дефайн EMBEDDED_VM в единичку.

   Вот пример использования:
<pre><code data-language="ol">
// embedded example
#include "olvm_xtra.h"

__attribute__((__visibility__("default")))
word* sample_add(OL* ol, word arguments) {
	word* fp; // memory pointer
	fp = ol->fp;

	word* fa = (word*)car(arguments); arguments = cdr(arguments);
	word* fb = (word*)car(arguments); arguments = cdr(arguments);

	// math
	int a = sftoi(fa);
	int b = sftoi(fb);
	int r = a + b;
	// result
	word* result = F(r);

	ol->fp = fp;//
	return result;
}

int main(int argc, char** argv)
{
    return vm_new(
        "(import (owl pinvoke) (owl io))"
        "(define % (dlopen '() RTLD_LAZY))" // get own handle
        "(define sample_add (dlsym+ % \"sample_add\"))"
        "(print \"sample_add: \""
        "   (sample_add 1 2))"
        "(halt 1)", 0);
}
</code></pre>

   Вот выдержка из внутреннего README, я ее скоро переведу.

<pre>
You can call your own functions from OL code.
Currently this is more complecated. You need:
   a) compile OL with -DHAS_DLOPEN and -DHAS_PINVOKE options
      (don't forget for -DEMBEDDED_VM)
   b) send to vm "(import (owl pinvoke))" (owl/pinvoke.scm must
      be accessible from your executable)
   c) send to vm "(define me (dload '() 1))" where "me" is some
      appropriate variable name or you can use "let, let*" or
      any other construction you like
Now you have two variants - embed OL function or simple C function call.
For embed OL function call you must
   d) declare you function for OL like this
      '(define myFuction (dlsym+ me "functionname"))'
      in this code
      *) "functionname" - function name string. you must export this
         function from your code to make it accessible from OL
      Now your function will receive OL* as furst parameter. This structure
      provides memory pointer and you have low-level access to OL scructures.
      This mechanism will help to provide you OL language native extensions, in
      feature structure will extent by help functions.

For normal C function call (actually platform invoke mechanism) you must
   d) declare you function for OL (via vm_puts, sure) like this
      '(define myFuction (dlsym me type-int+ "functionname" type-string type-int+))'
      in this code
      *) myFunction - any appropriate variable name for your function
         internal OL script name. This variable will associated with
         lambda that can be called in further code.
      *) second argument in (dlsym) is return type of native function
         available types you can check in owl/pinvoke.scm
      *) third (dlsym) arg is function name string. you must export this
         function from your code to make it accessible from OL
      *) other (dlsym) args is argument types for native function, pinvoke
         will try to convert arguments of myFunction lambda to this type.
         available types can be checked in owl/pinvoke.scm too.

         Small comment for this: if you want to get number - let use type-int+,
         if you expect SMALL number - let use type-fix+. No differences between
         types with + and with - are present.
   e) well, now you can freely use your native function like this for example
      '(print (myFunction "str1" 123))'

More information about pinvoke you can get in source files
   lib/sqlite.scm
   lib/opengl.scm and OpenGL/version-X-X.scm
   lib/winapi.scm
   lib/x11.scm

All embedded OL api in progress and can be changed in feature.
</pre>