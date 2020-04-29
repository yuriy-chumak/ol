// emscripten undocumented function:
function abortStackOverflow() {}

function doit(text)
{
   terminal.focus();
//   console.log("text:", text);
   if (text.indexOf("\n") == 0)
      text = text.substring(1);
   terminal.exec(text);
}

// TERMINAL:
var stdInput = ""; //unescape(encodeURIComponent(",load \"init.lisp\"")); // loading the script with initial code
var errorLen = 0;

var terminal;
$('#terminal').terminal(function(command, terminal) {
   var text = unescape(encodeURIComponent(command));
   ga('send', 'event', 'Console', 'eval', text, {
      nonInteraction: true
    });

   // todo: check parenthesis
   terminal.set_prompt('');
   ol_eval(text);
   terminal.set_prompt('> ');
}, {
   prompt: 'Please wait, loading library files...',
   name: 'repl',
   greetings: '',
   enabled: false,
   height: 200,

   onInit: function(term) {
      terminal = term;
      terminal.ready = false;
   }
});

$('#terminal').mousewheel(function(event) {
   terminal.scroll(event.deltaY);
   // console.log(event.deltaX, event.deltaY, event.deltaFactor);
   if (event.preventDefault)  //disable default wheel action of scrolling page
      event.preventDefault();
   else
      return false;
});

var Module = {
//   arguments: ['#', '-', '--embed'],
//   arguments: ['platform', '-'],
   dynamicLibraries: [], //, 'olvm.js', 'repl.wasm', 'oljs.wasm'],
   INITIAL_MEMORY: 67108864,

   preRun: function() {
      console.log("preRun");
      //LibraryManager.library = Module;

      function stdin() {
         if (stdInput.length == 0) {
            return undefined;
         }

         var chr = stdInput.charCodeAt(0);
         stdInput = stdInput.substring(1);
         return chr;
      }
      var stdout = null;
      var stderr = null;
      FS.init(stdin, stdout, stderr);

      Libraries.forEach( function(i) {
         console.log("i: ", i.path + "/" + i.name);
         if (i.path != "/")
            FS.createPath("/", i.path, true, true);
         FS.createDataFile(i.path + "/", i.name, i.data, true, false);
      });
   },
   postRun: function() {
      ol_init = Module.cwrap('ol_init', 'number', []);
      ol_eval = Module.cwrap('ol_eval', 'number', ['string']);

      ol_init();

      terminal.resume();
      terminal.set_prompt('> ');
      terminal.focus();
   },

   print: function(text) {
      if (arguments.length > 1) text = Array.prototype.slice.call(arguments).join(' ');

      if (terminal.ready == false) {
         terminal.ready = true;
         terminal.clear();
      }

/*      // reaction on "(print)" - show canvas
      if (text=="> ") {
         console.log("sssssss")
//         terminal.ready = true;
//         hideTerminal();
      }*/

      // let's process OL's prompt:
/*    terminal.position(1);
      while (text.indexOf("> ") == 0)
         text = text.substring(2);//*/
      terminal.set_prompt('> ');
      terminal.resume();
      terminal.echo(text);

      ga('send', 'event', 'Console', 'stdout', text, {
         nonInteraction: true});


      // well, we got greeting. let's import (lib opengl)
      //if (text == "type ',help' to help, ',quit' to end session.")
      //   terminal.exec("(import (lib gl)) (import (OpenGL version-1-0))");
         //terminal.exec("(import (lib opengl))");
      //if (text == "bye-bye :/")
      //   terminal.pause();
   },
   printErr: function(text) {
      error = (text || "").substring(errorLen);
      terminal.error(error);
      errorLen = (text || "").length;

      ga('send', 'event', 'Console', 'stderr', error, {
         nonInteraction: true});
      showTerminal();
   },
/*   canvas: (function() {
      var canvas = document.getElementById('canvas');

      // please, add to glutCreateWindow() 'preserveDrawingBuffer: true' attribute!
      // it should looks like
      // function _glutCreateWindow(name){var contextAttributes={antialias:(GLUT.initDisplayMode&128)!=0,depth:(GLUT.initDisplayMode&16)!=0,stencil:(GLUT.initDisplayMode&32)!=0,alpha:(GLUT.initDisplayMode&8)!=0,preserveDrawingBuffer:true}

      // As a default initial behavior, pop up an alert when webgl context is lost. To make your
      // application robust, you may want to override this behavior before shipping!
      // See http://www.khronos.org/registry/webgl/specs/latest/1.0/#5.15.2
      canvas.addEventListener("webglcontextlost", function(e) { alert('WebGL context lost. You will need to reload the page.'); e.preventDefault(); }, false);

      // and resize to terminal sizes
      $("#canvas").css({
         'height': $("#terminal").outerHeight(),
         'width':  $("#terminal").outerWidth()
      });
      showTerminal(); // а надо?
      return canvas;
   })(),*/

   setStatus: function(text) {
      console.log("status: [" + text + "]:", typeof(text));
   },

   totalDependencies: 0,
   noExitRuntime: 0,

   onAbort: function(text) {
      console.log("abort: ", text);
   },

   monitorRunDependencies: function(left) {
      console.log("monitorRunDependencies: ", left);
      // this.totalDependencies = Math.max(this.totalDependencies, left);
      // Module.setStatus(left ? 'Preparing... (' + (this.totalDependencies-left) + '/' + this.totalDependencies + ')' : 'All downloads complete.');
   }
};
//Module.setStatus('Downloading OL Virtual Machine');
window.onerror = function(event) {
   // TODO: do not warn on ok events like simulating an infinite loop or exitStatus
   Module.setStatus('onerror: Exception thrown, see JavaScript console', event);
   //spinnerElement.style.display = 'none';
   Module.setStatus = function(text) {
      if (text) Module.printErr('[post-exception status] ' + text);
   };
};

// FILE SYSTEM
var Libraries = [
      //{ path: "/otus", name: "ffi.scm",    file: "https://rawgit.com/yuriy-chumak/ol/master/libraries/otus/ffi.scm" },
      //{ path: "/otus", name: "ffi.scm",    file: "lib/otus/ffi.scm" },
      //{ path: "/lib",  name: "opengl.scm", file: "lib/opengl.scm" },

      //{ path: "/EGL",       name: "version-1-1.scm", file: "https://rawgit.com/yuriy-chumak/ol/master/libraries/EGL/version-1-1.scm" },

      //{ path: "/OpenGL",    name: "platform.scm", file: "https://rawgit.com/yuriy-chumak/ol/master/libraries/OpenGL/platform.scm" },
      //{ path: "/OpenGL",    name: "platform.scm",    file: "lib/OpenGL/platform.scm" },
      //{ path: "/OpenGL",    name: "version-1-0.scm", file: "lib/OpenGL/version-1-0.scm" },
      //{ path: "/OpenGL",    name: "version-1-0.scm", file: "https://rawgit.com/yuriy-chumak/ol/master/libraries/OpenGL/version-1-0.scm" },
      //{ path: "/lib/gl",  name: "config.scm", file: "lib/lib/gl/config.scm" },
      //{ path: "/lib",     name: "gl.scm",     file: "lib/lib/gl.scm" },

//      { path: "/OpenGL/ES", name: "version-2-0.scm", file: "https://rawgit.com/yuriy-chumak/ol/master/libraries/OpenGL/ES/version-2-0.scm" },
//      { path: "/OpenGL/ES", name: "version-2-0.scm", file: "lib/version-2-0.scm" },

      //{ path: "/", name: "test.lisp", file: "test.lisp" },
      //{ path: "/", name: "repl", file: "https://rawgit.com/yuriy-chumak/ol/master/repl" }
   ];
var Downloaded = 0;

/*Libraries.forEach( function(item) {
   $.ajax({
      url: item.file,
      type: 'GET',
      beforeSend: function (xhr) {
         xhr.overrideMimeType("text/plain; charset=x-user-defined");
         console.log("let's download ", item.file);
      },
      error: function(a, b, c) {
         console.log(a);
         console.log(b);
         console.log(c);
      },
      success: function( data ) {
         console.log("ok: ", item.path + "/" + item.name)
         item.data = data;

         if (++Downloaded == Libraries.length) {*/
            // load olvm
            var script = document.createElement('script');
            script.src = "olvm.js"; //javascripts/emscripten-1.37.35.js";

            script.addEventListener('load', function(me) {
                terminal.set_prompt('');
                terminal.echo("Booting Virtual Machine...")
            }, false);
            script.addEventListener('error', function(event) {
                terminal.set_prompt('');
                terminal.echo("Can't find olvm. Build it first and try again.")
            }, false);

            document.body.appendChild(script);
/*         }
      }
   });
});*/
