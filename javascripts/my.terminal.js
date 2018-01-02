if (!("TextEncoder" in window)) 
  alert("Sorry, this browser does not support TextEncoder...");
var utf8 = new TextEncoder("utf-8");
//utf8.encode("This is a string converted to a Uint8Array")

function toUTF8Array(str) {
    var utf8 = [];
    for (var i=0; i < str.length; i++) {
        var charcode = str.charCodeAt(i);
        if (charcode < 0x80) utf8.push(charcode);
        else if (charcode < 0x800) {
            utf8.push(0xc0 | (charcode >> 6), 
                      0x80 | (charcode & 0x3f));
        }
        else if (charcode < 0xd800 || charcode >= 0xe000) {
            utf8.push(0xe0 | (charcode >> 12), 
                      0x80 | ((charcode>>6) & 0x3f), 
                      0x80 | (charcode & 0x3f));
        }
        else {
            // let's keep things simple and only handle chars up to U+FFFF...
            utf8.push(0xef, 0xbf, 0xbd); // U+FFFE "replacement character"
        }
    }
    return utf8;
}

var inputs = [];
var input = null;
var input$ = 0;

var ol;
var ol_loaded;
var ol_failed;

function start_ol()
{
   terminal.pause();
   terminal.set_prompt('');
   ol_failed = undefined;
   setTimeout(function() {
      if (ol == undefined && ol_failed == undefined)
         terminal.echo("Please wait, it's loading...");
   }, 3000);
   jQuery.ajax({
      url: "olvm.js",
      dataType: 'script',
      async: true,
      success: function() {
         ol = this;
         if (ol_loaded != undefined)
            ol_loaded();
      },
      error: function() {
         ol_failed = true;
         terminal.echo("Sorry, can't load ol :( Please try again a bit later.");
         terminal.resume();
      }
   });
}

function doit(text)
{
   if (ol == undefined) {
      terminal.echo("$ ol");
      ol_loaded = function() {
         if (terminal.paused()) {
            setTimeout(ol_loaded, 500);
         }
         else {
            terminal.insert(text);
            terminal.focus();
            ol_loaded = undefined;
         }
      }
      start_ol();
   }
   else {
      terminal.insert(text);
      terminal.focus();
   }
}

var terminal =
$('#terminal').terminal(function(command, terminal) {
   console.log("command: [" + command + "]")

   if (ol == undefined) {
      if (command == 'ol') {
         start_ol();
      }
      else if (command == 'help') {
         terminal.echo("This is local terminal to instantly try provided in the ol project page samples.");
         terminal.echo("Please run 'ol' to start the ol interpreter.");
         terminal.echo("Anyway, it will be started automatically when you'll try to use 'send it to terminal' samples button.");
      }
      else {
         terminal.echo("Sorry, unknown command '"+command+"'. Please try 'help' or 'ol'.");
      }
   }
   else {
      terminal.set_prompt('');
      inputs.push(utf8.encode(command));
   }
}, {
   prompt: '$ ',
   name: 'repl',
   greetings: '/this is your local terminal session,\n    type "ol" to start the Otus Lisp/\n',
   height: 200,
   enabled: true
});

var Module = {
   preRun: function() {
      function stdin() {
         // Return ASCII code of character, or null if no input
         if (input == null) {
            input = inputs.pop() || [];
            input$ = 0;
         }
         if (input$ < input.length)
            return input[input$++];
         input = null;
         // debugger;
         return undefined;
      }

      function stdout(asciiCode) {
         // Do something with the asciiCode
      }

      function stderr(asciiCode) {
         // Do something with the asciiCode
      }

      FS.init(stdin, null, null);
   },
   postRun: [],

   arguments: ["-", "--interactive", "--home", "/ol"],
   print: function(text) {
      if (arguments.length > 1) text = Array.prototype.slice.call(arguments).join(' ');
            // These replacements are necessary if you render to raw HTML
            //text = text.replace(/&/g, "&amp;");
            //text = text.replace(/</g, "&lt;");
            //text = text.replace(/>/g, "&gt;");
            //text = text.replace('\n', '<br>', 'g');

      console.info(text);
      terminal.set_prompt('> ');
      terminal.position(1);
      if (terminal.paused())
         terminal.resume();
      while (text.indexOf("> ") == 0)
         text = text.substring(2);
      terminal.echo(text);
      if (text == "bye-bye :/") {
         terminal.set_prompt('$ ');
         ol = undefined;
      }
   },

   printErr: function(text) {
      Module.print("\x1b[31;0m" + text + "\x1b[0m");
   },

   canvas: (function() {
          var canvas = document.getElementById('canvas');

          // As a default initial behavior, pop up an alert when webgl context is lost. To make your
          // application robust, you may want to override this behavior before shipping!
          // See http://www.khronos.org/registry/webgl/specs/latest/1.0/#5.15.2
          // canvas.addEventListener("webglcontextlost", function(e) { alert('WebGL context lost. You will need to reload the page.'); e.preventDefault(); }, false);

          return canvas;
        })(),

   setStatus: function(text) {
          console.log("status: ", text)
          if (text == "Running...") {
            //terminal.echo("Connected.");
            //terminal.focus();
            //terminal.resume();
          }
        },

        totalDependencies: 0,
        monitorRunDependencies: function(left) {
          //this.totalDependencies = Math.max(this.totalDependencies, left);
          //Module.setStatus(left ? 'Preparing... (' + (this.totalDependencies-left) + '/' + this.totalDependencies + ')' : 'All downloads complete.');
        }
      };

      window.onerror = function(event) {
        // TODO: do not warn on ok events like simulating an infinite loop or exitStatus
        console.log(event);
        //Module.setStatus('Exception thrown, see JavaScript console');
        //spinnerElement.style.display = 'none';
        //Module.setStatus = function(text) {
        //  if (text) Module.printErr('[post-exception status] ' + text);
        //};
      };

$('#terminal').mousewheel(function(event) {
    terminal.scroll(event.deltaY);
    // console.log(event.deltaX, event.deltaY, event.deltaFactor);
    if (event.preventDefault)  //disable default wheel action of scrolling page
        event.preventDefault();
    else
        return false;
});
