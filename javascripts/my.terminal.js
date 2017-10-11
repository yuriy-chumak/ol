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

inputs.push(utf8.encode('(print "hello")'))


var terminal =
$('#terminal').terminal(function(command, terminal) {
   console.log("command: ", command)
   inputs.push(utf8.encode(command));
}, {
   prompt: '> ',
   name: 'repl',
   greetings: '\
OL - Otus Lisp - yet another pet lisp\n\
Copyright(c) 2014 - 2016 Yuriy Chumak\n\
\n\
Grew out of the Owl Lisp by Aki Helin\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n',
   height: 200,
   enabled: false,

   onInit: function(terminal) {
      terminal.pause();
      terminal.echo("Loading...");

/*      JSocket.init("TCP.swf", function () {
         console.log("creating TCP socket processor");
         socket = new JSocket({
            connectHandler: connectHandler,
            dataHandler:    dataHandler,
            closeHandler:   closeHandler,
            errorHandler:   errorHandler
         });
         socket.connect("iaaa.dlinkddns.com", 80);
         console.log("Trying to connect...");
      });
      function connectHandler() {
         terminal.echo("Connected.");
         terminal.focus();
         socket.write("EXEC /bin/ol HTTP/1.0"    + "\x0D\x0A");
         socket.write("Host: iaaa.dlinkddns.com" + "\x0D\x0A");
         socket.write("\x0D\x0A");
         socket.flush();
         terminal.resume();
      }
      function dataHandler(data) {
         if (data == "> ") // if starts from "> "
            ; // do nothing
         else if (data.length > 2 && data.indexOf('> ', data.length - 2) !== -1)
            terminal.echo(data.substring(0, data.length - 2));
         else
            terminal.echo(data);
      }

      function closeHandler() {
         terminal.pause();
         terminal.error("Disconnected.");
      }
      function errorHandler(errorstr) {
         terminal.pause();
         terminal.error("Error: " + errorstr);
      }*/
   }
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

      console.log(text);
      terminal.echo(text);
   },

   printErr: function(text) {
      if (arguments.length > 1) text = Array.prototype.slice.call(arguments).join(' ');
      /*if (0) { // XXX disabled for safety typeof dump == 'function') {
         dump(text + '\n'); // fast, straight to the real console
      } else {
         console.error(text);
      }*/
      console.log(text);
      terminal.echo(text);
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
            terminal.focus();
            terminal.resume();
          }


          /*if (!Module.setStatus.last) Module.setStatus.last = { time: Date.now(), text: '' };
          if (text === Module.setStatus.text) return;
          var m = text.match(/([^(]+)\((\d+(\.\d+)?)\/(\d+)\)/);
          var now = Date.now();
          if (m && now - Date.now() < 30) return; // if this is a progress update, skip it if too soon
          if (m) {
            text = m[1];
            progressElement.value = parseInt(m[2])*100;
            progressElement.max = parseInt(m[4])*100;
            progressElement.hidden = false;
            spinnerElement.hidden = false;
          } else {
            progressElement.value = null;
            progressElement.max = null;
            progressElement.hidden = true;
            if (!text) spinnerElement.style.display = 'none';
          }
          statusElement.innerHTML = text;*/
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
