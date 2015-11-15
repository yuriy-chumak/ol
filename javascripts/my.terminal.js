var socket;

var terminal =
$('#terminal').terminal(function(command, terminal) {
   socket.writeFlush(command);
}, {
   prompt: '> ',
   name: 'repl',
   greetings: '\
OL - Owl Lisp -- yet another pet lisp\n\
Copyright (c) 2014 Aki Helin\n\
Copyright (c) 2014, 2015 Yuriy Chumak\n\
~~~~~~~~~~~~~~~~~\n',
   height: 200,
   enabled: false,

   onInit: function(terminal) {
      terminal.pause();
      terminal.echo("Loading...");

      JSocket.init("TCP.swf", function () {
         console.log("yes");
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
         if (data.length > 2 && data.indexOf('> ', data.length - 2) !== -1)
            terminal.echo(data.substring(0, data.length - 2));
         else
            terminal.echo(data);
      }
      function closeHandler() {
         terminal.pause();
         terminal.echo("Disconnected.");
         alert('lost connection')
      }
      function errorHandler(errorstr) {
         terminal.pause();
         terminal.echo("Error: " + errorstr);
         alert(errorstr);
      }
   }
});

