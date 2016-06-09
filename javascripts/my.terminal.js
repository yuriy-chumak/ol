var socket;

var terminal =
$('#terminal').terminal(function(command, terminal) {
   socket.writeFlush(command);
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

      /*var tcp;
      try {
         tcp = navigator.TCPSocket.open('127.0.0.1', 9000, {
            useSecureTransport: false
         });
      }
      catch(err) {
         terminal.error(err);
      }*/

      JSocket.init("TCP.swf", function () {
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
      }
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
