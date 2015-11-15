function doit(text)
{
   terminal.insert(text);
   terminal.focus();
}

var History;

function updateLink()
{
   if ($(this).context.attributes.href.value.startsWith("?"))
   $(this).click(function (event) {
      event.preventDefault();
      var link = $(this).context;
      var href = link.attributes.href.value; //was: nodeValue

      var state = History.getState();
      History.replaceState({
            href:  state.data.href,
            title: document.title,
            content: $("#content").html()
         },
         state.title, state.url);
      History.pushState({ href: href }, document.title, href);
   });
}

// update all links to dynamic load pages:
$('a').each(updateLink);

function show(page)
{
   page = page || "?en"; // default page
   $("#content").load(page.substr(1) + ".html", function() {
      $("html, body").animate({ scrollTop: 0 });
      $('#content a').each(updateLink);
      Rainbow.color();
   });
}

// history manipulations:
(function(window,undefined) {
   History = window.History;
   var historyChanged = function(event) {
      var state = History.getState(); // Note: We are using History.getState() instead of event.state

      if (state.data.title)
         document.title = state.data.title;
      if (state.data.content) {
         $("#content").html(state.data.content)
         $('#content a').each(updateLink);
      }
      else
         show(state.data.href);
   }
   History.Adapter.bind(window, 'statechange', historyChanged);

   show(window.location.search);
})(window);

