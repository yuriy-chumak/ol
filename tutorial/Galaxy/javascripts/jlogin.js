function goto(url)
{
	document.location.href = url.replace(new RegExp(" ", 'g'), "+");
}

function onLoginSubmit() {
	goto( $('#login').attr('action') + "/" + $('#usermail').val() + "/" + $('#password').val() );
//	$(window).get("/login/" + $('#usermail').val() + "/" + $('#password').val());
/*	$.get("/login/" + $('#usermail').val() + "/" + $('#password').val(), function (data) {
		document.open();
		document.write(data);
		document.close();
		$.cache = {};
	}, "Something wrong");*/

	return false;
}

function onRaceSubmit() {
	goto( $('#race').attr('action') + '/' + $('#name').val() );// + "/" + $('#primary_trait').val();

	return false;
}