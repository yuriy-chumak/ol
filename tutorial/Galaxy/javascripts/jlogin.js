function onSubmit() {
	document.location.href = "/login/" + $('#usermail').val() + "/" + $('#password').val();
//	$(window).get("/login/" + $('#usermail').val() + "/" + $('#password').val());
/*	$.get("/login/" + $('#usermail').val() + "/" + $('#password').val(), function (data) {
		document.open();
		document.write(data);
		document.close();
		$.cache = {};
	}, "Something wrong");*/

	return false;
}
