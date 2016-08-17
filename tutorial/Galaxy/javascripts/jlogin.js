function onSubmit() {
	$('body').load("/login/" + $('#usermail').val() + "/" + $('#password').val());
	return false;
}
