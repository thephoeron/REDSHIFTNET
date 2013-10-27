$(document).ready(function() {

	//------------- Login page simple functions -------------//
 	$("html").addClass("loginPage");

 	wrapper = $(".login-wrapper");
 	barBtn = $("#bar .btn");

 	//change the tabs
 	barBtn.click(function() {
	  btnId = $(this).attr('id');
	  wrapper.attr("data-active", btnId);
	  $("#bar").attr("data-active", btnId);
	});

 	//show register tab
	$("#register").click(function() {
	  btnId = "reg";
	  wrapper.attr("data-active", btnId);
	  $("#bar").attr("data-active", btnId);
	});

	//check if user is change remove avatar
	var userField = $("input#user");
	var avatar = $("#avatar>img");

	//if user change email or username change avatar
	userField.change(function() {
		if($(this).val() === 'suggeelson@suggeelson.com') {
			avatar.attr('src', 'images/avatars/suggebig.jpg')
		} else {
			avatar.attr('src', 'images/avatars/no_avatar.jpg')
		}
	});

	//------------- Validation -------------//
	$("#login-form").validate({ 
		rules: {
			user: {
				required: true,
				minlength: 3
			}, 
			password: {
				required: true,
				minlength: 6
			}
		}, 
		messages: {
			user: {
				required: "Please provide a username",
				minlength: "Username must be at least 3 characters long"
			},
			password: {
				required: "Please provide a password",
				minlength: "Your password must be at least 5 characters long"
			}
		},
		submitHandler: function(form){
	        var btn = $('#loginBtn');
	        btn.removeClass('btn-primary');
	        btn.addClass('btn-danger');
	        btn.text('Checking ...');
	        btn.attr('disabled', 'disabled');
	        setTimeout(function() {
	        	btn.removeClass('btn-danger');
	        	btn.addClass('btn-success');
	        	btn.text('User find ...');
	        }, 1500);
	        setTimeout(function () {
	        	form.submit();
	        }, 2000);
		}
	});

});