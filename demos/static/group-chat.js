google.load("jquery", "1");

google.setOnLoadCallback(
    function() {
        $(document).ready(function() { C.init(); });
    }
);

C = {
	username: ""
};

C.login = function() {
	if(!C.username) {
		C.username = C.cleanup(prompt("Before you say anything. What\'s your name?"));
		C.send("(joined)", function() {
				   $('#chat-input').focus();
			   });
	}
};

C.init = function() {
	$('#chat-input').bind('keypress', function(e) {
							  var code = (e.keyCode ? e.keyCode : e.which);
							  if (code == 13) { C.send_chat_input(); }
						  });
	$('#chat-input').focus(C.login);

	C.poll(); // start polling
};

C.cleanup = function(s) {
	// can't handle ampersands
	return s.replace('&', '+');
};

C.send_chat_input = function(body) {
	C.send($('#chat-input').val(),
		   function(data)  { $('#chat-input').val(''); });
};

C.send = function(body, callback) {
	$.ajax({
		url: "/send",
        type: "GET",
		data: { 'from': C.username,
				'body': C.cleanup(body) },
        dataType: "text",
		success: callback
	});

};

C.poll = function() {
	$.ajax({
		url: "/updates",
        type: "GET",
        dataType: "text",
		success: function(data) { C.catch_new_data(data); C.poll(); }
	});
};


C.catch_new_data = function(str) {
	var parts = str.split("\n");
	var name = $('<span>').html(parts[0]);
	var message = $('<span>').html(parts[1]);

	name.attr('class', 'name');
	message.attr('class', 'message');

	var div = $('<div>');
	div.attr('class', 'chat-line');
	div.css('display', 'none');
	div.append(name);
	div.append(message);
	$('#chat-log').prepend(div);
	div.slideDown();

};