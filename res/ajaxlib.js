WEB_SOCKET_SWF_LOCATION = 'res/WebSocketMain.swf';
WEB_SOCKET_DEBUG = true;

var num_failures = 0;
var interval_id;
var ws;

function getURLParameter(name) {
    return unescape(
        (RegExp(name + '=' + '(.+?)(&|$)').exec(location.search)||[,null])[1]
    );
}

function on_ajax_error() {
  num_failures = num_failures + 1;
  if (num_failures >= 5) {
      clearTimeout(interval_id);
  };
};

function on_ajax_success() {
  num_failures = 0;
};

function callback(user_action) {
  var character = user_action['character'];
  var dialogue = user_action['dialogue'];
  var action = user_action['action'];

  var html = "<div class='user-entry'>";

  if (action.length > 0 && dialogue.length == 0) {
      html = html + "<p class='action'>"
            +"<span onclick='requestCharDescription(\""+character+"\")'>"+character+"</span>"
            +" "+action+"</p>";
  }
  else {
      html = html + "<p class='character' onclick='requestCharDescription(\""+character+"\")'>"+character+"</p>";
      if (action.length > 0) {
          html = html + "<p class='parenthetical'>"+"("+action+")</p>";
      };
      html = html + "<p class='dialogue'>"+dialogue+"</p>";
  };

 $('#chat-box').append($(html));
  // If the html is pre-rendered...
  // $('#chat-box').append(user_action);
  var objDiv = document.getElementById('chat-box');
  objDiv.scrollTop = objDiv.scrollHeight;
};

function enable_input () {
    $('#user-input-area :input').attr('disabled', false);
}
function disable_input () {
    $('#user-input-area :input').attr('disabled', true);
}

function requestCharDescription(charname) {
    return ws.send(JSON.stringify(['char-desc',charname]));
}

function ping() {
    $.get('pingme',on_ajax_success);
    ws.send(JSON.stringify(['ping']));
}

function currentChar () {
    return getURLParameter('char');
}

function startRecording() {
    alert('Recording scene.');
    ws.send(JSON.stringify(["start-recording"]));
}

function stopRecording() {
    alert('Done recording.');
    ws.send(JSON.stringify(["stop-recording"]));
}

function send_input() {
  var msg = $('#user-input').val();
  if (msg) {
      $('#user-input').val('');
      ws.send(JSON.stringify(['user-input',msg]));
  };
};

function init_chat() {
    disable_input();
    ws = new WebSocket('ws://dagon.ath.cx:8889/chat');
    // When running as root, use this.
    // ws = new WebSocket('ws://dagon.ath.cx:843/chat');
    ws.onopen = function() {
        ws.send(JSON.stringify({useragent: navigator.userAgent,
                                char: currentChar()}));
        enable_input();
    };
    ws.onmessage = function(e) {
        var message = JSON.parse(e.data);
        if (message[0] == 'user-action') {
            callback(message[1]);
        };
        if (message[0] == 'pong') {
            console.log('got a pong');
        };
        if (message[0] == 'char-desc') {
            alert('Description: ' + message[1]);
        };
    };
    ws.onclose = function () {
        disable_input();
    };
};

function init() {
    if (window.WebSocket) {
        init_chat();
    }
    // For some reason, doing this chained script loading seems to be preventing Flash from doing
    // its job.
    else {
        $.getScript('res/swfobject.js', function () {
                        $.getScript('res/web_socket.js', init_chat);
                    });
    };
    // ping the server to keep the session and websocket alive.
    interval_id = setInterval(ping,1000*60*5);
    $(document).ajaxError(on_ajax_error);
}

$(document).ready(init);
