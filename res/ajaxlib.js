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

function callback(data) {
  $('#chat-box').append(data);
  var objDiv = document.getElementById('chat-box');
  objDiv.scrollTop = objDiv.scrollHeight;
};

function enable_input () {
    $('#user-input :input').attr('disabled', false);
}
function disable_input () {
    $('#user-input :input').attr('disabled', true);
}

function ping() {
    $.get('pingme',on_ajax_success);
    // ws.send('ping');
}

function currentChar () {
    return getURLParameter('char');
}

function startRecording() {
    alert('Recording scene.');
}

function stopRecording() {
    alert('Done recording.');
}

function addMsg() {
  var action = $('#user-action').val();
  var dialogue = $('#user-dialogue').val();
  if (action || dialogue) {
      $('#user-action').val('');
      $('#user-dialogue').val('');
      ws.send(JSON.stringify({action:action,dialogue:dialogue}));
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
        callback(e.data);
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
    // ping the server every 10 minutes to keep the session alive.
    interval_id = setInterval(ping,1000*60*10);
    $(document).ajaxError(on_ajax_error);
}

$(document).ready(init);
