WEB_SOCKET_SWF_LOCATION = 'res/WebSocketMain.swf';
WEB_SOCKET_DEBUG = true;

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

function ajaxPing() {
    $.get('pingme');
}

var ws;

function addMsg() {
  var action = $('#user-action').val();
  var dialogue = $('#user-dialogue').val();
  if (action || dialogue) {
      $('#user-action').val('');
      $('#user-dialogue').val('');
      ws.send("{\"action\":\""+action+"\",\"dialogue\":\""+dialogue+"\"}");
  };
};

function init_chat() {
    disable_input();
    ws = new WebSocket('ws://dagon.ath.cx:8889/chat');
    // When running as root, use this.
    // ws = new WebSocket('ws://dagon.ath.cx:843/chat');
    ws.onopen = function() {
        ws.send(navigator.userAgent);
        alert('Websocket connected.');
        enable_input();
    };
    ws.onmessage = function(e) {
        callback(e.data);
    };
    ws.onclose = function () {
        disable_input();
        alert('Websocket disconnected. Refresh.');
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
    // ping the server every minute to keep the session alive.
    setInterval(ajaxPing,1000*60*1);
}

$(document).ready(init);
