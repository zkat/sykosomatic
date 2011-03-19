WEB_SOCKET_SWF_LOCATION = 'res/WebSocketMain.swf';
WEB_SOCKET_DEBUG = true;

function callback(data) {
  $('#chat-box').append(data);
  var objDiv = document.getElementById('chat-box');
  objDiv.scrollTop = objDiv.scrollHeight;
};

var ws;

function enable_input () {
    $('#user-input :input').attr('disabled', false);
}
function disable_input () {
    $('#user-input :input').attr('disabled', true);
}

function init() {
    disable_input();
    ws = new WebSocket('ws://dagon.ath.cx:8889/chat');
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

function addMsg() {
  var action = $('#user-action').val();
  var dialogue = $('#user-dialogue').val();
  $('#user-action').val('');
  $('#user-dialogue').val('');
  ws.send("{\"action\":\""+action+"\",\"dialogue\":\""+dialogue+"\"}");
};

$(document).ready(init);
// function callback(data) {
//   $('#chat-box').append(data);
//   var objDiv = document.getElementById('chat-box');
//   objDiv.scrollTop = objDiv.scrollHeight;
// };

// function addMsg() {
//   var action = $('#user-action').val();
//   var dialogue = $('#user-dialogue').val();
//   $('#user-action').val('');
//   $('#user-dialogue').val('');
//   $.get('ajax', { 'f' : 'ADD-ACTION', 'action' : action, 'dialogue' : dialogue }, callback);
// }

// function updateChat() {
//   $.get('ajax', { 'f' : 'UPDATE-CHAT' }, callback);
// }

// $(document).ready(updateChat);
// setInterval(updateChat, 1000);
