WEB_SOCKET_SWF_LOCATION = 'res/WebSocketMain.swf';
WEB_SOCKET_DEBUG = true;

function callback(data) {
  $('#chat-box').append(data);
  var objDiv = document.getElementById('chat-box');
  objDiv.scrollTop = objDiv.scrollHeight;
};

var ws;

function init() {
    ws = new WebSocket('ws://localhost:12345/chat');
    ws.onopen = function() {
        alert('connecting');
    };
    ws.onmessage = function(e) {
        callback(e.data);
    };
    ws.onclose = function () {
        alert('disconnected');
    };
};

function addMsg() {
  var action = $('#user-action').val();
  var dialogue = $('#user-dialogue').val();
  $('#user-action').val('');
  $('#user-dialogue').val('');
  ws.send("{ 'action' : '"+action+"', 'dialogue' : '"+dialogue+"'}");
};

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
