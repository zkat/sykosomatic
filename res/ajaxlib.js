function callback(data) {
  $('#chat-box').append(data);
  var objDiv = document.getElementById('chat-box');
  objDiv.scrollTop = objDiv.scrollHeight;
};

function addMsg() {
  var action = $('#user-action').val();
  var dialogue = $('#user-dialogue').val();
  $('#user-action').val('');
  $('#user-dialogue').val('');
  $.get('ajax', { 'f' : 'ADD-ACTION', 'action' : action, 'dialogue' : dialogue }, callback);
}

function updateChat() {
  $.get('ajax', { 'f' : 'UPDATE-CHAT' }, callback);
}

$(document).ready(updateChat);
setInterval(updateChat, 1000);
