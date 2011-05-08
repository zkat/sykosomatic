WEB_SOCKET_SWF_LOCATION = 'res/WebSocketMain.swf';
WEB_SOCKET_DEBUG = true;

var sykosomatic;
if (!sykosomatic) {
    sykosomatic = {};
}

(function () {

     ///
     /// Utils
     ///
     function enable_input () {
         $('#user-input-area :input').attr('disabled', false);
     };

     function disable_input () {
         $('#user-input-area :input').attr('disabled', true);
     };

     function get_url_parameter(name) {
         return unescape(
             (RegExp(name + '=' + '(.+?)(&|$)').exec(location.search)||[,null])[1]
         );
     }

     function current_char () {
         return get_url_parameter('char');
     }

     ///
     /// Websockets
     ///
     var ws;
     function init_websockets() {
         disable_input();
         ws = new WebSocket('ws://dagon.ath.cx:8889/chat');
         // When running as root, use this.
         // ws = new WebSocket('ws://dagon.ath.cx:843/chat');
         ws.onopen = function() {
             ws.send(JSON.stringify({useragent: navigator.userAgent, char: current_char()}));
             enable_input();
         };
         ws.onmessage = function(e) {
             dispatch_message(JSON.parse(e.data));
         };
         ws.onclose = function () {
             disable_input();
         };
     };

     function ws_send (obj) {
       ws.send(JSON.stringify(obj));
     };

     ///
     /// From client
     ///
     function request_char_description(charname) {
         ws_send(['char-desc',charname]);
     }

     function start_recording() {
         ws_send(["start-recording"]);
     }

     function stop_recording() {
         ws_send(["stop-recording"]);
     }

     function send_input() {
         var msg = $('#user-input').val();
         if (msg) {
             $('#user-input').val('');
             ws_send(['user-input',msg]);
         };
     };

     function ping() {
         $.get('pingme',on_ajax_success);
         ws_send(['ping']);
     }

     ///
     /// From server
     ///
     var dispatch_table = {
         'pong' : function(msg) { console.log('got a pong'); },
         'char-desc' : function(msg) { alert('Description: ' + msg[1]); },
         'parse-error' : function(msg) { alert('Parser error: ' + msg[1]); }
     };

     function dispatch_message (message) {
         var msg_callback = dispatch_table[message[0]];
         if (msg_callback) {
           msg_callback(message);
         };
     };

     function mk_dialogue(text) {
         return $("<p class='dialogue'>"+text+"</p>");
     };
     function mk_paren(text) {
         return $("<p class='parenthetical'>("+text+")</p>");
     };

     function mk_unit() {
         return $("<div class='unit' />");
     }
     function mk_character(name) {
         return $("<p class='character'"
                  +"onclick='javascript:sykosomatic.request_char_description(\""+name+"\")'>"
                  +name
                  +"</p>");
     }
     function mk_action(actor, action) {
         var html = "<p class='action'>";
         html = html+"<span onclick='javascript:sykosomatic.request_char_description(\""+actor+"\")'>"+actor+"</span>";
         html = html+" "+action;
         html = html+"</p>";
         return $(html);
     }
     function mk_append_action(actor, action) {
         return $("<span> <span onclick='javascript:sykosomatic.request_char_description(\""+actor+"\")'>"+actor+"</span>"
                  +" "+action+"</span>");
     }
     var last_actor;
     var last_unit;
     dispatch_table['action'] = function (msg) {
         var actor = msg[1]['actor'];
         var action = msg[1]['action'];

         if ((last_actor == actor) && (last_unit == 'action')) {
             $('.unit:last > p.action').append(mk_append_action(actor,action));
         } else {
             var unit = mk_unit();
             unit.append(mk_action(actor,action));
             $('#chat-box').append(unit);
         }
         var obj_div = document.getElementById('chat-box');
         obj_div.scrollTop = obj_div.scrollHeight;
         last_actor = actor;
         last_unit = 'action';
     };

     dispatch_table['dialogue'] = function (msg) {
         var actor = msg[1]['actor'];
         var dialogue = msg[1]['dialogue'];
         var paren = msg[1]['parenthetical'];

         if ((last_actor == actor) && (last_unit == 'dialogue')) {
             if (paren.length > 0) {
                 $('.unit:last').append(mk_paren(paren));
                 $('.unit:last').append(mk_dialogue(dialogue));
             }
             else {
                 $('.unit:last > p.dialogue:last').text(function (idx, text) {
                                                            return text + ' ' + dialogue;
                                                        });
             }
         } else {
             var unit = mk_unit();
             unit.append(mk_character(actor));
             if (paren.length > 0) {
                 unit.append(mk_paren(paren));
             };
             unit.append(mk_dialogue(dialogue));
             $('#chat-box').append(unit);
         };
         last_actor = actor;
         last_unit = 'dialogue';
         var obj_div = document.getElementById('chat-box');
         obj_div.scrollTop = obj_div.scrollHeight;
     };

     ///
     /// Init
     ///
     var interval_id;
     var num_ajax_failures = 0;
     function on_ajax_error() {
         num_failures = num_ajax_failures + 1;
         if (num_failures >= 5) {
             clearTimeout(interval_id);
         };
     };

     function on_ajax_success() {
         num_ajax_failures = 0;
     };

     function init() {
         if (window.WebSocket) {
             init_websockets();
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

     sykosomatic.init = init;
     sykosomatic.send_input = send_input;
     sykosomatic.start_recording = start_recording;
     sykosomatic.stop_recording = stop_recording;
     sykosomatic.request_char_description = request_char_description;
 })();

$(document).ready(sykosomatic.init);
