WEB_SOCKET_SWF_LOCATION = 'res/js/WebSocketMain.swf';
WEB_SOCKET_DEBUG = true;
if ('MozWebSocket' in window) {
    WebSocket = MozWebSocket;
}

var sykosomatic =
(function () {

    var ws;
    var pub = {};
    ///
    /// Utils
    ///

    function popup_dialog (title,text,modalp,buttons) {
        var d = elt('div');
        $(d).append(elt('p').text(text));
        $(d).dialog({title:title,modal:modalp,buttons:buttons});
    }

    function hide_loading_spinner() {
        $('#loading-spinner').hide('fast');
    }

    function enable_input () {
        $("#game-input > div :input, #dialogue-input > div :input").attr('disabled',false);
    }

    function disable_input () {
        $("#game-input > div :input, #dialogue-input > div :input").attr('disabled',true);
    }

    function get_url_parameter(name) {
        return unescape(
            (RegExp(name + '=' + '(.+?)(&|$)').exec(location.search)||[,null])[1]
        );
    }

    function elt(tag,classname) {
        var element = $(document.createElement(tag));
        if (classname) {
            $(element).addClass(classname);
        }
        return element;
    }

    function current_char () {
        return parseInt(get_url_parameter('char'));
    }

    function mk_dialogue(text) {
        var e = elt('p','dialogue');
        $(e).text(text);
        return e;
    }
    function mk_paren(text) {
        var e = elt('p','parenthetical');
        $(e).text("("+text+")");
        return e;
    }
    function mk_unit() {
        return elt('div','unit');
    }
    function mk_character(name) {
        var e = elt('p','character');
        $(e).click(function(){request_char_description(name);});
        $(e).text(name);
        return e;
    }
    function mk_action(actor, action) {
        var e = elt('p','action');
        if (actor) {
            var span = elt('span');
            $(span).text(actor);
            $(span).click(function(){request_char_description(actor);});
            $(e).append(span);
            $(e).append(document.createTextNode(' '));
        }
        $(e).append(document.createTextNode(action));
        return e;
    }
    function mk_append_action(actor, action) {
        var e = elt('span');
        $(e).append(document.createTextNode(" "));
        if (actor) {
            var span = elt('span');
            $(span).text(actor);
            $(span).click(function(){request_char_description(actor);});
            $(e).append(span);
            $(e).append(document.createTextNode(' '));
        }
        $(e).append(document.createTextNode(action));
        return e;
    }
    function mk_transition(text) {
        var e = elt('p','sceneheader');
        $(e).text(text);
        return e;
    }
    function mk_ooc(display_name,text) {
        var e = elt('p','ooc-line');
        $(e).text(display_name+": "+text);
        return e;
    }

    var last_actor;
    var last_unit;
    function add_action(actor,action) {
        action = action+'.';
        if ((last_actor == actor) && (last_unit == 'action')) {
            $('#scene-display > .unit:last > p.action').append(mk_append_action(actor,action));
        } else {
            var unit = mk_unit();
            unit.append(mk_action(actor,action));
            $('#scene-display').append(unit);
        }
        var obj_div = document.getElementById('scene-display');
        obj_div.scrollTop = obj_div.scrollHeight;
        last_actor = actor;
        last_unit = 'action';
    }
    pub.add_action = add_action;

    function add_dialogue(actor,dialogue,paren) {
        if ((last_actor == actor) && (last_unit == 'dialogue')) {
            if (paren && paren.length > 0) {
                $('#scene-display > .unit:last').append(mk_paren(paren));
            }
            $('#scene-display > .unit:last').append(mk_dialogue(dialogue));
            // else {
            //     $('.unit:last > p.dialogue:last').text(function (idx, text) {
            //                                                return text + ' ' + dialogue;
            //                                            });
            // }
        } else {
            var unit = mk_unit();
            unit.append(mk_character(actor));
            if (paren && paren.length > 0) {
                unit.append(mk_paren(paren));
            };
            unit.append(mk_dialogue(dialogue));
            $('#scene-display').append(unit);
        };
        last_actor = actor;
        last_unit = 'dialogue';
        var obj_div = document.getElementById('scene-display');
        obj_div.scrollTop = obj_div.scrollHeight;
    }
    pub.add_dialogue = add_dialogue;

    function add_transition(text) {
        var unit = mk_unit();
        unit.append(mk_transition(text));
        $('#scene-display').append(unit);
        last_actor = null;
        last_unit = 'transition';
        var obj_div = document.getElementById('scene-display');
        obj_div.scrollTop = obj_div.scrollHeight;
    }
    pub.add_transition = add_transition;

    function add_ooc(display_name,text) {
        var unit = mk_unit();
        unit.append(mk_ooc(display_name,text));
        $('#ooc-display').append(unit);
        var obj_div = document.getElementById('ooc-display');
        obj_div.scrollTop = obj_div.scrollHeight;
    }
    pub.add_ooc = add_ooc;
    ///
    /// Websockets
    ///

    function init_websockets() {
        disable_input();
        ws = new WebSocket('ws://zushakon.sykosomatic.org:8889/chat');
        // When running as root, use this.
        // ws = new WebSocket('ws://dagon.ath.cx:843/chat');
        ws.onopen = function() {
            var obj = { useragent : navigator.userAgent,
                        token : $('#wstoken').text(),
                        'char' : current_char() };
            ws.send(JSON.stringify(obj));
            enable_input();
            hide_loading_spinner();
        };
        ws.onmessage = function(e) {
            dispatch_message(JSON.parse(e.data));
        };
        ws.onclose = function () {
            disable_input();
        };
    }

    function ws_send (obj) {
        ws.send(JSON.stringify(obj));
    }

    ///
    /// From client
    ///
    function request_char_description(charname) {
        ws_send(['obj-desc',charname]);
    }
    pub.request_char_description = request_char_description;

    function start_recording() {
        ws_send(["start-recording"]);
    }
    pub.start_recording = start_recording;

    function stop_recording() {
        ws_send(["stop-recording"]);
    }
    pub.stop_recording = stop_recording;

    function send_emit() {
        var msg = $('#emit-input :input').val();
        if (msg) {
            $('#emit-input :input').val('');
            ws_send(['emit',msg]);
        }
        return false;
    }
    pub.send_emit = send_emit;

    function send_dialogue() {
        var msg = $('#dialogue-input :input').val();
        if (msg) {
            $('#dialogue-input :input').val('');
            ws_send(['dialogue',msg]);
        }
        return false;
    }

    function send_action() {
        var msg = $('#action-input :input').val();
        if (msg) {
            $('#action-input :input').val('');
            ws_send(['action',msg]);
        }
        return false;
    }

    function send_ooc_input() {
        var msg = $('#ooc-input :input').val();
        if (msg) {
            $('#ooc-input :input').val('');
            ws_send(['ooc',msg]);
        }
        return false;
    }
    pub.send_ooc_input = send_ooc_input;

    function ping() {
        ws_send(['ping']);
    }

    ///
    /// From server
    ///
    var dispatch_table = {
        // 'pong' : function(msg) { console.log('got a pong'); },
        'obj-desc' : function(msg) { popup_dialog('Description', msg[1], false,
                                                  {Ok:function(){$(this).dialog("close");}}); },
        'parse-error' : function(msg) { popup_dialog('Parser error', msg[1], true,
                                                     {Ok:function(){$(this).dialog("close");}}); }
    };

    function dispatch_message (message) {
        var msg_callback = dispatch_table[message[0]];
        if (msg_callback) {
            msg_callback(message);
        }
    };

    dispatch_table['action'] = function (msg) {
        add_action(msg[1]['actor'],msg[1]['action']);
    };

    dispatch_table['dialogue'] = function (msg) {
        add_dialogue(msg[1]['actor'],msg[1]['dialogue'],msg[1]['parenthetical']);
    };

    dispatch_table['transition'] = function (msg) {
        add_transition(msg[1]);
    };

    dispatch_table['ooc'] = function (msg) {
        add_ooc(msg[1]['display-name'],msg[1]['text']);
    };

    ///
    /// Action autocompletion
    ///
    var ac_response_handler = false;
    function init_autocomplete() {
        $("#action-input :input").autocomplete({
            source:function (request,response){
                ws_send(["complete-action",request.term]);
                ac_response_handler = response;
            }
        });
    }

    dispatch_table['completion'] = function(msg) {
        if (ac_response_handler) {
            ac_response_handler(msg[1]);
            ac_response_handler = false;
        }
    };

    ///
    /// Init
    ///
    var interval_id;
    function install_onsubmits() {
        $("#ooc-input").submit(send_ooc_input);
        $("#action-input").submit(send_action);
        $("#dialogue-input").submit(send_dialogue);
        $("#emit-input").submit(send_emit);
    }

    function init() {
        if (window.WebSocket) {
            init_websockets();
        }
        // For some reason, doing this chained script loading seems to be preventing Flash from doing
        // its job.
        else {
            $.getScript('res/swfobject.js', function () {
                $.getScript('res/web_socket.js', init_websockets);
            });
        };

        // UI stuff
        // Not sure why this doesn't play nice with enable_input()
        $(".btn").button();

        $("#game-input").tabs();
        $("#game-input .ui-tabs-nav, #game-input .ui-tabs-nav > *")
            .removeClass("ui-corner-all ui-corner-top")
            .addClass("ui-corner-bottom");
        install_onsubmits();

        init_autocomplete();

        var current_tab = 0;
        var inputs = ['dialogue-input','action-input','ooc-input','emit-input'];
        $(document).keypress(function(e) {
            if (e.keyCode == $.ui.keyCode.TAB) {
                if (e.shiftKey) {
                    current_tab--;
                    if (current_tab < 0) {
                        current_tab = inputs.length - 1;
                    }
                }
                else {
                    current_tab++;
                    if (current_tab >= inputs.length) {
                        current_tab = 0;
                    };
                };
                $("#game-input").tabs("select",current_tab);
                $("#game-input > div :input").get(current_tab).focus();
                if (e.preventDefault) {
                    e.preventDefault();
                }
                return false;
            }
            return e;
        });
        $("#dialogue-input").keypress(function(e) {
            if (e.keyCode == $.ui.keyCode.ENTER) {
                $("#dialogue-input").submit();
                e.preventDefault();
            };
        });
        $("#game-input > div :input").get(0).focus();

        $("select").chosen();

        // ping the server to keep the websocket alive.
        interval_id = setInterval(ping,1000*60*15);
        //$(document).unload(disable_input);
    }
    pub.init = init;

    return pub;
})();

$(document).ready(sykosomatic.init);
