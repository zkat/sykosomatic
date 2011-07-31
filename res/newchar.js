$(document).ready(function(){

function update_preview () {
    $.get('/newchar-preview',$('form').serialize(),function (data){
        $('#preview').html(data);
    });
}

//update_preview();
//$('form').change(update_preview);

function prettify_ui() {
    //$('#creation-forms').accordion();
    $('.button').button();
    $('.next-tab').click(function (){
        var tab_id = $(this).parent('div').attr('id');
        var tab_index = $('#tabs .ui-tabs-panel').index(document.getElementById(tab_id));
        $('#tabs').tabs('select',tab_index+1);
    });
}
prettify_ui();

function count_fields(field_class) {
    return $('.'+field_class).length;
}

/**
 * Career UI
 */
function enable_career() {
    var hidden_fields = $('.careers:hidden');
    var num_fields = hidden_fields.length;
    if (num_fields <= $('.careers').length) {
        $('.careers:hidden:first').show();
    }
}

function init_career_buttons() {
    $('#add-career').click(enable_career);
    $('.careers > button').click(function() {
        var div = $(this).parent('.careers');
        div.find(':input').each(function () {
            $(this).val('');
        });
        div.hide();
    });
    $('.careers').hide();
}
init_career_buttons();

/**
 * Feature UI
 */
var max_bodypart_count = 5;
function add_bodypart_choice() {
    var num_fields = count_fields('bodyparts');
    if (num_fields < max_bodypart_count) {
        $.get('/newchar/bodypart',{'idx':num_fields},function(data){
            $('#bodyparts').append($(data));
            $('#bodyparts-'+num_fields).change(function(){
                var div = $(this).parent('.field');
                $.get('/newchar/bodypart-adjs',{'idx':num_fields,'adj':$(this).val()},function(data){
                    $(div).children('.adj').html(data);
                });
            });
        });
    }
}

function init_bodypart_buttons() {
    $('#add-bodypart').click(add_bodypart_choice);
    $('#remove-bodypart').click(function(){
        $('#bodyparts .bodyparts:last').remove();
    });
}
init_bodypart_buttons();
});