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
var max_career_count = 5;

function add_career_choice() {
    var num_fields = count_fields('careers');
    if (num_fields < max_career_count) {
        $.get('/newchar/career',{'idx':num_fields},function(data){
            $('#careers').append($(data));
        });
        //check_enabled_p();
    };
}

function init_career_buttons() {
    $('#add-career').click(add_career_choice);
    $('#remove-career').click(function(){
        $('#careers .careers:last').remove();
    });
}
init_career_buttons();
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