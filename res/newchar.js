$(document).ready(function(){

//$('#creation-forms').accordion();
$('.button').button();
$('.next-tab').click(function (){
    var tab_id = $(this).parent('div').attr('id');
    var tab_index = $('#tabs .ui-tabs-panel').index(document.getElementById(tab_id));
    console.log(tab_index);
    $('#tabs').tabs('select',tab_index+1);
});

function update_preview () {
    $.get('/newchar-preview',$('form').serialize(),function (data){
        $('#preview').html(data);
    });
}
//update_preview();
//$('form').change(update_preview);
function text_input (name, label) {
    return $(document.createElement('div'))
        .addClass('field')
        .append($(document.createElement('label')).attr('for',name).text(label))
        .append($(document.createElement('input'))
                .attr('name',name)
                .attr('id',name)
                .attr('type','text'));
};

function numerous_fields (max_count,
                          add_button_id,remove_button_id,
                          field_class,field_div_id,
                          field1_name,field1_label,
                          field2_name,field2_label) {
    function enable_button(button_id) {
        $('#'+button_id).button('enable');
    }
    function disable_button(button_id) {
        $('#'+button_id).button('disable');
    }
    function count_fields() {
        return $('.'+field_class).length;
    }
    function check_enabled_p() {
        var num_fields = count_fields();
        if (num_fields >= max_count) {
            disable_button(add_button_id);
        }
        else {
            enable_button(add_button_id);
        }
        if (num_fields >= 1) {
            enable_button(remove_button_id);
        }
        else {
            disable_button(remove_button_id);
        }
    }
    $('#'+add_button_id).click(function (){
        var num_fields = count_fields();
        if (num_fields < max_count) {
            var field = document.createElement('div');
            $(field).addClass(field_class)
                .append(text_input(field1_name+'['+num_fields+']',field1_label+' #'+(num_fields+1)))
                .append(text_input(field2_name+'['+num_fields+']',field2_label));
            $('#'+field_div_id).append(field);
            check_enabled_p();
        };
    });
    $('#'+remove_button_id).click(function (){
        $('#'+field_div_id+' .'+field_class+':last').remove();
        check_enabled_p();
    });
    disable_button(remove_button_id);
}
numerous_fields(5,
                'add-career','remove-career',
                'career-field','careers',
                'careers','Career',
                'career-times','How long?'
               );
numerous_fields(3,
                'add-bodypart','remove-bodypart',
                'bodypart-field','bodyparts',
                'bodyparts','Feature',
                'bodypart-adjs','Adjective'
               );
});