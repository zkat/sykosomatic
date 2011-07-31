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
var career_field_group_class = 'career_field';

function mk_career_dropdown(idx) {
    var select = $(document.createElement('select'))
            .attr('name','careers['+idx+']')
            .attr('id','careers['+idx+']');
    var careers = {'':'Choose a career...',
                   'lumberjack':'Lumberjack',
                   'programmer':'Software Developer',
                   'messiah':'Savior'};
    var curr_career;
    for (var i in careers) {
        curr_career = $(document.createElement('option'))
            .text(careers[i])
            .val(i);
        $(select).append(curr_career);
    }
    return select;
}

function mk_career_dropdown_div(idx) {
    var label = $(document.createElement('label'))
            .attr('for','careers['+idx+']')
            .text('Career #'+(idx+1));
    var dropdown = mk_career_dropdown(idx);
    var years_spent = mk_years_spent(idx);
    var years_lbl = $(document.createElement('label'))
            .attr('for','career-times['+idx+']')
            .text('years');
    return $(document.createElement('div')).addClass('careers')
        .append(label,dropdown,years_spent,years_lbl);
}

function mk_years_spent(idx) {
    return $(document.createElement('input'))
        .addClass('career-times')
        .attr('name','career-times['+idx+']')
        .attr('id','career-times['+idx+']');
}

function add_career_choice() {
    var num_fields = count_fields('careers');
    if (num_fields < max_career_count) {
        var field = document.createElement('div');
        var dropdown = mk_career_dropdown_div(num_fields);
        $(field).addClass(career_field_group_class)
            .append(dropdown);
        $('#careers').append(field);
        //check_enabled_p();
    };
}
$('#add-career').click(add_career_choice);
$('#remove-career').click(function(){
    $('#careers .careers:last').remove();
});

// function text_input (name, label) {
//     return $(document.createElement('div'))
//         .addClass('field')
//         .append($(document.createElement('label')).attr('for',name).text(label))
//         .append($(document.createElement('input'))
//                 .attr('name',name)
//                 .attr('id',name)
//                 .attr('type','text'));
// };

// function numerous_fields (max_count,
//                           add_button_id,remove_button_id,
//                           field_class,field_div_id,
//                           field1_name,field1_label,
//                           field2_name,field2_label) {
//     function enable_button(button_id) {
//         $('#'+button_id).button('enable');
//     }
//     function disable_button(button_id) {
//         $('#'+button_id).button('disable');
//     }
//     function count_fields() {
//         return $('.'+field_class).length;
//     }
//     function check_enabled_p() {
//         var num_fields = count_fields();
//         if (num_fields >= max_count) {
//             disable_button(add_button_id);
//         }
//         else {
//             enable_button(add_button_id);
//         }
//         if (num_fields >= 1) {
//             enable_button(remove_button_id);
//         }
//         else {
//             disable_button(remove_button_id);
//         }
//     }
//     $('#'+add_button_id).click(function (){
//         var num_fields = count_fields();
//         if (num_fields < max_count) {
//             var field = document.createElement('div');
//             $(field).addClass(field_class)
//                 .append(text_input(field1_name+'['+num_fields+']',field1_label+' #'+(num_fields+1)))
//                 .append(text_input(field2_name+'['+num_fields+']',field2_label));
//             $('#'+field_div_id).append(field);
//             check_enabled_p();
//         };
//     });
//     $('#'+remove_button_id).click(function (){
//         $('#'+field_div_id+' .'+field_class+':last').remove();
//         check_enabled_p();
//     });
//     disable_button(remove_button_id);
// }
// numerous_fields(5,
//                 'add-career','remove-career',
//                 'career-field','careers',
//                 'careers','Career',
//                 'career-times','How long?'
//                );
// numerous_fields(3,
//                 'add-bodypart','remove-bodypart',
//                 'bodypart-field','bodyparts',
//                 'bodyparts','Feature',
//                 'bodypart-adjs','Adjective'
//                );
});