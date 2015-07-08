function addField(id,classe) {
    $("#" + id).append($($("." + classe + ":first")[0]).clone().append(""));
}

webshims.setOptions('forms-ext', {types: 'date'});
webshims.polyfill('forms forms-ext');

$(function () {
    $('[data-toggle-tooltip]').tooltip()
    $('[readonly] + .input-group-addon').on("click",function(e){
        var elem=$(e.currentTarget).parent('div').find('input');
        if(elem.attr("readonly")){
            elem.removeAttr("readonly")
        }else{
            elem.attr("readonly","readonly")
        }
    })
})