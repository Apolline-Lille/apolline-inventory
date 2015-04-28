function submitForm(id){
    $("#"+id).submit();
}

function addField(id,classe) {
    console.log(classe,id)
    $("#" + id).append($($("." + classe + ":first")[0]).clone().append(""));
}

webshims.setOptions('forms-ext', {types: 'date'});
webshims.polyfill('forms forms-ext');