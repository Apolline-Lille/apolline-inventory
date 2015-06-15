function addField(id,classe) {
    $("#" + id).append($($("." + classe + ":first")[0]).clone().append(""));
}

webshims.setOptions('forms-ext', {types: 'date'});
webshims.polyfill('forms forms-ext');

$(function () {
    $('[data-toggle-tooltip]').tooltip()
})