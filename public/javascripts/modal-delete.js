$('#modal-delete').on('show.bs.modal', function (event) {
    var button = $(event.relatedTarget)
    var title = button.data('title')
    var link = button.data('link')
    var modal = $(this)
    modal.find('#title-delete').text(title)
    modal.find("#accept-delete").attr("href",link)
})