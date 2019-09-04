Shiny.addCustomMessageHandler('focus_search', function(message) {
        $('#search_text').focus();
      });
      
$(document).keyup(function(event) {
    if ($("#note_text").is(":focus") && (event.keyCode == 13)) {
        $("#note_search").click();
    }
}); 