$(document).keyup(function(event) {
    if ($("#search").is(":focus") && (event.keyCode == 13)) {
        $("#accept").click();
    }
    if ($("#note_text").is(":focus") && (event.keyCode == 13)) {
        $("#note").click();
    }
});