Shiny.addCustomMessageHandler('updateCheckbox', function(message) {
    document.getElementById(message.id).checked = message.value;
});
