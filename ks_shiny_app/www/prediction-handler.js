Shiny.addCustomMessageHandler("predictionResult",
  function(message) {
    alert("The prediction is: " + message.toUpperCase());
  }
);
