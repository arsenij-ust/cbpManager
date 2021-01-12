# image
output$resourceDataImg <- renderImage({
  return(
    list(
      src = system.file("www", "resource_data.PNG", package = "cbpManager"),
      contentType = "image/png",
      alt = "timeline-example",
      width = "auto"
    )
  )
}, deleteFile = FALSE)

