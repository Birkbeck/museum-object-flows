text_box <- function(text) {
  tags$div(
    style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; margin-top: 20px;",
    p(text)
  )
}

popover_js <- HTML("
$(document).ready(function() {
  $('[data-toggle=\"popover\"]').popover({
    trigger: 'hover', // Show on hover
    container: 'body', // Attach to body to avoid layout issues
    html: true
  });
});")

form_item <- function(title, content, item) {
  tagList(
    tags$span(
      tags$strong(paste0(title, ":")),
      tags$i(
        class = "fa fa-info-circle",
        style = "color: #007bff; cursor: pointer;",
        `data-toggle` = "popover",
        `data-placement` = "right",
        title = title,
        `data-content` = content
      )
    ),
    tags$script(popover_js),
    item
  )
}
