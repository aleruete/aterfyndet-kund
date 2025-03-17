myDashboardBrand <- function(title, color = NULL, href = NULL, 
                             image = NULL, circle = FALSE, 
                             elevation = FALSE, 
                             opacity = 0.8) {
  if (!is.null(color)) 
    bs4Dash:::validateStatusPlus(color)
  shiny::tags$a(class = if (!is.null(color)) 
    paste0("brand-link bg-", color)
    else "brand-link", href = if (!is.null(href)) 
      href
    else "#", target = if (!is.null(href)) 
      "_blank", if (!is.null(image)) {
        shiny::tags$img(src = image, 
                        class = paste0("brand-image",
                                       ifelse(circle, " img-circle", ""), 
                                       ifelse(elevation," elevation-3", "")), 
                        style = paste0("opacity: ", opacity))
      }, shiny::tags$span(class = "brand-text font-weight-light", 
                          title))
}
