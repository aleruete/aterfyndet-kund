# Define UI for application that draws a histogram
header <- dashboardHeader(
  # title = myDashboardBrand(
  #   title = "Återfyndet", 
  #   color = "success",
  #   href = "https://aterfyndet.se/",
  #   image = "./img/Blanco Círculo Logo.png"
  # ),
  # titleWidth = widthSide,
  compact = TRUE,
  status = "dark",
  disable = TRUE)#,
  # leftUi = NULL, #dropdownMenuOutput("filters"),
  # rightUi = tagList(
  #   userOutput("user")
  # ),
  # controlbarIcon = icon("gear"),
  # fixed = FALSE)

sidebar <- dashboardSidebar(skin = "light",
                            # width = widthSide,
                            disable = TRUE#,
                            # minified = TRUE,
                            # collapsed = TRUE,
                            # sidebarMenu(id = "tabs",
                            #             menuItem("Kunder", tabName = "Clients",
                            #                      icon = icon("user", class = "fas"))
                            #            
                            # )
)

body <- dashboardBody(
  useShinyjs(),
  tags$head(
    tags$script('$(document).on("keyup", function(e) {
                  if(e.keyCode == 13){
                    Shiny.onInputChange("keyPressed", Math.random());
                  }
                  });'
    ),
    # to focus on element
    tags$script('
      Shiny.addCustomMessageHandler("refocus",
            function(e_id) {
            document.getElementById(e_id).focus();
                                  });'),
    # Include our custom CSS
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "shortcut icon", href = "./img/Negro Círculo Logo.png")
  ),
  load_ui_content("ui/tab_clients.R")
)


dashboardPage(title = "Återfyndet", 
              header = header,
              sidebar = sidebar,
              body = body,
              controlbar = NULL,
              footer = NULL, #footer,
              # scrollToTop = TRUE,
              freshTheme = bs4dashTheme,
              # preloader = list(html = tagList(spin_1(), "Loading ..."), 
              #                  color = "#F48635"),
              options = NULL,
              fullscreen = FALSE,
              help = NULL,
              dark = NULL,
              scrollToTop = FALSE) 
# end dashboard page

    
    