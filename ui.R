# Define UI for application that draws a histogram
header <- dashboardHeader(
  title = myDashboardBrand(
    title = "Återfyndet", 
    color = "success",
    href = "https://aterfyndet.se/",
    image = "./img/Blanco Círculo Logo.png"
  ),
  titleWidth = widthSide,
  status = "success",
  leftUi = NULL, #dropdownMenuOutput("filters"),
  # rightUi = tagList(
  #   userOutput("user")
  # ),
  controlbarIcon = icon("gear"),
  fixed = TRUE)

sidebar <- dashboardSidebar(width = widthSide,
                            skin = "light",
                            minified = TRUE,
                            collapsed = TRUE,
                            sidebarMenu(id = "tabs",
                                        # menuItem("Lager",  tabName = "Stock",
                                        #          icon = icon("folder-open", class = "fas")),
                                        menuItem("Kunder", tabName = "Clients",
                                                 icon = icon("user", class = "fas")),
                                        menuItem("Boka Tid", tabName = "Booktime",
                                                 icon = icon("info", class = "fas"))#,
                            )
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
    # tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "shortcut icon", href = "./img/Negro Círculo Logo.png"),
    tags$style(".container-drag-source, .box-dad {font-size: 18px;}"),
    tags$style("html, body {overflow: visible !important;") ## this is to fix the problem with scrolling when equisse module is added
  ),
  
  # fluidRow(
  tabItems(
    ############# Inventory ###################
    # tabItem(tabName = "Stock", 
    #         load_ui_content("ui/tab_stock.R")
    # ),
    ############# Users ###################
    tabItem(tabName = "Clients", 
            load_ui_content("ui/tab_clients.R")
    ),
    ############# BookTime ###################
    tabItem(tabName = "Booktime",
            load_ui_content("ui/tab_book.R")
    )
  )# end tabItems
  # )
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

    
    