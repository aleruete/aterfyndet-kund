tagList(
  tabBox(
    id = "lagertab",
    status = "primary",
    type = "tabs",
    solidHeader = FALSE, 
    closable = FALSE,
    collapsible = FALSE, 
    width = 12,
    tabPanel(
      title = "Lager",
      value = "stock",
      fluidRow(
        column(3,
               materialSwitch(
                 inputId = "showSold",
                 label = "Vissa utsålda artiklar", 
                 value = FALSE,
                 status = "primary")
               ),
        column(3,
               materialSwitch(
                 inputId = "showInvalid",
                 label = "Vissa ogiltiga artiklar", 
                 value = FALSE,
                 status = "primary")
        ),
        column(4, offset = 2, 
               actionButton("markItemSold", "Markera som såld", 
                            icon = icon("file-invoice-dollar"),
                            status = "info"),
        #        ),
        # column(3, #offset = 3,
               actionButton("editItem", "Redigera", 
                            icon = icon("pencil"),
                            status = "danger")
        )
      ),
      fluidRow(
        dataTableOutput("stockList")
      )
    ),
    tabPanel(
      title = "Nya artiklar",
      value = "newitems",
      load_ui_content("ui/tab_newitems.R")
    ),
    tabPanel(
      title = "Sälj",
      value = "sale",
      load_ui_content("ui/tab_sale.R")
    ),
    tabPanel(
      title = "Inlämning",
      value = "submissions",
      load_ui_content("ui/tab_submission.R")
    )
  )
)