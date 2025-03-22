tagList(
    box(
      id = "kundertab",
      status = "gray-dark",
      headerBorder = FALSE,
      # type = "tabs",
      solidHeader = FALSE, 
      closable = FALSE,
      collapsible = FALSE,
      width = 12,
      uiOutput("selectedClient"),
      ## Payments
      infoBoxOutput("clientDebt", width = 12),
      
      dataTableOutput("paymentsList")
      
      # # tabPanel(
      # #   title = "Kunder", 
      # #   dataTableOutput("clientsList")
      # # ),
      # tabPanel(
      #   title = "Kund rapport",
        # load_ui_content("ui/tab_clientreport.R")
      # )#,
      # tabPanel(
      #   title = "Utbetalning",
      #   load_ui_content("ui/tab_payments.R")
      # )
  )
)
