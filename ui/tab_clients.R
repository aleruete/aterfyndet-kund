tagList(
    box(
      id = "kundertab",
      status = "primary",
      type = "tabs",
      solidHeader = FALSE, 
      closable = FALSE,
      collapsable = FALSE,
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