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
      
      uiOutput("paymentsListUI")
  )
)
