function(input, output, session) {

# Reactive values ####
  con <- reactiveVal()
  # url <- reactiveValues(query = NULL, hash = NULL)
  authorised <- reactiveValues(val = FALSE, isadmin = NULL, auth = NULL)
  users <- reactiveValues(data = NULL, username = NULL, hash = NULL)
  synk <- reactiveValues(status = FALSE, timestamp = NULL)
  stock <- reactiveValues(data = NULL, selected = NULL, print = NULL)
  clients <- reactiveValues(data = NULL, selected = NULL)
  category <- reactiveValues(data = NULL)
  payments <- reactiveValues(data = NULL, debt = NULL)
  
# Load database ####
  withProgress({
    if (production){
      # from dropbox
      message("Running from cloud")
      dropfiles <- drop_dir(path = dropboxpath) |> 
        filter(.tag == "file", grepl(".db", name))
      nfiles <- nrow(dropfiles)
      stopifnot("No .db in folder" = nfiles > 0)

      for (j in seq(nfiles)) {
        # dest_file <- paste0(tempdir(), "/", dropfiles$name[j])
        dest_file <- paste0("data/", dropfiles$name[j])
        drop_download(path = dropfiles$path_display[j],
                      local_path = dest_file,
                      overwrite = TRUE) #dtoken = token
        # load(dest_file, verbose = TRUE)
        
        synk$status <- TRUE
        synk$timestamp <- timestamp()
        incProgress(amount = 0.5 * 1/nfiles)
      }
    } else {
      ## locally
      message("Running locally")
      synk$status <- TRUE
    }
    setProgress(1)
  }, message = "Läser in bakgrund data")
  
  # Load data from database ####
  observe({
    withProgress({
      
      if (synk$status) {
        consql <- dbConnect(RSQLite::SQLite(), 
                            dbname = ifelse(production, 
                                            'data/aterfyndet.db',
                                            'data/aterfyndet-test.db'))
        consqlGlobal <<- consql
        con(consql)
        message("Connection established")
      }

      if (!is.null(con())) {
        users$data <- dbReadTable(conn = con(), "users")
        message("You are in Neo")
        setProgress(1)
      } else { message("Couldn't connect"); stop() }
    }, message = "Läser in bakgrund data")
  })
  
  # Login  #####  
  observe({
    req(!authorised$val)
    loginDialog()
  })
  observeEvent(input$keyPressed, {
    shinyjs::click("login")
  }) 
  
  ## hit login ####
  observeEvent(input$login, {
    if (is.na(input$username) | input$username == "")  return()
    if (is.na(input$pass) | input$pass == "")  return()
    
    users$username <- input$username
    users$hash <- encrypt_string(input$pass, key = keysecret)
    updateTextInput(session, "pass", label = NULL, value = "")
    
    removeModal()

    tryCatch({
      logingin <- users$data |> 
        filter(användare == users$username)

      if (identical(logingin$hash, users$hash)) {
        authorised$val <- TRUE
      }else{
        authorised$val <- FALSE
      }
    }, error = function(w) w, warning = function(w) w,
    finally = {
      if (is.null(authorised$val)) {
        shinyalert(title = "Inloggningen misslyckades",
                   text = "Kontrollera din nätverk",
                   closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = FALSE,
                   type = "error",
                   showConfirmButton = TRUE, showCancelButton = FALSE,
                   confirmButtonText = "OK", #confirmButtonCol = "#6AA039",
                   timer = 0, animation = TRUE
        )
      }
      if (!authorised$val) {
        shinyalert(title = "Inloggningen misslyckades",
                   text = HTML("Kontrollera dina uppgifter och försök igen."),
                   closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = TRUE,
                   type = "error",
                   showConfirmButton = TRUE, showCancelButton = FALSE,
                   confirmButtonText = "OK", #confirmButtonCol = "#6AA039",
                   timer = 0, animation = TRUE
            )
        loginDialog()
      } #if authorised$val = FALSE

      if (authorised$val) {
        authorised$isadmin <- as.logical(logingin$is_admin)

        shinyalert(title = "Välkommen",
                   text = "Du har loggat in",
                   closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = FALSE,
                   type = "success",
                   showConfirmButton = TRUE, showCancelButton = FALSE,
                   confirmButtonText = "OK", #confirmButtonCol = "#6AA039",
                   timer = 2500, animation = FALSE
        )
      }
    })
  })#end Login
  
  ## If authenticated #####
  observeEvent(authorised$val,{
    req(authorised$val)
print(authorised$isadmin)

      # Load rest of data from database ####
      withProgress({
          setProgress(.1)
          clients$data <- dbReadTable(conn = con(), "clients")
          id_kund <- users$data |> 
            filter(användare == users$username) |> 
            pull(id_kund)
          
          clients$selected <- clients$data |> 
            filter(id == id_kund)
          # selectInput("clientFilter", "Kund id:", 
          #             choices = NULL),
# print(clients$selected)
          setProgress(.3)
          stock$data <- dbGetQuery(conn = con(), glue("SELECT * FROM stock WHERE id_kund = {clients$selected$id}")) |> 
            select(-id) |>
            mutate(id_kund = as.integer(id_kund), 
                   id_kategori = as.integer(id_kategori), 
                   pris = as.numeric(pris), 
                   artikel = as.integer(artikel),
                   sald = as.integer(sald), 
                   sald_pris = as.numeric(sald_pris))

          setProgress(.5)
          category$data <- dbGetQuery(conn = con(), "SELECT * FROM categories") |> 
            arrange(id)
          setProgress(.8)
          payments$data <- dbGetQuery(conn = con(), glue("SELECT * FROM payments WHERE id_kund = {clients$selected$id}"))
          
          message("All loaded")
          setProgress(1)
      }, message = "Läser in bakgrund data")

    # if (authorised$isadmin) { #authorised$val & 
    # } else { ## if not admin
    #   shinyalert("Aja baja!", "Du har inte behörighet", type = "error" , timer = 2000)
    # }
  })
  
  observe({
    if (!is.null(stock$data)) {
      submis <- max(stock$data$id_inlamn, 0, na.rm = TRUE )
      updateNumericInput(session, "submissionid", value = submis)
    }
  })
  
# Lager ####
## Lager list ####
  output$stockList <- renderDataTable({

    if (is.null(stock$data)) return(NULL)
    stocktab <- stock$data |> 
      filter(if (input$showInvalid) giltig %in% c(0,1) else giltig == 1,
             if (input$showSold) sald %in% c(0,1) else sald == 0) |>
      left_join(category$data, by = c("id_kategori" = "id")) |> 
      select(id_kund, kategori, beskrivning, pris, artikel, kod, 
             sald, sald_datum, sald_pris, giltig) |> 
      mutate(#id_kund = as.factor(id_kund), 
             kategori = as.factor(kategori), 
             artikel = as.factor(artikel),
             sald = as.logical(sald), 
             giltig = as.logical(giltig))
    
    datatable(stocktab,
              # editable = list(target = 'cell', 
              #                 disable = list(columns = c(0:1,4:9))),
              class = 'row-border stripe compact hover',
              rownames = FALSE, selection = "single",
              extensions = 'Buttons',
              # autoHideNavigation = TRUE,
              colnames = c("Kund id", "Kategori", "Beskrivning", "Pris", "Artikel", 
                           "Kod", "Såld", "Såld datum", "Såld pris", "Giltig"),
              # filter = "top",
              options = list(
                dom = 'lftBp',
                # dom = "<'top'<'left-col'B><'center-col'r><'right-col'f>>ti<'clear'>", #'Btfi',
                columnDefs = list(
                  list(targets = c(9), visible = FALSE)
                  ),
                buttons = list(
                  list(extend = 'copy',
                       text =  '<i class="fa fa-files-o"></i>',
                       titleAttr = 'Kopiera'),
                  list(extend = 'excel',
                       text = '<i class="fa fa-file-excel-o"></i>',
                       titleAttr = 'Excel'),
                  list(extend = 'print',
                       text = '<i class="fa fa-file-pdf-o"></i>',
                       titleAttr = 'PDF')
                ),
                stateSave = TRUE,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Swedish.json'),
                searching = TRUE, 
                autoFill = FALSE, 
                ordering = TRUE, 
                lengthMenu = list(c(10, 25, 50, 100, -1), 
                                  c('10', '25', '50', '100','Alla')),
                pageLength = 25,
                lengthChange = TRUE, scrollX = TRUE, scrollY = FALSE, 
                paging = TRUE)
              ) |> 
      formatCurrency(c(4,9), currency = "kr", before = FALSE) |> 
      formatStyle(columns = 1, valueColumns = 7, target = "row",
                     backgroundColor = styleEqual(c(TRUE), c("grey"))) |> 
      formatStyle(columns = 1, valueColumns = 10, target = "row",
                  backgroundColor = styleEqual(c(FALSE), c("#db8383")))
      
  })
  
  proxyStockList <- dataTableProxy("stockList")
  
  # Clients ####
  # output$clientsList <- renderDataTable({
  #   if (is.null(clients$data)) return(NULL)
  #   clientsTab <- clients$data |>
  #     select(id, namn, pnr, sista4, adress, postnummer, ort, telefon, epost, giltig) |> 
  #     mutate(id = as.integer(id),
  #            pnr = paste0(sprintf("%06d", coalesce(pnr, 0)), "-", 
  #                         sprintf("%04d", coalesce(sista4, 0))),
  #            giltig = as.logical(giltig)) |> 
  #     select(-sista4)
  #   
  #   datatable(clientsTab, 
  #             class = 'row-border stripe compact hover',
  #             rownames = FALSE, selection = 'none', 
  #             extensions = 'Buttons', 
  #             # autoHideNavigation = FALSE,
  #             # editable = list(target = 'cell', 
  #             #                 disable = list(columns = c(0,2))),
  #             colnames = c("Kund id", "Namn", "PNR","Adress", "Postnummer", "Ort",
  #                          "Telefon", "e-post", "Giltig"),
  #             options = list(
  #               dom = 'lftBp',
  #               buttons = list(
  #                 list(extend = 'copy',
  #                      text =  '<i class="fa fa-files-o"></i>',
  #                      titleAttr = 'Kopiera'),
  #                 list(extend = 'excel',
  #                      text = '<i class="fa fa-file-excel-o"></i>',
  #                      titleAttr = 'Excel'),
  #                 list(extend = 'print',
  #                      text = '<i class="fa fa-file-pdf-o"></i>',
  #                      titleAttr = 'PDF')
  #               ),
  #               stateSave = TRUE,
  #               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Swedish.json'),
  #               searching = TRUE, autoFill = FALSE, ordering = TRUE, 
  #               lengthMenu = list(c(10, 25, 50, 100, -1), 
  #                                 c('10', '25', '50', '100','Alla')),
  #               pageLength = 25,
  #               lengthChange = TRUE, scrollX = TRUE, scrollY = FALSE, 
  #               paging = TRUE)
  #   )
  # })
  # 
  # proxyClientList <- dataTableProxy("clientsList")
  # 
  # observeEvent(input$clientsList_cell_edit, {
  #   info <- input$clientsList_cell_edit
  # 
  #   ## TODO this IF should not be necessary
  #   if (!info$col %in% c(0,2)) {
  #     i = info$row
  #     j = info$col + 1
  #     
  #     ## TODO should I treat empty strings as NULL?
  #     clientsTab <- clients$data |> 
  #       select(id, namn, pnr, adress, postnummer, ort, telefon, epost, giltig)
  #     column <- names(clientsTab)[j]
  # 
  #     if (column == "giltig") {
  #       if (!info$value %in% c("true", "false", "TRUE", "FALSE")) {
  #         shinyalert("Aja baja!", "Giltig kan bara vara 'true' eller 'false'", 
  #                    type = "error" , timer = 2000)
  #         info$value <- NULL
  #       } else {
  #         info$value <- as.integer(as.logical(info$value))
  #       }
  #     }
  #     
  #     if (column == "epost") {
  #       if(!isValidEmail(info$value)) {
  #         shinyalert("Aja baja!", "E-post är inte giltig", 
  #                    type = "error" , timer = 2000)
  #         info$value <- NULL
  #       }
  #     }
  # 
  #     info$value <- coalesce(info$value, clientsTab[i, j])
  #     clientsTab[i,j] <- info$value
  # 
  #     # update reactive value
  #     # wItem <- which(clients$data$id %in% clientsTab$id[i])
  #     # clients$data[wItem, j] <- info$value
  # 
  #     # write to database
  #     dbExecute(con(), glue("UPDATE clients
  #                           SET {column} = '{info$value}'
  #                           WHERE id = {clientsTab$id[i]};"))
  # 
  #     ## synk to DropB if in Production
  #     if (production) {
  #       drop_synk_up()
  #     }
  #   }
  #   
  # })     
  
  
  
  ##client report ####
  output$selectedClient <- renderUI({
    req(clients$selected)
    nitems <- stock$data |> 
      filter(id_kund == clients$selected$id, 
             giltig == 1) |> 
      reframe(items = n(),
              sold = sum(sald))
    
    tagList(
      fluidRow(
        column(width = 12,
               h3(glue("{clients$selected$namn} (ID: {clients$selected$id})")),
               h4(glue("Antal inlämnat artiklar: {nitems$items}")),
               h5(glue("Antal sålda artiklar: {nitems$sold}"))
        )
      )#,
      # fluidRow(
      #   column(width = 6, offset = 6,
      #          dateRangeInput("reportMonth", "Period:",
      #                         start = floor_date(Sys.Date(), unit = "month"),
      #                         end = ceiling_date(Sys.Date(), unit = "month") - 1,
      #                         min = "2020-01-01",
      #                         max = ceiling_date(Sys.Date(), unit = "month") - 1,
      #                         format = "yyyy-mm-dd", language = "sv",
      #                         separator = " - ")
      #   )
      # )
    )
  })
  
  # output$clientReportAll <- renderUI({
  #   req(input$clientFilter)
  #   if (input$clientFilter == "all") {
  #     total <- stock$data |> 
  #       filter(sald == 1, 
  #              giltig == 1) |> 
  #       reframe(items = n(),
  #               inkomst = sum(sald_pris),
  #               vinst = sum(sald_pris) * 0.8 * 0.6)
  #     tagList(
  #       # h3(glue("{clients$selected$namn} (ID: {clients$selected$id})")),
  #       h4(glue("Antal sålda artiklar: {total$items}"))
  #     )
  #   }
  # })
  
  # output$clientReportAllPlot <- renderPlot({
  #   req(input$clientFilter)
  #   if (input$clientFilter == "all") {
  #     total <- stock$data |> 
  #       filter(sald == 1,
  #              giltig == 1) |> 
  #       group_by(sald_datum) |> 
  #       reframe(inkomst = sum(sald_pris),
  #               vinst = sum(sald_pris) * 0.8 * 0.6)
  #     
  #     par(mar = c(3,6,1,1), las = 1)
  #     barplot(total$inkomst, names.arg = as.Date(total$sald_datum),  
  #             xlab = "datum", ylab = "", col = "#db7aa1",
  #             border = NA)  
  #   }
  # })
  
  # output$clientReportTotal <- renderUI({
  # output$clientReportTotal <- renderInfoBox({
  #   req(clients$selected)
  #   if (is.null(clients$selected)) return(NULL)
  #   total <- stock$data |> 
  #     filter(id_kund == clients$selected$id,
  #            sald == 1,
  #            giltig == 1) |> 
  #     summarise(items = n(), 
  #               sold = sum(sald_pris),
  #               commission = sum(sald_pris) * 0.8 * 0.4,
  #               income = sum(sald_pris) * 0.8 * 0.6)
  # 
  #   infoBox(
  #     title = glue("Total sålt: {total$sold} kr"),
  #     value = glue("Total provision: {total$commission} kr"),
  #     subtitle = glue("Total intäkt: {total$income} kr"),
  #     color = "indigo",
  #     icon = icon("dollar")
  #   )
  #   
  # })
  
  # output$clientReportMonth <- renderInfoBox({ #renderUI({
  #   req(clients$selected)
  #   if (is.null(clients$selected)) return(NULL)
  #   total <- stock$data |> 
  #     filter(id_kund == clients$selected$id,
  #            sald == 1,
  #            giltig == 1) |> 
  #     mutate(sald_datum = as.Date(sald_datum)) |>
  #     filter(sald_datum >= input$reportMonth[1] & 
  #            sald_datum <= input$reportMonth[2]) |> 
  #     summarise(items = n(), 
  #               sold = sum(sald_pris),
  #               commission = sum(sald_pris) * 0.8 * 0.4,
  #               income = sum(sald_pris) * 0.8 * 0.6)
  #   
  #   infoBox(
  #     title = glue("Sålt i period: {total$sold} kr"),
  #     value = glue("Provision i period: {total$commission} kr"),
  #     # subtitle = glue("Intäkt: {total$income} kr"),
  #     color = "fuchsia",
  #     icon = icon("dollar")
  #   )
  # })
  
  ## UI for client debt
  # output$clientPayments <- renderUI({
  #   req(clients$selected)
  #   if (is.null(clients$selected)) return(NULL)
  #   fluidRow(
  #     column(width = 6,
  #            infoBoxOutput("clientDebt", width = 12)
  #     ),
  #     column(width = 6, 
  #            actionButton("makePayment", "Utbetalning", 
  #                         icon = icon("hand-holding-dollar"),
  #                         status = "info", style = "margin-top: 20px; margin-left: 10px;")#,
  #            # actionButton("reportPayment", "Ladda ner rapport", 
  #            #              icon = icon("hand-holding-dollar"),
  #            #              status = "info", style = "margin-top: 20px; margin-left: 10px;")
  #     )
  #   )
  # })
  
  output$clientDebt <- renderInfoBox({
    req(clients$selected)
    if (is.null(clients$selected)) {
      payment$debt <- NULL
      return(NULL)
    }
    totalSold <- stock$data |> 
      filter(id_kund == clients$selected$id,
             sald == 1,
             giltig == 1) |> 
      summarise(commission = sum(sald_pris) * 0.8 * 0.4)
    
    totalPaid <- payments$data |> 
      filter(id_kund == clients$selected$id,
             utbetald == 1) |> 
      summarise(paied = sum(belopp))
    
    payments$debt <- round(totalSold$commission - totalPaid$paied, 2)
    
    infoBox(
      title = "Att betala",
      value = glue("{payments$debt} kr"),
      color = "olive",
      icon = icon("hand-holding-dollar")
    )
    
  })
  
  
  
  
  
  ## payments ####
  output$paymentsList <- renderDataTable({
    
    if (is.null(payments$data)) return(NULL)
    paymentstab <- payments$data |> 
      select(-id) |> 
      mutate(#id_kund = as.factor(id_kund), 
        belopp = as.numeric(belopp), 
        datum = ymd(datum),
        utbetald = as.logical(utbetald))
    
    datatable(paymentstab,
              # editable = list(target = 'cell', 
              #                 disable = list(columns = c(0:1,4:9))),
              class = 'row-border stripe compact hover',
              rownames = FALSE, selection = "single",
              extensions = 'Buttons', 
              # autoHideNavigation = TRUE,
              colnames = c("Kund id", "Belopp", "Datum", "Betalningssätt", "Utbetald"),
              # filter = "top",
              options = list(
                dom = 'lftBp', 
                # columnDefs = list(
                #   list(targets = c(9), visible = FALSE)
                # ),
                buttons = list(
                  list(extend = 'copy',
                       text =  '<i class="fa fa-files-o"></i>',
                       titleAttr = 'Kopiera'),
                  list(extend = 'excel',
                       text = '<i class="fa fa-file-excel-o"></i>',
                       titleAttr = 'Excel'),
                  list(extend = 'print',
                       text = '<i class="fa fa-file-pdf-o"></i>',
                       titleAttr = 'PDF')
                ),
                stateSave = TRUE,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Swedish.json'),
                searching = TRUE, 
                autoFill = FALSE, 
                ordering = TRUE, 
                lengthMenu = list(c(10, 25, 50, 100, -1), 
                                  c('10', '25', '50', '100','Alla')),
                pageLength = 25,
                lengthChange = TRUE, scrollX = TRUE, scrollY = FALSE, 
                paging = TRUE)
    ) |> 
      formatCurrency(c(2), currency = "kr", before = FALSE)
    
  })
  
  proxyPaymentsList <- dataTableProxy("paymentsList")
  
   
  # on session end #### 
  onSessionEnded(function() {
    dbDisconnect(consqlGlobal)
  })
  onUnhandledError(function(err){
    dbDisconnect(consqlGlobal)
  })
  onStop(function(err){
    dbDisconnect(consqlGlobal)
  })
  # onStop(function() {dbDisconnect(consqlGlobal)})
  
}