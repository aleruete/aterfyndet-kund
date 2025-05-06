function(input, output, session) {

# Reactive values ####
  con <- reactiveVal()
  url <- reactiveValues(query = NULL)
  authorised <- reactiveValues(val = FALSE, isadmin = NULL, auth = NULL)
  users <- reactiveValues(data = NULL, username = NULL, memberId = NULL, hash = NULL)
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
        clients$data <- dbReadTable(conn = con(), "clients")
        
        message("You are in Neo")
        setProgress(1)
      } else { message("Couldn't connect"); stop() }
    }, message = "Läser in bakgrund data")
  })
  
  ### Pre populate some fields with url e.g. url/?userID=aleruete ####
  observe({
    url$query <- parseQueryString(session$clientData$url_search)

    if (!is.null(url$query[['memberId']])) {
      users$memberId <- url$query[['memberId']]
    }

  })
  
  # Login  #####  
  # observe({
  #   req(!authorised$val)
  #   loginDialog()
  # })
  # 
  # observeEvent(input$keyPressed, {
  #   shinyjs::click("login")
  # }) 
  
  ## hit login ####
  # observeEvent(input$login, {
  observeEvent(users$memberId, {
    if (is.na(users$memberId) | users$memberId == "")  return()
    
    # users$username <- input$username
    # users$hash <- encrypt_string(input$pass, key = keysecret)
    # updateTextInput(session, "pass", label = NULL, value = "")
    
    # removeModal()

    tryCatch({
      clients$selected <- clients$data |> 
        # filter(användare == users$username)
      filter(memberId == users$memberId)

      if (nrow(clients$selected) > 0) {
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
        return(NULL)
        # loginDialog()
      } #if authorised$val = FALSE

      if (authorised$val) {
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

      # Load rest of data from database ####
      withProgress({
          setProgress(.1)

          setProgress(.3)
          stock$data <- dbGetQuery(conn = con(), glue("SELECT * FROM stock WHERE id_kund = {clients$selected$id}")) |> 
            select(-id) |>
            mutate(id_kund = as.integer(clients$selected$id), 
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
  })
  
  
  ##client report ####
  output$selectedClient <- renderUI({
    req(clients$selected)
    if(nrow(clients$selected) > 0) {
    
      nitems <- stock$data |> 
        filter(id_kund == clients$selected$id, 
               giltig == 1) |> 
        reframe(items = n(),
                sold = sum(sald))
      
      tagList(
        fluidRow(
          column(width = 12,
                 h3(glue("{clients$selected$namn} (ID: {clients$selected$id})"))#,
                 # h4(glue("Antal inlämnat artiklar: {nitems$items}")),
                 # h5(glue("Antal sålda artiklar: {nitems$sold}"))
          )
        )
        
      )
    } else {
      tagList(
        fluidRow(
          column(width = 12,
                 h3("Det verkar som att din säljprofil inte är aktiverad."),
                 h4("Om det har gått mer än tre veckor sedan du lämnade in kläder, vänligen kontakta oss.")
          )
        )
      )
    }
  })
  
  output$clientDebt <- renderInfoBox({
    req(authorised$val)
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
      title = "Innestående saldo",
      value = glue("{payments$debt} kr"),
      color = "info", #"gray-dark",
      icon = icon("hand-holding-dollar")
    )
    
  })
  
  ## payments ####
  output$paymentsList <- renderDataTable({

    if (is.null(payments$data)) return(NULL)
    paymentstab <- payments$data |> 
      select(datum, belopp, betalningssatt) |> 
      mutate(#id_kund = as.factor(id_kund), 
        belopp = as.numeric(belopp), 
        datum = ymd(datum))#,
        # utbetald = as.logical(utbetald))
    
    datatable(paymentstab,
              # editable = list(target = 'cell', 
              #                 disable = list(columns = c(0:1,4:9))),
              class = 'row-border stripe compact hover',
              rownames = FALSE, selection = "none",
              extensions = 'Buttons', 
              # autoHideNavigation = TRUE,
              colnames = c("Datum", "Belopp", "Betalningssätt"), #"Kund id", , "Utbetald"
              # filter = "top",
              options = list(
                dom = 'tlBp', 
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
      formatCurrency(c(2), currency = " kr", before = FALSE)
    
  })
  
  proxyPaymentsList <- dataTableProxy("paymentsList")
  
  output$paymentsListUI <- renderUI({
    req(authorised$val)
    req(clients$selected)
    
    tagList(
      fluidRow(
        column(width = 12,
               br(),
               h5("Utbetalnings historik"),
               dataTableOutput("paymentsList")
        )
      )
      
    )
  })
  
   
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