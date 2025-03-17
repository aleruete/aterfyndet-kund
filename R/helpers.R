loginDialog <- function(){
  showModal(
    modalDialog(
      load_ui_content("ui/login.R"),
      footer = tagList(
        # actionBttn("register", "Registrera", icon = icon("user-plus", class = "fas"), 
        #            color = "primary", style = "jelly", size = "xs",
        #            onclick ="window.open('https://greensway.shinyapps.io/CanopyRegister/', '_blank')"),
        # actionBttn("forgot", "Lösenord", icon = icon("question", class = "fas"), 
        #            color = "warning", style = "jelly", size = "xs"),
        actionBttn("login", "Logga in", icon = icon("right-to-bracket", class = "fas"), 
                   color = "success", style = "jelly", size = "xs")
      ),
      size = "s",
      easyClose = FALSE)
  )
}

logInOutUI <- function(user, person, rol){
  
  dispName <- ifelse(person$data[,"personad"] == "", user$name, person$data[,"personad"])
  avatarName <- gsub(" ", "+", gsub(",", "", dispName))
  avatar <- paste0("https://eu.ui-avatars.com/api/?name=",avatarName, 
                   "&background=006635&color=fff&size=32")
  #### Alt https://avatar.oxro.io/
  
  rolTag <- ifelse(any(rol[,"id_rol"] %in% c(1,1008)),
                   "Administratör",
                   ifelse(any(rol[,"id_rol"] %in% c(1011)),
                          "Projekt administratör", "Användare"))
  tagList(
    userBlock(
      image = avatar,
      title = dispName,
      subtitle = rolTag
    )
  )
}

editItemUI <- function(selected) {
  showModal(
    modalDialog(
      title = "Redigera artikel",
      # h3(glue("{client$namn} (ID: {client$id})")),
      # numericInput("amountPay", "Belopp", 
      #              value = amount, step = .01,
      #              width = 250, min = 0),
      # 
      # selectInput("typePay", "Betalningssätt", 
      #             choices = c("swish", "inbyte"),
      #             width = 250),
      # dateInput("datePay", "Datum", value = Sys.Date(),
      #           width = 250),
      checkboxInput("itemSold", "Såld", value = as.logical(selected$sald)),
      checkboxInput("itemValid", "Giltig", value = as.logical(selected$giltig)),
      
      
      footer = tagList(
        actionBttn("cancelItem", NULL, 
                   icon = icon("xmark", class = "fas"), 
                   style = "material-circle", 
                   color = "danger", 
                   size = "xs"),  
        HTML("&nbsp;"),
        actionBttn("okItem", NULL, 
                   icon = icon("check", class = "fas"), 
                   style = "material-circle", 
                   color = "success", size = "xs")
      ), 
      easyClose = FALSE, fade = TRUE, size = "m") 
  )
}

payUI <- function(amount, client) {
  showModal(
    modalDialog(
      title = "Registrera utbetalning",
      h3(glue("{client$namn} (ID: {client$id})")),
      numericInput("amountPay", "Belopp", 
                   value = amount, step = .01,
                   width = 250, min = 0),
      
      selectInput("typePay", "Betalningssätt", 
                  choices = c("swish", "inbyte"),
                  width = 250),
      dateInput("datePay", "Datum", value = Sys.Date(),
                width = 250),
      checkboxInput("confirmPay", "Utbetald", value = TRUE),
      downloadLink("getTicket", ""),
      
      footer = tagList(
        actionBttn("cancelPay", NULL, 
                   icon = icon("xmark", class = "fas"), 
                   style = "material-circle", 
                   color = "danger", 
                   size = "xs"),  
        HTML("&nbsp;"),
        actionBttn("pay", NULL, 
                   icon = icon("money-bill-wave", class = "fas"), 
                   style = "material-circle", 
                   color = "success", size = "xs")
      ), 
      easyClose = FALSE, fade = TRUE, size = "m") 
  )
}