tagList(
  fixedRow(
    column(width = 10, offset = 1,
           h3("Login", style = "text-align: center"), 
           div(style = "display: inline-block; vertical-align:top; width: 20px; margin-left: 0px; margin-top: 15px", 
               icon("user", class = "fas")),
           div(style = "display: inline-block; vertical-align:top; width: 190px; margin: 0px;",
               textInput("username", label = NULL, placeholder = "Användarnamn")),
           br(),
           div(style = "display: inline-block; vertical-align:top; width: 20px; margin-left: 0px; margin-top: 15px", 
               icon("lock", class = "fas")),
           div(style = "display: inline-block; vertical-align:top; width: 190px; margin-left: 0px;",
               passwordInput("pass", label = NULL, placeholder = "Lösenord"))#,
           #  br(),
           #  div(style = "display: inline-block; vertical-align:top; float: right;",
           #      actionBttn("login", "Logga in", icon = icon("right-to-bracket", class = "fas"), 
           #                 color = "success", style = "jelly", size = "xs")),
           # div(style = "display: inline-block; vertical-align:top; margin-left: -15px;", ##float: right;
           #     actionBttn("register", "Registrera", icon = icon("user-plus", class = "fas"), 
           #                color = "primary", style = "jelly", size = "xs"))
    )
  )
)