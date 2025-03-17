ticket <- function(tmpdir, client, amount, date, type){
    withProgress(message = "Förbereder kviton", {
      unlink('ticket_cache', recursive = TRUE)
      setProgress(value = 0.1, detail = "Förbereder fil")
      
      # client <- clients$selected$id,
      # belopp <- input$amountPay,
      datum <- date
      # betalningssatt <- input$typePay,
      # utbetald <- input$confirmPay)

        
        setProgress(value = 0.6, detail = "Jag har allt data")
        quarto_render(
            'ticket.qmd',
            # output_format = "pdf",
            output_file = 'ticket.pdf',
            # execute_dir = tmpdir,
            execute_params = list(datum = date)
        )
        
        setProgress(value = 0.7, detail = "Kvitton är klart")
        
message(tmpdir)       
message(list.files(tmpdir))

        setProgress(value = 1, detail = "Här kommer kvitton")
         
    })
 }