# #### Format and export to csv for printer
# exportPrinter <- function(stock){
#   stockPrint <- stock |> 
#     select(id_kund, id_kategori, artikel, beskrivning, pris, kod) |>
#     rename("Kund id" = id_kund, "Kategori" = id_kategori, "Artikel nummer" = artikel,
#            "Beskrivning" = beskrivning, "Pris" = pris, "Kod" = kod)
#   
#   downloadHandler(
#     filename = function() {
#       paste("printer", input$client, now(), ".csv", sep = "_")
#     },
#     content = function(file) {
#       write.csv(stockPrint, file, row.names = FALSE)
#     }
#   )
# }