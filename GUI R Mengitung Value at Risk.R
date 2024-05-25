#============PROGRAM PERHITUNGAN VALUE AT RISK : DISTRIBUSI NORMAL============

install.packages("shiny")
install.packages("DT")
library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel(title = "Perhitungan Value at Risk: Distribusi Normal"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_excel", "Input File Excel",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      textInput("investasi", "Modal Awal"),
      textInput("alpha", "Alpha"),
      textInput("hp", "Holding Period"),
      actionButton("hitung", "Hitung")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Preview Data", h3("Berikut Ini adalah Data Return Saham", style =
                                      "font-family:'Montserrat';color:blue;text-align:center"), DTOutput("tabel")),
        tabPanel("Perhitungan VaR",
                 tags$h5('Mean:'),
                 verbatimTextOutput("mean"),
                 tags$h5('Standar Deviasi:'),
                 verbatimTextOutput("sd"),
                 tags$h5('Jumlah Data:'),
                 verbatimTextOutput("n"),
                 tags$h5('Nilai Z:'),
                 verbatimTextOutput("z"),
                 tags$h5('Value at Risk:'),
                 verbatimTextOutput("var"),
                 tags$h5('Persen Value at Risk:'),
                 verbatimTextOutput("persen_var")
        )
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  # Output Tabel
  output$tabel <- renderDT({
    file_excel <- input$file_excel
    if (is.null(file_excel))
      return(NULL)
    read.csv(file_excel$datapath, sep = input$pemisah)
  })
  
  # Output VaR
  observeEvent(input$hitung, {
    file_excel <- input$file_excel
    if (is.null(file_excel))
      return(NULL)
    data <- read.csv(file_excel$datapath, sep = input$pemisah)
    data_return <- data[, 1]  
    s0 <- as.numeric(input$investasi)
    hp <- as.numeric(input$hp)
    alpha <- as.numeric(input$alpha)
    
    # Menghitung z value sesuai dengan alpha
    z_value <- qnorm(1 - alpha)
    
    VaR_Z <- -(s0 * z_value * sd(data_return) * sqrt(hp))
    persen_VaR_Z <- (VaR_Z / s0) * 100
    
    output$mean <- renderPrint(mean(data_return))
    output$sd <- renderPrint(sd(data_return))
    output$n <- renderPrint(length(data_return))
    output$z <- renderPrint(z_value)
    output$var <- renderPrint(VaR_Z)
    output$persen_var <- renderPrint(persen_VaR_Z)
  })
}

shinyApp(ui = ui, server = server)
