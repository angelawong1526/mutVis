library(shiny)

ui <- fluidPage(

  # Title
  titlePanel("Location of mutations on protein and DNA sequence"),

  # Sidebar layout
  sidebarLayout(

    # Inputs
    sidebarPanel(
      textInput("unknownProt",
                label = h4("Protein Sequence"),
                value = "MAVLILVLLAVVILQAAPIRKLEDLLPTRYPPDHELVYWCTYANQCDFCWECVHGICRNRIQADWPVIHQNDWIINCTVSRWNGICSYYEGPRNHTDHQMDCANPTSHTYPHREYMKIYERDDL"),
      numericInput("minMatch",
                   label = "Minimum percentage of Match",
                   value = 85,
                   min = 1,
                   max = 100)
    ),

    # Main panel
    mainPanel(
      plotOutput("protPlot")
    )
  )
)

server <- function(input, output) {

  output$protPlot <- renderPlot({
    protPlot <- mutVis::matchProt(input$unknownProt, minMatch = input$minMatch)
    return(protPlot)
  })
}

shinyApp(ui = ui, server = server)
