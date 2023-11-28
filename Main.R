## Author: Nicholas Winn
## ntwinn23@gmail.com
## Visualization of Transcriptional Reversion of Cardiac Myocyte Fate During Mammalian Cardiac Regeneration

# Import libraries
library(shiny)
library(bslib)
library(ggplot2)
library(colourpicker)
library(DT)
library(bslib)

### Define UI ###
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "sketchy"),
    titlePanel("BF591 RShiny Assignment"),
      sidebarLayout(
        sidebarPanel(
          fileInput(inputId = "upload", label = "Upload a CSV", accept = ".csv"),
          fileInput("metadata", label = "Upload metadata in CSV", accept = ".csv"),
          actionButton("submit_button", "Submit", icon = NULL)
        ),
        mainPanel(style='height: 90vh; overflow-y: auto;', 
          textOutput(outputId = "fileInfo"),
          actionButton("change_button", "Apply Changes", icon = NULL),
          tabsetPanel(
            tabPanel("Sample",
                      tableOutput("sample"),
              tabsetPanel(
                tabPanel("Metadata",
                      tableOutput("metadata")
                ),
                tabPanel("Table",
                      tableOutput("summary_table")
                ),
                tabPanel("Histogram",
                         plotOutput("histogram")
                ),
                tabPanel("Volcano plot",
                      plotOutput("volcano_plot"),
                      radioButtons("button_x", "Choose a column for the X-axis", 
                                   choices = c("baseMean", "log2FoldChange", "pvalue", "padj")),
                      radioButtons("button_y", "Choose a column for the Y-axis",
                                   choices = c("basemean", "log2FoldChange", "pvalue", "padj")),
                      colourInput("base", "Base point color", value = "#D4A5D9"),
                      colourInput("highlight", "Highlight point color", value = "#50C7B5"),
                      sliderInput("scale_adjust", "Select the magnitude of adjustment", 
                                  min = -300, max = 0, step = 10, value = -100)
                )
              )
            ),
            tabPanel("Count",
                      tableOutput("count")
            ),
            tabPanel("DE",
                      plotOutput("de")
            ),
            tabPanel("GSEA",
                      plotOutput("gsea")
            ),
          )
        )
      )
)

### Define server ###
server <- function(input, output, session) {

# Loading data #

# Increase data size for bigger data sets
  options(shiny.maxRequestSize = 30*1024^2)
 
# Sample data 
  load_data <- reactive({
    req(input$upload)
    data <- read.csv(input$upload$datapath)
    return(data)
  })

# Metadata in Sample tab
  meta_data <- reactive({
    req(input$metadata)
    meta <- read.csv(input$upload$datapath)
    return(meta)
  })
  
# Histogram in Sample tab
  histogram_plot <- function(datah, x_axis, y_axis) {
    
  }
    
# Volcano Plot
  volcano_plot <- function(datav, x_name, y_name, slider, color1, color2) {
    if (y_name == "padj"){
      datav$padj<-log10(datav$padj)
      scale_adjust <- slider
    }
    else {
      scale_adjust<-10^slider
    }
    theme_custom <- function (base_size = 11, base_family = "") {
      theme_bw() %+replace% 
        theme(
          panel.grid.major  = element_line(color = "pink"),
          panel.background = element_rect(fill = "black"),
          panel.border = element_rect(color = "pink", fill = NA),
          axis.line = element_line(color = "purple"),
          axis.ticks = element_line(color = "purple"),
          axis.text = element_text(color = "black")
        )
    }
        p <- ggplot(datav, aes(x = !!sym(x_name), y = !!sym(y_name), color = padj <= scale_adjust, size = "auto")) +
          geom_point() +
          scale_color_manual(values = c('FALSE'=color1, 'TRUE'=color2)) +
          scale_y_reverse()+
          labs(x = x_name, y = y_name, color=paste0('padj<10^',scale_adjust), title = "Volcano Plot")+
          theme_custom()
        
        return(p)
      }
  
#  data <- reactive({
#    req(input$upload)
#    
#    ext <- tools::file_ext(input$upload$name)
#    switch(ext,
#           csv = vroom::vroom(input$upload$datapath, delim = ","),
#           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
#           validate("Invalid file; Please upload a .csv or .tsv file")
#    )
#  })
#  
#  data_spec <- reactive({
#    req(input$upload)
#    
#    ext <- tools::file_ext(input$upload$name)
#    switch(ext,
#           csv = vroom::spec(input$upload$datapath, delim = ","),
#           tsv = vroom::spec(input$upload$datapath, delim = "\t"),
#           validate("Invalid file; Please upload a .csv or .tsv file")
#   )
#  })
  
# Tables and Plots # 
  
# Summary table
  sum_table <- function(dataf) {
    return(dataf)
  }
  
# Metadata table 
  meta_table <- function(datam) {
    return(datam)
  }
  
# Histogram plot
  histo <- function(datah) {
    return(datah)
  }

### Outputs ### 
  
# Summary table of counts data
  output$summary_table <- renderTable({
    isolate({
      sum_table(load_data())
    })
  })

# Metadata table
  output$summary_table <- renderTable({
    isolate({
      meta_table(load_data())
    })
  })
  
# Histogram plot
  output$histogram <- renderPlot({
    isolate({
      histogram_plot(load_data(), input$x_axis, y_axis)
    })
  })
  
# Volcano Plot
  output$volcano_plot <- renderPlot({
    input$change_button
    isolate({
      volcano_plot(load_data(), input$button_x, input$button_y, input$scale_adjust,
                   input$base, input$highlight)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)