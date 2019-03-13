# Load necessary libraries
library(shiny)
library(DT)
library(plotly)
library(crosstalk)


# Load data (enter in the path to the downloaded data csv file between the "")
data <- read.csv("Dataset_S2_for_app.csv")

# You should now have to do anything else othe than click on the >Run App in 
#the top right hand cornor

m <- data %>%
        tibble::rownames_to_column()

ui <- fluidPage(
        h1("Random Bar Code Transposon Sequencing Results for Strain Fitness Analysis of Synechococcus sp. PCC 7942 in Light-Dark Cycles", align = "left"),
        h4("Genome-wide fitness assessment during diurnal growth reveals an expanded role of the cyanobacterial circadian clock protein KaiA"),
        h5("David G. Welkie, Benjamin E. Rubin, Yong-Gang Chang, Spencer Diamond, Scott A. Rifkin, Andy LiWang, and Susan S. Golden"),
        h5("PNAS July 24, 2018 115 (30) E7174-E7183; https://doi.org/10.1073/pnas.1802940115"),
        h5("Random barcode transposon sequencing (RB-TnSeq) is a method that exploits transposon mutagenesis coupled to high throughput sequencing. Breifly, a dense mutant library is created with each member of the library containing a unique barcode. The library is grown up and genomic DNA is sequenced to connect each barcoded transposon to the surrounding sequence, so the barcode serves as an identity tag for each mutant. After initial linking of transposon barcodes to surrounding sequence the library can be used for pooled screening for fitness contribution of individual loci under specific growth conditions. In this approach the library is split into one control condition (continuous light) and into any number of experimental conditions (in this case 12h light-12h dark cycles). After a period of growth the culture grown in each condition is screened by high throughput sequencing of the barcodes. The count of each barcode is inversely correlated with the importance of the associated gene in the condition sampled. By using pooled barcoded mutants, this approach plays into the strengths of next generation sequencing, resulting in an unbiased, inexpensive, and quantitative whole genome screen."), 

        plotlyOutput("x2"),
        verbatimTextOutput("hover"),
        verbatimTextOutput("click"),
        verbatimTextOutput("brush"),
        verbatimTextOutput("zoom"),
        DT::dataTableOutput("x1"),
        fluidRow(
                p(class = 'text-center', downloadButton('x3', 'Download Filtered Data')),
        h5(
"See manuscript for full citations of data, some of which has been added here from : 
i) Rubin BE, Wetmore KM, Price MN, Diamond S, Shultzaberger RK, Lowe LC, Curtin G, Arkin AP, Deutschbauer A, Golden SS. 2015. The essential gene set of a photosynthetic organism. Proc Natl Acad Sci U S A doi:10.1073/pnas.1519220112
ii) Markson JS, Piechura JR, Puszynska AM, O'Shea EK. 2013. Circadian control of global gene expression by the cyanobacterial master regulator RpaA. Cell 155:1396-1408, 
iii) Diamond S, Rubin BE, Shultzaberger RK, Chen Y, Barber CD, Golden SS. 2017. Redox crisis underlies conditional light-dark lethality in cyanobacterial mutants that lack the circadian regulator, RpaA. Proc Natl Acad Sci U S A 114:E580-E589", align = "center")
        )
)

server <- function(input, output) {
        
        d <- SharedData$new(m, ~rowname)
        
        # highlight selected rows in the scatterplot
        output$x2 <- renderPlotly({
                
                s <- input$x1_rows_selected
                if (!length(s)) {
                        p <- d %>%
                                plot_ly(x = ~LDC_Fitness_Score, y = ~FDR, mode = "markers",
                                        text = ~paste(Gene_______________________
                                                      ,'<br>', Symbol),
                                        color = ~Fitness_of_mutant_in_LDC, name = '') %>%
                                layout(yaxis = list(autorange = "reversed")) %>% 
                                layout(yaxis = list(type = "log")) %>%
                                layout(showlegend = T) %>%
                                highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = 'Filtered'))
                } else if (length(s)) {
                        pp <- m %>%
                                plot_ly() %>%
                                add_trace(x = ~LDC_Fitness_Score, y = ~FDR, mode = "markers", color = I('black'), name = 'Unfiltered') %>%
                                layout(yaxis = list(autorange = "reversed")) %>% 
                                layout(yaxis = list(type = "log")) %>%
                                layout(showlegend = T)
                        
                        # selected data
                        pp <- add_trace(pp, data = m[s, , drop = F], x = ~LDC_Fitness_Score, y = ~FDR, mode = "markers",
                                        color = I('red'), name = 'Filtered') %>%
                                layout(yaxis = list(autorange = "reversed"))  %>%
                                layout(yaxis = list(type = "log"))
                }
                
        })
        
        # highlight selected rows in the table
        output$x1 <- DT::renderDataTable({
                m2 <- m[d$selection(),]
                dt <- DT::datatable(m)
                if (NROW(m2) == 0) {
                        dt
                } else {
                        DT::formatStyle(dt, options = list(
                                pageLength = 5, autoWidth = TRUE), "rowname", target = "row",
                                color = DT::styleEqual(m2$rowname, rep("white", length(m2$rowname))),
                                backgroundColor = DT::styleEqual(m2$rowname, rep("black", length(m2$rowname))))
                }
                
                
                datatable(data, filter = 'top', options = list(
                        pageLength = 5, autoWidth = TRUE))
                
        })
        
        # download the filtered data
        output$x3 = downloadHandler('filteredlist.csv', content = function(file) {
                s <- input$x1_rows_selected
                if (length(s)) {
                        write.csv(m[s, , drop = FALSE], file)
                } else if (!length(s)) {
                        write.csv(m[d$selection(),], file)
                }
        })
        
        output$x4 <- renderPlotly({
                plot_ly(x1) %>%
                        add_pie(data = count(data, LDC_Fitness_Score), labels = ~LDC_Fitness_Score, values = ~n,
                                name = "Phenotype", domain = list(x = c(0, 0.4), y = c(0.4, 1)))
        })
        
        #output$hover <- renderPrint({
        #  d <- event_data("plotly_hover")
        #  if (is.null(d)) "Hover events appear here (unhover to clear)" else d
        #})
        
        #output$click <- renderPrint({
        #  d <- event_data("plotly_click")
        #  if (is.null(d)) "Click events appear here (double-click to clear)" else d
        #})
        
        output$brush <- renderPrint({
                
                d <- event_data("plotly_selected")
                if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d
        })
        
        #output$zoom <- renderPrint({
        #  d <- event_data("plotly_relayout")
        #  if (is.null(d)) "Relayout (i.e., zoom) events appear here" else d
        #})
}

shinyApp(ui, server)

