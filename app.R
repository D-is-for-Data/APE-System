
# Global ------------------------------------------------------------------

# install.packages('semantic.dashboard') <- for nicer look of shiny apps

require(shiny)
require(semantic.dashboard)
require(ggplot2)
require(dplyr)
require(DT)

helpData <- data.frame('step'=c(1,2),
                       'intro'=c('Welcome to the intro, let me show you around.','Ummm'),
                       'element'=c(NA,NA),
                       'position'=c('auto','auto'))

# Data Input --------------------------------------------------------------

# install.packages('randomNames')
# require(randomNames)
# 
# set.seed(123)
# data = data.frame(student=randomNames(20,name.order = 'first.last',name.sep = ' '),
#                   n_comments=sample(1:10,20,replace = T),
#                   similarity=runif(20,0,1),
#                   grade=sample(3:10,20,replace = T))
# write.csv(data,'student_data.csv',row.names = F)



# UI ----------------------------------------------------------------------


ui <- dashboardPage(
  
  dashboardHeader(title = "APES", fixed=T,color = "teal",inverted = T, tags$li(class = "dropdown", 
                                                                                       style = "padding: 0px 0px 10px 0px;font-size:0%;",
                                                                                       tags$h2("APES")),
                  
                  # Info Button
                  tags$button(
                    class = "right floated circular ui info icon button", style = "margin: 18px;",
                    tags$i(class = "info icon")
                  ),
                  div(
                    class = "ui info modal",
                    div(class = "header", "Info"),
                    div(
                      class = "content",
                      h5(
                        "APES is an Online Participation Evaluation System created through collaborartion of IE professors and students."
                      ),
                      h5(
                        "We hope you have a seamless experience!"
                      ),
                      h6(
                        "Developed at",
                        a("IE University", href = "https://www.ie.edu/university/", target = "_blank"),
                        "."
                      )
                    )
                  ),
                  tags$script("$('.info.modal').modal('attach events', '.info.button', 'show');")),
  # Sidebar Menu
  dashboardSidebar(
    size = "thin", color = "teal",
    sidebarMenu(
      menuItem(tabName = "data", "Import Data", icon = icon("table")),
      menuItem(tabName = "main", "Main", icon = icon("home")),
      menuItem(tabName = "summary", "Summary", icon = icon("users")),
      menuItem(tabName = 'cloud', 'Word Cloud', icon = icon('cloud')),
      menuItem(tabName = 'help', 'Help', icon = icon('question')))),
  
  # Body
  dashboardBody(
    

        tabItems(
          selected = 1,
          
          # Import Data Page
          tabItem(
            tabName = "data",
            fluidRow(
              column(4),
              column(8,
                     HTML("<br><br><center> <h1>Welcome to APES!</h1> </center>"),
                     HTML("<h5><center>To start please upload the text file containing 
                  your student's comments, as well as the summary of your lecture. You can then view
                  insights on each student in terms of their number of useful comments and their similarity 
                  to the provided summary on the following pages.<center><h5><br><br>")
              )),
            
            fluidRow(column(16,HTML("<hr>"))),

            #Import Student Data
            fluidRow(
              column(2),
              column(16,
                     HTML("<b>STEP 1:</b> Import the Student Comment File by clicking on 'Browse...'"))
            ),
            fluidRow(
              column(6),
                     fileInput('file','Select File',multiple = F, 
                                 accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))),

            fluidRow(column(4),
              box(width=8,title='Student Data',
                         color = "teal", ribbon = T, title_side = "top left",
                         column(width = 8,
                                dataTableOutput("datatable")))),

          # Import Lecture Data  
          fluidRow(
            column(2),
            column(16,
                   HTML("<b>STEP 2:</b> Now import the Lecture Summary File"))
          ),
          
          fluidRow(
            column(6),
            fileInput('file2','Select File',multiple = F, 
                      accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))),
          
          fluidRow(column(4),
                   box(width=8,title='Lecture Data',
                       color = "teal", ribbon = T, title_side = "top left",
                       column(width = 8,
                              dataTableOutput("datatable2"))))),
      
          # Main Page
            tabItem(
            tabName = "main",
            selectizeInput(inputId = 'name',
                           label = 'Choose Name of Student:',
                           choices = c('Please first upload data.'),
                           options = list(placeholder='Type name...',
                                          onInitialize = I('function() { this.setValue(""); }'))),
            fluidRow(
              
            column(8,
              
              box(width = 8,
                  title = "Useful Comments",
                  color = "teal", ribbon = T, title_side = "top left",
                  column(width = 8,
                         valueBoxOutput('num_comments')))),
              box(width = 8,
                  title = "Similarity",
                  color = "teal", ribbon = T, title_side = "top left",
                  column(width = 8,
                         valueBoxOutput('similarity_comments')))),
            
            fluidRow(
              
              column(8,
              
              box(width = 8,
                  title = "Useful Comments",
                  color = "teal", ribbon = T, title_side = "top left",
                  column(width = 8,
                         plotOutput("plot1")))),
              box(width = 8,
                  title = "Similarity",
                  color = "teal", ribbon = T, title_side = "top left",
                  column(width = 8,
                         plotOutput("plot2"))))
          ),
          tabItem(
            tabName = 'help',
            fluidRow(
              h2('This is the help page.')
            )
          )
          ) #tabItems bracket
        ), #dashboard body bracket
  
  theme = "cosmo") #dashboard page bracket


# Server ------------------------------------------------------------------


server <- shinyServer(function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)})
  
  output$datatable <- renderDataTable({
    req(input$file)
    df <- input$file
    read.csv(df$datapath)
  })
  
  observe({updateSelectizeInput(session,'name',
                       choices=data()$student,
                       selected = NULL)})

  value1 <- reactiveVal(NA)
  
  observeEvent(input$name,{
    newValue1 <- as.numeric(data() %>%
                              filter(student==as.character(input$name)) %>%
                              select(n_comments))
  value1(newValue1)  
  })
  
  output$num_comments <- renderValueBox({valueBox(paste('By',input$name),value1(), color = 'teal')})
  
  
  value2 <- reactiveVal(NA)
  
  observeEvent(input$name,{
    newValue2 <- as.numeric(data() %>%
                              filter(student==as.character(input$name)) %>%
                              select(similarity))
    value2(newValue2)  
  })
  
  output$similarity_comments <- renderValueBox({valueBox(paste('By',input$name),paste(round(value2(),2),'/1'), color = 'teal')})
    
  
  output$plot1 <- renderPlot({
    
    if (is.null(data())) {
      return(NULL)
    } else {
    
    data() %>%
      mutate(highlight=ifelse(student==input$name,'name','other')) %>%
      ggplot(aes(x=reorder(student,n_comments),y=n_comments,fill=highlight)) +
      geom_bar(stat='identity') +
      scale_fill_manual(values = c('name'=semantic_palette[["teal"]],'other'=semantic_palette[["grey"]])) +
      coord_flip() +
      ggtitle('Number of Useful Comments per Student') +
      xlab('') +
      ylab('Useful Comments') +
      theme_minimal() +
      theme(plot.title = element_text(size=16,face='bold',hjust = 0.5),
            axis.title.x = element_text(hjust = 0.99),
            axis.text.y = element_text(size=12),
            axis.text.x = element_text(size=10),
            legend.position = 'none')}})
  
  output$plot2 <- renderPlot({
    
    if (is.null(data())) {
      return(NULL)
    } else {
    
    data() %>%
      mutate(highlight=ifelse(student==input$name,'name','other')) %>%
      ggplot(aes(x=reorder(student,similarity),y=similarity, fill=highlight))+
      geom_bar(stat='identity')+
      scale_fill_manual(values = c('name'=semantic_palette[["teal"]],'other'=semantic_palette[["grey"]])) +
      coord_flip() +
      ggtitle('Similarity Score of each Student') +
      xlab('') +
      ylab('Similarity Score of Comments') +
      theme_minimal() +
      theme(plot.title = element_text(size=16,face='bold',hjust = 0.5),
            axis.title.x = element_text(hjust = 0.99),
            axis.text.y = element_text(size=12),
            axis.text.x = element_text(size=10),
            legend.position = 'none')}})

})


# Run App -----------------------------------------------------------------

shinyApp(ui, server)

