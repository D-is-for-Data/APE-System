# Global ------------------------------------------------------------------

require(shiny)
require(semantic.dashboard)
require(ggplot2)
require(dplyr)
require(DT)
require(wordcloud)
require(tm)
require(tidyr)
require(shinyalert)
require(lsa)
require(tidytext)
require(readr)

# Functions ---------------------------------------------------------------

# Cleaning Data
process<-function(wordcount,data){
  for(i in 1:nrow(data)){
    nwords<-sapply(strsplit(as.character(data[i,"Comment"]), " "), length)
    if(nwords<wordcount){
      data[i,"Comment"]<-NA
    }else{
      data[i,"Comment"]<-data[i,"Comment"]
    }
  }
  return(data)
} 

# Counting Number of Words
numbercomments<-function(data){
  ncomments<-data %>% filter(!is.na(Comment)) %>% group_by(Student) %>% tally()
  tablecomments<-data.frame('student'=as.character(ncomments$Student),'n_comments'=ncomments$n)
  return(tablecomments)
}

# Calculating Similarity
get_sim <- function(df_comment,df_summary) {
  df1 <- df_comment
  df2 <- df_summary
  df1 <- na.omit(df1)
  rownames(df1) <- NULL 
  colnames(df1) <- NULL
  df_comments = t(rbind(df1[2], df2))
  corpus = VCorpus(VectorSource(df_comments))
  my.tdm <- TermDocumentMatrix(corpus)
  ap_td = tidy(my.tdm)
  ap_td = ap_td %>% 
    cast_dfm(term, document, count) 
  my.df = (as.matrix(ap_td))
  sim = cosine(my.df)
  sim_row = sim[,dim(sim)[1]]
  df1$similarity = sim_row[1:(length(sim_row))]
  names(df1)<-c('student','comment','similarity')
  return(df1 %>%
    group_by(student) %>%
    summarise(similarity=sum(similarity)))
}


# Making Word Cloud Repeatable

wordcloud_rep <- repeatable(wordcloud)


# UI ----------------------------------------------------------------------

ui <- dashboardPage(theme = 'cosmo', 
                    dashboardHeader(title = "APES", 
                                    logo_path="", logo_align = 'left',
                                    color = "black",inverted = T,
                                    tags$li(class = "dropdown", 
                                            style = "padding: 0px 0px 10px 0px;font-size:0%;color:#ffffff;",
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
                                          a("IE University", href = "https://www.ie.edu/university/", target = "_blank")
                                          
                                        )
                                      )
                                    ),
                                    tags$script("$('.info.modal').modal('attach events', '.info.button', 'show');")),
                    # Sidebar Menu
                    dashboardSidebar(
                      size = "thin", color = "black", side='left', inverted=T,visible=T, center=T,
                      sidebarMenu(
                        menuItem(tabName = "data", "Import Data", icon = icon("table")),
                        menuItem(tabName = "main", "Main", icon = icon("home")),
                        menuItem(tabName = 'cloud', 'Word Cloud', icon = icon('cloud')),
                        # menuItem(tabName = "summary", "Summary", icon = icon("users")),
                        menuItem(tabName = 'help', 'Help', icon = icon('question')),
                        HTML('<br>'),
                        column(12,align='center',HTML('<a href = "https://www.google.com/forms/about/",
                                    target = "_blank">
                                    <button
                                    style = "background-color: #0066cc; 
                                    height:30px; width:80px;
                                    color: #fff;border-color: #0066cc"> Feedback
                                    </button></a>')))),
                    
                    # BodyR
                    dashboardBody(
                      
                      
                      tabItems(
                        selected = 1,
                        
                        # Import Data Page
                        tabItem(
                          tabName = "data",
                          useShinyalert(),
                          # fluidRow(
                          #   column(4),
                          #   column(8,
                          #          HTML("<br><br><center> <h1>Welcome to APES!</h1> </center>"),
                          #          HTML("<h5><center>To start please upload the file containing 
                          #       your student's comments and a text file with a summary of the respective lecture. You can then view
                          #       insights on each student in terms of their number of useful comments and their similarity 
                          #       to the provided summary on the main page.<center><h5><br><br>")
                          #   )),
                          # 
                          # fluidRow(column(16,HTML("<hr>"))),
                          
                          # Import Student Data
                          fluidRow(
                            tags$br(),
                            column(16),
                            box(width=8,
                            column(8,
                                   HTML("<p style='font-size:24px'><b>STEP 1:</b> Import the Student Comment File by clicking on 'Browse...'</p>"),
                                   column(2),
                                   fileInput('file','Select File',multiple = F, 
                                             accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                   tags$br())),
                            box(width=8,
                                column(8,
                                       HTML("<p style='font-size:24px'><b>STEP 2:</b> Now import the Lecture Summary File<br><br></p>"),
                                       column(2),
                                       fileInput('file2','Select File',multiple = F, 
                                                 accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                       tags$br())),
                            column(16,
                            box(width=8,title='Student Data',
                                color = "blue", ribbon = T, title_side = "top left",
                                column(width=8,
                                       dataTableOutput("datatable")))))),
                      


                        # Word Cloud
                        tabItem(
                          tabName = 'cloud',
                          fluidRow(column(4,
                                          HTML("<h3>Word Cloud Settings</h3>"),
                                          hr(),
                                          sliderInput('wordfreq', 'Select the minimum frequency of word', min = 1, max = 10, value = 2),
                                          HTML("<br>"),
                                          sliderInput('maxword', 'Select the maximum amount of words', min = 1, max = 200, value = 100),
                                          HTML("<br>"),
                                          selectInput("color", "Select the word cloud color theme", c("Dark2","Pastel1","Accent"), selected = "Dark2")
                          ),
                          column(12,
                                 plotOutput('wcplot'))
                          )),
                        
                        # Main Page
                        tabItem(
                          column(16),
                          tabName = "main",
                          selectizeInput(inputId = 'name',
                                         label = 'Choose Name of Student:',
                                         choices = c('Please first upload data.'),
                                         options = list(placeholder='Type name...',
                                                        onInitialize = I('function() { this.setValue(""); }'))),
                          fluidRow(
                            
                            column(8,
                                   
                                   box(width = 8,
                                       title = "Comments",
                                       color = "blue", ribbon = T, title_side = "top left",
                                       column(width = 8,
                                              valueBoxOutput('num_comments')))),
                            box(width = 8,
                                title = "Similarity",
                                color = "blue", ribbon = T, title_side = "top left",
                                column(width = 8,
                                       valueBoxOutput('similarity_comments')))),
                          
                          fluidRow(
                            
                            column(8,
                                   
                                   box(width = 8,
                                       title = "",
                                       color = "blue", ribbon = T, title_side = "top left",
                                       column(width = 8,
                                              plotOutput("plot1")))),
                            box(width = 8,
                                title = "",
                                color = "blue", ribbon = T, title_side = "top left",
                                column(width = 8,
                                       plotOutput("plot2"))))
                        ),
                        
                        tabItem(
                          tabName = 'help',
                          fluidRow(box(width = 8, color = "blue",
                                       h1(HTML('<center><b>Tips</b></center>'),noWS='outside'),
                                       h3(HTML("<ol>
                <li> It may be useful to look at the Word Cloud of student comments before writing a lecture summary</ol>"))),
                                   box(width = 8, color = "blue",
                                       h1(HTML('<center>Contact<center>'),noWS='outside'),
                                       h3(HTML("<ul>
                <li> For questions regarding the use of APES please contact"),
                                          a("email@sample.com", href = "mailto:email@sample.com?", target = "_blank"))))
                        )
                      ) #tabItems bracket
                    ) #dashboard body bracket
) #dashboard page bracket


# Server ------------------------------------------------------------------


server <- shinyServer(function(input, output, session) {
  
  # Welcome Message
  shinyalert(
    title = "Welcome to APES!",
    text = "We are here to assist you in the online participation evaluation process. 
    To start please upload the file containing your student's comments and a text file with a summary of the respective lecture.",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = "",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "Let's Go!",
    confirmButtonCol = "#0066cc",
    timer = 0,
    imageUrl = "https://static.thenounproject.com/png/1240957-200.png",
    animation = TRUE
  )
  
  # Data Input 1
  dataInput <- reactive({
    req(input$file)
    raw <- read.csv(input$file$datapath,sep = ":",header = F)
    names(raw)<-c("Student","Comment")
    process(4,raw)})
  
  organized_data <- reactive({
    req(input$file)
    numbercomments(dataInput())
  })
  
  output$datatable <- renderDataTable({
    req(input$file)
    dataInput() %>%
      filter(!is.na(Comment)) %>%
      group_by(Student) %>%
      mutate(ID = paste0("Comment", 1:n())) %>%
      ungroup() %>%
      spread(ID, Comment) %>%
      select(c("Student", paste0("Comment", 1:(ncol(.) - 1))))
  },options=list(scrollX=T,pageLength=3,lengthMenu = list(c(3, 5, 10, -1), c('3', '5', '10','All'))))
  
  
  # Data Input 2
  dataInput2 <- reactive({
    req(input$file2)
    as.character(read_file(input$file2$datapath))})
  
  sim_data <- reactive({
    req(input$file,input$file2)
    get_sim(dataInput(),dataInput2())
  })
  
  
  wc_data <- reactive({
    req(input$file)
    docs = Corpus(VectorSource(dataInput()$Comment))
    docs = tm_map(docs, content_transformer(tolower))
    docs = tm_map(docs, removePunctuation)
    docs = tm_map(docs, removeNumbers)
    docs = tm_map(docs, removeWords,
                  c(stopwords("SMART"), "the", "and", "but","dont"))
    dtm = TermDocumentMatrix(docs)
    m = as.matrix(dtm)
    v = sort(rowSums(m),decreasing=TRUE)
    data.frame(word = names(v),freq=v)
  })
  
  # World Cloud Plot
  
  output$wcplot <- renderPlot({
    
    if (is.null(dataInput())) {
      return(NULL)
    } else {
      
      wordcloud_rep(words=wc_data()$word,
                    freq=wc_data()$freq,
                    min.freq = input$wordfreq,
                    max.words = input$maxword,
                    colors = brewer.pal(8, as.character(input$color)),
                    rot.per = 0.1,
                    scale=c(8,1),
                    random.order=F,
                    fixed.asp=T)
    }
  },width = 1000, height = 900)
  
  
  # Main Page
  observe({updateSelectizeInput(session,'name',
                                choices=numbercomments(dataInput())$student,
                                selected = NULL)})
  
  #Value Boxes
  value1 <- reactiveVal('')
  
  observeEvent(input$name,{
    newValue1 <- as.numeric(organized_data() %>%
                              filter(student==as.character(input$name)) %>%
                              select(n_comments))
    value1(newValue1)  
  })
  
  output$num_comments <- renderValueBox({
    req(input$name)
    valueBox(paste('By',input$name),value1(), color = 'blue')
  })

  
  
  value2 <- reactiveVal(NA)

  observeEvent(input$name,{
    newValue2 <- as.numeric(sim_data() %>%
                              filter(student==as.character(input$name)) %>%
                              select(similarity))
    value2(newValue2)
  })

  output$similarity_comments <- renderValueBox({
    req(input$name)
    valueBox(paste('By',input$name),paste(round(value2(),2)), color = 'blue')})
  
  # Plots
  
  output$plot1 <- renderPlot({
    
    if (is.null(dataInput())) {
      return(NULL)
    } else {
      
      organized_data() %>%
        mutate(highlight=ifelse(student==input$name,'name','other')) %>%
        ggplot(aes(x=reorder(student,n_comments),y=n_comments,fill=highlight)) +
        geom_bar(stat='identity') +
        scale_fill_manual(values = c('name'=semantic_palette[["blue"]],'other'=semantic_palette[["grey"]])) +
        coord_flip() +
        ggtitle('Number of Comments per Student',subtitle = '(at least 4 Words)') +
        xlab('') +
        ylab('Comments') +
        theme_minimal() +
        theme(plot.title = element_text(size=16,face='bold',hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              axis.title.x = element_text(hjust = 0.99),
              axis.text.y = element_text(size=12),
              axis.text.x = element_text(size=10),
              legend.position = 'none')}})
  
  output$plot2 <- renderPlot({

    if (is.null(dataInput2())) {
      return(NULL)
    } else {

    sim_data() %>%
      mutate(highlight=ifelse(student==input$name,'name','other')) %>%
      ggplot(aes(x=reorder(student,similarity),y=similarity, fill=highlight))+
      geom_bar(stat='identity')+
      scale_fill_manual(values = c('name'=semantic_palette[["blue"]],'other'=semantic_palette[["grey"]])) +
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


