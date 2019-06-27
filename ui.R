library(ggplot2)
library(shiny)
library(dplyr)
library(plotly)

dummyDep <- c('Title 31',
              'CageVaultCashiers',
              'Cashiers',
              'Casino Shift Managers',
              'Floor Attendants',
              'Gold Rush',
              'Drop Crew')

dummyPeople <- c('Hannah',
                 'Deanna',
                 'Pat',
                 'Shawn',
                 'Sharon',
                 'Maria',
                 'Roseanna',
                 'Phil',
                 'Sherri',
                 'Diane',
                 'Janine',
                 'Babbette',
                 'Michele',
                 'Ruthann',
                 'Sharon',
                 'Terri',
                 'Rachel')

dummyErrors <- c('Recon Error',
                 'BJ Drop',
                 'Deposit Error',
                 'Cashier open/close form',
                 'Change bank',
                 'Cleaner Tips',
                 'Tips',
                 'Did not post Table games tips',
                 'Did not sign',
                 'Did not specify misc',
                 'Did not submit cash advance',
                 'Did not submit receipt',
                 'Did not submit synopsis',
                 'Did not verify Cage',
                 'Did not void',
                 'EXCHANGE ERROR',
                 'F&B/Bar Tips',
                 'Incorrectly posted jackpots',
                 'JACKPOT SIGNATURE',
                 'Kiosk boxes',
                 'Misc on Recon',
                 'Signature',
                 'Promos BJ/GRC',
                 'TG tips',
                 'TITO/JP on recon',
                 'Verify Cage',
                 'Verify Tips',
                 'VOID')

ui <- fluidPage(
  
  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(type = "tabs",
              
      tabPanel("New", hr(),
               fluidRow(
                 column(1),
                 column(11,
                        h2("Finance Reporter"),
                        p("Enter and sumbit financal errors by departement and individual basis."),
                        selectInput('dep', 'Department', choice=dummyDep),
                        selectInput('person', 'Person', choice=dummyPeople),
                        selectInput('cat', 'Error Catagory', choice=dummyErrors),
                        textAreaInput('comment', 'Error Description'),
                        dateInput('errorDate', 'Error Date'),
                        actionButton('submitButton', 'Submit')))
               ),
      
      tabPanel("Plots", 
               
               fluidRow(
                 column(2, selectInput('dep', 'Department', choice=df$DEP)),
                 column(2, selectInput('personPlot', 'Person', choice=df[,'PEOPLE'], selected = 'All')),
                 column(2, dateRangeInput('dateRange', label='Date Range', start = min(df$DATE), end = max(df$DATE)))
                 ),
               
               fluidRow(
                 column(6, plotlyOutput('errorByPerson')),
                 column(6, plotlyOutput('errorTimeLine'))
                 ),
               fluidRow(column(12,hr())),
               fluidRow(
                 column(12, plotlyOutput('generalAll'))
                 )
               )
      )
)
df <- getDummyData()
head(df)

server <- function(input, output, session) {
  
  #error timeline view -------------------------------------------------------
  output$errorTimeLine <- renderPlotly({
    timedf <- df

    d <- input$dateRange
    
    plotdf <- timedf %>% 
      filter(PEOPLE == input$personPlot, DATE >= d[1] & DATE <= d[2]) %>% 
      group_by(DATE) %>% summarise(TOTAL = n())
    
    print(plotdf)
    
    l <- ggplot(data = plotdf) +
      geom_line(mapping = aes(x = DATE, y = TOTAL), stat = 'identity', color = 'red', size = .7) +
      geom_point(mapping = aes(x=DATE, y = TOTAL, text = paste("Date:", DATE, "<br>Total Error:", TOTAL)), 
                 stat = 'identity', size = .7)
    ggplotly(l, tooltip = "text")
  })
  
  #total errors for each person view ---------------------------------------------
  output$errorByPerson <- renderPlotly({
    
    df2 <- df
    d <- input$dateRange
    
    df2 <- df2 %>% filter(PEOPLE == input$personPlot, DATE >= d[1] & DATE <= d[2]) %>% 
      group_by(ERROR) %>% 
      summarise(TOTAL = n())
    
    l <- ggplot(data = df2) +
      geom_bar(mapping = aes(x = ERROR, 
                             y = TOTAL, 
                             fill = ERROR, 
                             text = paste("Error:", 
                                          ERROR, 
                                          "<br>Total:", 
                                          TOTAL)), 
               stat = 'identity') +
      
      ggtitle(paste('Total error count:', sum(df2$TOTAL), "for", input$personPlot, "between", d[1], "/", d[2])) +
      xlab(input$personPlot) +
      ylab('Error Count') +
      theme(axis.text.x = element_text(angle = -45), legend.position = "none")
    ggplotly(l, tooltip = "text")
  })
  
  #General view --------------------------------------------------------------------
  output$generalAll <- renderPlotly({
    nums <- df %>% group_by(PEOPLE) %>% summarise(TOTAL = n())
    p <- ggplot(nums, aes(x = PEOPLE, y = TOTAL, fill = PEOPLE, 
                     text = paste("Person: ", PEOPLE, "<br>Total Errors:", TOTAL))) + 
      ggtitle(paste('Total error count: ',nrow(df))) + 
      geom_bar(stat='identity') +
      theme(axis.text.x = element_text(size = 7)) +
      xlab('Staff') + 
      ylab('Total Errors')
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)
