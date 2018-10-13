censusdata = read_csv('finaldata.csv')
freddata = read_csv('saverate.csv')

function(input, output, session) {

## HOME GRAPHS #####

  output$plot <- renderPlotly({
    plot_ly(freddata, x = freddata$uniqueyear, y = freddata$avgsave, type = 'scatter', mode = "lines") %>% 
      add_trace( x = freddata$uniqueyear, y = freddata$avgsave, frame = freddata$uniqueyear,
                 type = 'scatter', marker = list(color = 'red', size = 15)
        
      ) %>%
      layout(xaxis = x, yaxis = y, showlegend = FALSE)
  })
  
  x <- list(
    title = "Year"
  )
  
  y <- list(
    title = "Save Rate (%)")
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "This will show the average saving rate in decimal format for a given year" else d
  })
 
  
## QUICK FACTS GRAPHS ####
  output$Income <- renderPlot({
    ggplot(data = censusdata, 
           aes(x=censusdata$Region, y=censusdata$Income, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + labs(fill = "Region") +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            legend.position="none"
            ) 
  })
  output$Education <- renderPlot({
    ggplot(data = censusdata, 
           aes(x=censusdata$Region, y=censusdata$Education, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + labs(fill = "Region") +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none" )
  })
  
  output$alcohol <- renderPlot({
    ggplot(data = censusdata, 
           aes(x=censusdata$Region, y=censusdata$Alcohol, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + labs(fill = "Region") +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none" )
  })
  
  output$clothes <- renderPlot({
    ggplot(data = censusdata, 
           aes(x=censusdata$Region, y=censusdata$Apparel, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + labs(fill = "Region") +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none" )
  })
  
  
 # BOX STATS 
  
  output$value1 <- renderValueBox({
    valueBox(
      paste0("$",formatC(max(censusdata$Income), format="d", big.mark=','))
      ,HTML(paste("<b>",'Highest Income:',"</b>", censusdata$State[which.max(censusdata$Income)])
      )) 
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      paste0("$",formatC(max(censusdata$Food), format="d", big.mark=','))
      ,HTML(paste("<b>",'Highest Food Cost:',"</b>", censusdata$Region[which.max(censusdata$Food)])
      ))  
  })
  output$value3 <- renderValueBox({
    valueBox(
      paste0("$",formatC(max(censusdata$Healthcare), format="d", big.mark=','))
      ,HTML(paste("<b>",'Highest Healthcare Cost:',"</b>", censusdata$Region[which.max(censusdata$Healthcare)])
      ))  
  })
 
  output$table <- DT::renderDataTable({
    DT::datatable(censusdata[,-1])
  })

## CALCULATOR COST OF LIVING ####

  output$bat <- renderText({
    HTML(paste("To maintain your standard of living in ",
    as.character(input$from), " you should earn:", 
    paste0("$",censusdata$Income[input$to == censusdata$State]),
    ", which is",
    percent(1-(min(censusdata$Income[input$to == censusdata$State], input$income)/max(censusdata$Income[input$to == censusdata$State], input$income))),
    ifelse(censusdata$Income[input$to == censusdata$State] < input$income, "cheaper!", "more expensive!")))
  })


## CALCULATOR MY PLAN ##### 
  
  ## TABS ##
  values <- reactiveValues()
  
  Month <- c(1,2,3,4,5,6,7,8,9,10,11,12)              
  Expenditure <- c(2,4,6,8,10,12,14,16,18,20,22,24)
  Fixed <- c(1,2,3,4,4,5,6,7,8,9,10,12)
  Save <- c(7,8,9,10,11,12,13,14,15,16,17,18)
  Income <- c(60,55,44,77,99,88,35,86,90,100,467,124)
  
  df <- data.frame(Month, Expenditure, Fixed, Save, Income)
  
  
  output$contents <- renderRHandsontable({
    
    rhandsontable(df, width = 550, height = 300) %>%    
      hot_col(col = "Month", type = "dropdown")
  })
  
  saveData <- eventReactive({input$saveBtn},{
    finalDF <- hot_to_r(input$contents)
    finalDF$Month <- ifelse(finalDF$Month =="",NA,finalDF$Month)
    newDF <- finalDF[complete.cases(finalDF),]
    return(newDF)
  })
  
  output$contentFinal <- renderRHandsontable(
    rhandsontable(saveData())
  )
  
  output$dashboard1 <- renderPlot(
    ggplot(hot_to_r(input$contentFinal), aes(x = Month, y = Expenditure )) +
      geom_bar(stat = "identity", fill = "orange")
    
  )
  
  output$dashboard2 <- renderPlot(
    ggplot(hot_to_r(input$contentFinal), aes(x = Month, y = Save )) +
      geom_bar(stat = "identity", fill = "blue")
    
  )
  observeEvent(input$saveBtn, saveData())
  
  ## DAILY WEEKLY CALCULATION ##
  
  num_to_month = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  daysmonth = c(30, 31, 28)
  
  output$daily <- renderValueBox({
    valueBox(
      formatC(input$bees/ifelse(input$month %in% c(4, 6, 9, 11), 30, ifelse(input$month %in% c(1,3, 5, 7,8, 10, 12), 31, 28)), format="d", big.mark=','),
      paste('Daily'))  
  })
  
  output$weekly <- renderValueBox({
    valueBox(
      formatC(input$bees/4, format="d", big.mark=',')
      ,paste('Weekly'))  
  })
  
  output$monthly <- renderValueBox({
    valueBox(
      formatC(input$bees/2, format="d", big.mark=',')
      ,paste('Bi-weekly'))  
  })

 
}

