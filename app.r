library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)


source("R test.R")


ui <- navbarPage("Online Activism Analyze",
  tabPanel("Introduction",
           h2("The reason why we chose to analyze Online Activism"),
           h3("Online activism may be a term that many people are unfamiliar with. As a product of the new era, it is a way to express our voice and defend our rights and interests, so I think this topic is full of interest. 
              In the following part, we will analyze the success rate of Internet activism in the world, and try to combine online activism with CSR to find the connection between them."),
           imageOutput("homeimg")
           ),
  
  tabPanel("The Global Success Rate of Online Activism(2010-2012)",
           h2("Before we start, let's do a little test!"),
           p("Please answer the following questions(Just guess)"),
           textInput(
             inputId = "name",
             label = "What is your name?",
           ),
           radioButtons(
             inputId = "govtype",
             label = "Under which type of government does online activism have the highest success rate?",
             choices = list("Democracy" = 1, "Emerging Democracy" = 2, "Authoritarian" = 3, "Transition" = 4)
           ),
           h3("Your Answers:"),
           textOutput(outputId = "greetings"),
           textOutput(outputId = "feedback"),
           plotOutput(outputId = "govplot"),
           h2("What about violence?"),
           h3(paste("The success rate of online activism where violence has occurred is in", viol_achieved_percent, "%."))
           ),
  tabPanel("Online Activism & CSR Companies",
           h3("In this section we are going to explore the relationship between the success rate of online activism and the number of CSR companies in different countries."),
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 inputId = "country",
                 label = "Locate a country",
                 choices = merge_df$Countryname
               )
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Table", tableOutput(outputId = "table")),
                 tabPanel("Plot", plotlyOutput(outputId = "plot")),
                 tabPanel("About",
                          h3("About the Data"),
                          p("As can be seen from the figure, there are over 3,178 CSR companies in the United States. 
                            Meanwhile, compared with other countries, the number of online activism in the United States is also much higher. 
                            The trend is similar in Britain. Although there are many factors affecting these two data, it is not difficult to see that 
                            the number of CSR companies in a country increases with the number of online activism.")
                 )
               )
             )
           ),
           ),
  tabPanel("Success rate of online activism and the number of CSR companies in different country",
           h3("In addition to analyzing the impact on CSR by the number of online activism, we also try to analyze it by the success rate of online activism."),
           plotlyOutput(outputId = "rate"),
           h4("As can be seen from the plot in the figure, the success rate of online activism in most countries is around 50%, 
              and the average score of local CSR enterprises is between 87-102. When the success rate of online activism exceeds about 66 percent, 
              we can see an obvious upward trend in the average score of CSR companies. And when the success rate is around 100 percent, their average scoring ceiling becomes even higher.")
           ),
  tabPanel("Summary",
           h2("Summary"),
           h3("Through this analysis, we learned about the success rate of online activism under different factors and its impact on corporate social responsibility. 
              In my opinion, cyber activism is a tool for us to convey our own ideas and safeguard our own interests. We can predict that when the number of cyber activism incidents increases, 
              both the quantity and quality of CSR enterprises will increase to some extent."),
           h3("Due to the discontinuity and uniformity of the data, the correlation between the two may also be influenced by many other factors."),
           imageOutput("sumimg")
)
)
server <- function(input, output){
  output$greetings <- renderText({
    return(paste("Hi", input$name, "!"))
  })
  output$feedback <- renderText({
    if(input$govtype == 1){
      return(paste("Nice Try, online activism have", demo_achieved_percent, "% success rate under Democracy."))
    }else if(input$govtype == 2){
      return(paste("Nice Try, online activism have", emer_achieved_percent, "% success rate under Emerging Democracy."))
    }else if(input$govtype == 3){
      return(paste("Nice Try, online activism have", author_achieved_percent, "% success rate under Authoritarian."))
    }else if(input$govtype == 4){
      return(paste("Correct! online activism have", trans_achieved_percent, "% success rate under Transition."))
    }
  })
  output$govplot <- renderPlot({
    if(input$govtype == 4){
      gov_p <- ggplot(gov_df, aes(x=Governmenttype, y=AchievedPercent, fill=Governmenttype))+
        geom_bar(stat="identity")
      return(gov_p)
    }
  })
  output$table <- renderTable({
    df_table <- filter(merge_df, Countryname==input$country)
  return(df_table)
  })
  output$plot <- renderPlotly({
    p <- ggplot(merge_df, aes(x = total_act, y = total_CSR, color=Countryname, label = Countryname)) +
      geom_point() +
      labs(x="Number of online activism events", y="Number of CSR Companies") +
      geom_text(data = filter(merge_df, Countryname==input$country), color = "black")
  })
  output$rate <- renderPlotly({
    rate_p <- ggplot(data = merge_df, aes(x = achieved_percent, y = mean_CSR_score, group = 1, label = Countryname)) +
      labs(x = "The success rate of online activism", y = "The mean score of CSR Companies") +
      geom_line() +
      geom_point()
    return(rate_p)
  }) 
  output$homeimg <- renderImage({
    list(src = "ativism_banner2_0.png",
         width = "50%",
         height = 900)
  }, deleteFile = F)
  output$sumimg <- renderImage({
    list(src = "csr.png",
         width = "40%",
         height = 800)
  }, deleteFile = F)
}

shinyApp(ui = ui, server = server)
