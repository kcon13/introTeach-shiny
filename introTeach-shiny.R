library(shiny)
library(shinyBS)
library(ggplot2)
#library(devtools)
#install_github("stats295r-fa20/package-group13")
#library(introTeach)
source("functions.R")
ui <- fluidPage(
  titlePanel(tags$h1("Z and T Test Calculator", align = "center")),

  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "test",
                  label = actionLink("tl", "What kind of test should be performed?"),
                  choices = c("Z", "T")),

      numericInput(inputId = "mean",
                   label = actionLink("ml", "What is the mean?"),
                   0, step = 0.5),

      numericInput(inputId = "null",
                   label = actionLink("nl", "What is the expected value?"),
                   0, step = 0.5),

      selectInput(inputId = "alt",
                  label = actionLink("al", "What is the direcion of the alternative hypothesis?"),
                  choices = c("Greater than", "Less than", "Not equal to")),

      numericInput(inputId = "sd",
                   label = actionLink("sl", "What is the standard deviation?"),
                   1, step = 0.5),

      numericInput(inputId = "sampsize",
                   label = actionLink("ssl", "What is the sample size?"),
                   1, min = 0),

      numericInput(inputId = "sig",
                   label = actionLink("sigl", "What is the significance level?"),
                   0.05, min = 0, max = 1, step = 0.025),

      checkboxInput(inputId = "pval",
                    label = actionLink("pl", "Include a p-value in the output"),
                    value = FALSE),

      actionButton(inputId = "button",
                   label = "Submit"),
      actionButton(inputId = "help",
                   label = "Help"),


      bsModal(id = "tlm", title = "Test Help",
              trigger = "tl", size = "large",
              htmlOutput(outputId = "test_text")),
      bsModal(id = "mlm", title = "Mean Help",
              trigger = "ml", size = "large",
              htmlOutput(outputId = "mean_text")),
      bsModal(id = "nlm", title = "Null Help",
              trigger = "nl", size = "large",
              htmlOutput(outputId = "null_text")),
      bsModal(id = "alm", title = "Alternative Help",
              trigger = "al", size = "large",
              htmlOutput(outputId = "alt_text")),
      bsModal(id = "slm", title = "Standard Deviation Help",
              trigger = "sl", size = "large",
              htmlOutput(outputId = "sd_text")),
      bsModal(id = "sslm", title = "Sample Size Help",
              trigger = "ssl", size = "large",
              htmlOutput(outputId = "ss_text")),
      bsModal(id = "siglm", title = "Significance Level Help",
              trigger = "sigl", size = "large",
              htmlOutput(outputId = "sig_text")),
      bsModal(id = "plm", title = "P-value Help",
              trigger = "pl", size = "large",
              htmlOutput(outputId = "p_text")),
      bsModal(id = "hlm", title = "Help",
              trigger = "help", size = "large",
              htmlOutput(outputId = "text_help"))

    ),
    mainPanel(#h2("", align = "center"),
              plotOutput(outputId = "shade"),
              textOutput(outputId = "conc"))
  )

)

server <- function(input, output){
  tau <- eventReactive(input$button, {
                         shade_area(start = (input$mean),
                                  mu = (input$null),
                                  sd = (input$sd),
                                  n = (input$sampsize),
                                  direction = ifelse(input$alt == "Greater than", "greater",
                                                     ifelse(input$alt == "Less than", "less", "both")),
                                  test = (input$test))
                         })

  string <- eventReactive(input$button, {
                        if(input$test == "Z"){
                               calc_z_score(x = (input$mean),
                                            mu = (input$null),
                                            sigma = (input$sd),
                                            n = (input$sampsize),
                                            sig_level = (input$sig),
                                            p_val = input$pval,
                                            alternative = ifelse(input$alt == "Greater than", "greater",
                                                                 ifelse(input$alt == "Less than", "less", "both")))
                          }else{
                               calc_t_score(x = (input$mean),
                                            mu = (input$null),
                                            s = (input$sd),
                                            n = (input$sampsize),
                                            sig_level = (input$sig),
                                            p_val = input$pval,
                                            alternative = ifelse(input$alt == "Greater than", "greater",
                                                                 ifelse(input$alt == "Less than", "less", "both")))
                          }
                      })


  output$test_text <- renderUI({  HTML(paste("What kind of a test are you running?",
                                             "To provide input, please select one of the options listed.",
                                             "Choose z-test if the standard deviation is a population value.",
                                             "Choose t-test if the standard deviaton is estimated from a sample.",
                                             "Note: choose z-test if the question is about proportions.",
                                             sep = '<br/>')) })

  output$mean_text <- renderUI({  HTML(paste("What is the mean or value of interest?",
                                             "To provide input, please enter a number.",
                                             "The 'mean' in this case refers to the value being tested, so it need
                                             not be the mean of a sample.",
                                             "This may be a single 'X' value, the mean of a sample, or any other
                                             value of interest.",
                                             sep = '<br/>')) })

  output$null_text <- renderUI({  HTML(paste("What is the value of the null hypothesis?",
                                             "To provide input, please enter a number.",
                                             "This may be a population mean, a previous mean, or an expected value.",
                                             "If running a hypothesis test, this number should be exactly what mu is
                                             assumed to be in the null hypothesis statement.",
                                             sep = '<br/>')) })

  output$alt_text <- renderUI({  HTML(paste("What is the alternative hypothesis?",
                                            "To provide input, please select one of the options provided.",
                                             "This is asking for you to specify the direction of the alternative hypothesis.",
                                            "If the test is a two-sided hypothesis test, the mean above needs to be below the null
                                            hypothesis value. If the mean given is above the mean, take the mean and subtract off
                                            the null hypothesis value.  The result will be a positive number. Then, take that
                                            positive number and subtract it from the mean.  You will now have a 'corresponding'
                                            number that is below the null hypothesis value.",
                                             "Note: 'Greater than' corresponds to an interest in the probability of being
                                            'bigger than' or 'at least' as the vaule as what is observed.",
                                            "Note: 'Less than' corresponds to an interest in the probability of being 'smaller than'
                                            or 'at most' as the value as what is observed.",
                                            "Note: 'Not equal to' corresponds to an interest in the probability of being 'different'
                                            than the value that is observed.",
                                             sep = '<br/>')) })

  output$sd_text <- renderUI({  HTML(paste("What is the standard deviation of the test?",
                                           "To provide input, please enter a number greater than 0.",
                                           "This value may come frome a population or a sample.",
                                           "Note: this is the standard deviation, not the standard error.
                                           Please provide the standard deviation of the sample or the population.",
                                           "If this is a proprotion question, then you may need to calculate this value.",
                                           "If there does not seem to be a stardard deviation given, then this may be a
                                           test of proportions and you may need to calculate the standard deviation.",
                                             sep = '<br/>')) })

  output$ss_text <- renderUI({  HTML(paste("What is the sample size of the test?",
                                           "To provide input, please enter a number greater than 0.",
                                             "If no sample size is given, then it is acceptable to leave 1 as the input.",
                                             sep = '<br/>')) })

  output$sig_text <- renderUI({  HTML(paste("What is the significance level of the test?",
                                            "To provide input, please enter a number between 0 and 1.",
                                             "The significance level is commonly denoted as alpha.",
                                             "The significance level is the probability of rejecting the null hypothesis
                                            when the null hypothesis is true.",
                                            "Note: this value does not matter if a p-value is not of interest.",
                                            "Note: if a p-value is of interest, but no significance level is provided
                                            (or not of interest), then any value is fine.",
                                             sep = '<br/>')) })

  output$p_text <- renderUI({  HTML(paste("Do you want a p-value to be displayed with the output?",
                                          "To provide input, please either check the box or leave the box unchecked.",
                                             "If you are only interested in the z- or t-score, then you do not need to check the box.",
                                             "If the p-value is a quantity of interest, then do check the box.",
                                             sep = '<br/>')) })


  output$text_help <- renderUI({HTML(paste("This Shiny App is meant to help visualize and answer z-score and one
                                            sample hypothesis testing questions",
                                             "Each of the inputs can be left as is, or change to the desired numbers/options.",
                                             "The inputs will be used to create a plot (called a 'density' plot),
                                            with some area shaded.",
                                             "The shaded area will correspond to the direction of the alternative
                                            hypothesis as well as by the numbers inputed for mean, standard deviation,
                                            null hypothesis, and sample size.",
                                             "Below the plot will be some text that reiterates the values provided and
                                            provides a z- or t-score (depending on which is selected), as well as a
                                            p-value and appropriate conclusion (if the p-value box is check).",
                                             "If you are unsure of what an input value should be, you can click on the
                                            question to get some help.",
                                             "Note: for a two-sided test, the 'null hypothesis' value should be bigger than the 'mean' value.",
                                             sep = '<br/>'))})
    output$shade <- renderPlot({tau()})
  output$conc <- renderText({string()})
}

shinyApp(ui = ui, server = server)
