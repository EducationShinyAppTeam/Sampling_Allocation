# Load Packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(car)
library(sortable)
library(shinyWidgets)
library(rstatix)
library(lme4)
library(dplyr)
library(ggplot2)


# Global Constants ----
SimpleCal <- function(B, N = 600, sizes, sigma, sigRatios, allocations, target){
  sizes <- c(sizes, N-sum(sizes))
  sigmas <- sigma * sigRatios
  allocations <- c(allocations, 1-sum(allocations))
  numerator <- sum(sizes^2 * sigmas^2 / allocations)
  denominator <- (N^2) * (B^2 / 4) + sum(sizes * (sigmas^2))
  n <- numerator / denominator
  nTarget <- target*n 
  return(nTarget)
}
CostbasedCal <- function(B, N = 600, sizes, sigma, sigRatios, costs, target){
  sizes <- c(sizes, N-sum(sizes))
  sigmas <- sigma * sigRatios
  numerator <- (sum(sizes * sigmas / sqrt(costs))) * (sum(sizes * sigmas * sqrt(costs)))
  denominator <- N^2 * (B^2 / 4) + sum(sizes * sigmas^2)
  n <- numerator / denominator
  nTarget <- n * (sizes[target]*sigmas[target]/sqrt(costs[target]))/sum(sizes * sigmas / sqrt(costs))
  return(nTarget)
}
NeymanCal <- function(B, N = 600, sizes, sigma, sigRatios, target){
  sizes <- c(sizes, N - sum(sizes))
  sigmas <- sigma * sigRatios
  numerator <- (sum(sizes * sigmas))^2
  denominator <- N^2 * (B^2 / 4) + sum(sizes * sigmas^2)
  n <- numerator / denominator
  nTarget <- n * (sizes[target]*sigmas[target])/sum(sizes * sigmas)
  return(nTarget)
}
BudgetCal <- function(Budget, N = 600, sizes, sigma, sigRatios, costs, target){
  sizes <- c(sizes, N - sum(sizes))
  sigmas <- sigma * sigRatios
  numerators <- sizes * sigmas / sqrt(costs)
  denominator <- sum(sizes * sigmas / sqrt(costs))
  allocations <- numerators/ denominator
  n <- Budget / sum(costs * allocations)
  nTarget <- n * allocations[target]
  return(nTarget)
}
BudgetCal2 <- function(Budget, N = 600, sizes, sigma, sigRatios, costs, target){
  sizes <- c(sizes, N - sum(sizes))
  sigmas <- sigma * sigRatios
  numerators <- sizes * sigmas / sqrt(costs)
  denominator <- sum(sizes * sigmas / sqrt(costs))
  allocations <- numerators/ denominator
  n <- Budget / sum(costs * allocations)
  return(n)
}


# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "red",
    ### Create the app header
    dashboardHeader(
      title = "Allocation of the Sample",
      titleWidth = 250,
      tags$li(
        class = "dropdown",
        actionLink("info",
                   icon("info"))),
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
               href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Assumptions_of_ANOVA"
        )
      ),
      tags$li(
        class = "dropdown",
        tags$a(href='https://shinyapps.science.psu.edu/',
               icon("home")))
    ),
    ### Create the sidebar/left navigation menu
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ### Create the content
    dashboardBody(
      tabItems(
        #### Set up the Overview Page
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Allocation of the Sample"),
          p("This app introduces the concept of different type of allocation"),
          h2("Instructions"),
          tags$ol(
            tags$li("Click the go button to enter the prerequisites page."),
            tags$li("In the explore section, view and compare the concept of each 
                    allocation type")
          ),
          ##### Go Button
          div(style = "text-align: center;",
              bsButton(
                inputId = "go1",
                label = "GO!",
                icon = icon("bolt"),
                size = "large")),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p("This version of the app was developed and coded by Phichchaya Sutaporn",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 06/13/2022 by Phichchaya Sutaporn.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          h2("Prerequisites"),
          box(
            title = strong("What is allocation?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Recall that in the stratified random sampling when sample size n is chosen, there are many
            ways to divide n into the individual stratum sample sizes, n1, n2,..., nL. 
            Allocation is",
            strong("a procedure for dividing a sample among the strata.")
          ),
          box(
            title = strong("What is the goal of using allocation method?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "The main objective of allocation method is to",
            strong("obtain the speicific amount of information at the minimum cost")
          ),
          box(
            title = strong("What are the factors that affect the allocation scheme?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "There are 3 factors affecting allocation scheme:",
            tags$ol(
              tags$li(strong("Number of elements"),
                      tags$br(),
                      "For example, a sample size 20 from a population of 200 elements should contain more information than a sample of 20 from 20,000 elements.",
                      tags$br(),
                      "Therefore, large sample sizes should
          be assigned to strata containing large numbers of elements."),
          tags$li(strong("Variability of observations within 
          each stratum"),
          tags$br(),
          "The more variability there is, the larger n we 
          need to estimate parameters, with given precision."),
          tags$li(strong("Cost of obtaining an observation
          from each stratum"),
          tags$br(),
          "The higher cost, the smaller ni will be for 
          a given stratum to minimize cost.")
            )
          ),
          box(
            title = strong("Summary Table"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            tags$table(
              rules = "all",
              border = "1pt",
              tags$caption(
                "Type of Allocation Summary table",
                style = "text-align: center;"
              ),
              tags$thead(
                tags$tr(
                  tags$th("Type", style = "text-align: center;"),
                  tags$th("Assumption",style = "text-align: center;"),
                  tags$th("Result",style = "text-align: center;"),
                  align = "center"
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$th("Simple", scope = "row", style = "text-align: center;"),
                  tags$td("\\(a_{i}\\) are known"),
                  tags$td("Same \\(n_{i}\\) for each stratum"),
                  align = "center"
                ),
                tags$tr(
                  tags$th("Cost-Based", scope = "row", style = "text-align: center;"),
                  tags$td("\\(c_{i}\\) are known"),
                  tags$td("Different \\(n_{i}\\) for each stratum at the minimum
                          cost"),
                  align = "center"
                ),
                tags$tr(
                  tags$th("Neyman", scope = "row", style = "text-align: center;" ),
                  tags$td("\\(c_{i}\\) are", strong("equal or unknown")),
                  tags$td("Different \\(n_{i}\\) for each stratum"),
                  align = "center"
                ),
                tags$tr(
                  tags$th("Budget", scope = "row", style = "text-align: center;"),
                  tags$td("This is a Cost-based allocation",
                          strong("given a limit budget")),
                  tags$td("Different \\(n_{i}\\) for each stratum at the
                          minimum cost", strong("within limit budget")),
                  align = "center"
                )
              )
            ),
            br(),
            p("Where"),
            tags$ul(
              style = "list-style: none;",
              tags$li("\\(a_{i}\\) denotes the proportion of observation
                      allocated to each stratum"),
              tags$li("\\(c_{i}\\) denotes the cost of obtaining a single
                      observation from the \\(i\\)th stratum")
            )
          )
        ),
        ### Set up an Explore Page ----
        tabItem(
          tabName = "explore",
          h2("Explore"),
          br(),
          p("On this page, you will explore how each type of allocation method works.
            Please follow the steps below by using the sliders to adjust the parameters. 
            Then observe the plots of sample size when using a different type of allocation.
            As you adjust the sliders, think about how each one affects the sample size"),
          p(),
          br(),
          h3("Step 1: Set Initial Values"),
          p("There are a total of  \\(N\\) = 600 sampling units in the population
            and there are \\(L\\) = 3 strata.
            Fix stratum 3's number of sampling unit = 200 and stratum 3's 
            standard deviation = 5"),
          fluidRow(
            tags$form(
              class = "form-inline",
              column(
                width = 3,
                offset = 1,
                sliderInput(
                  inputId = "N1",
                  label = "Number of sampling unit in stratum 1, \\(N_{1}\\)",
                  min = 100,
                  max = 300,
                  value = 200,
                  step = 1
                )
              ),
              column(
                width = 3,
                offset = 1,
                sliderInput(
                  inputId = "N2",
                  label = "Number of sampling unit in stratum 2, \\(N_{2}\\)",
                  min = 100,
                  max = 300,
                  value = 200,
                  step = 1
                )
              ),
              column(
                width = 3,
                offset = 1,
               p("Number of sampling unit in stratum 3,"),
               uiOutput("N3Summary"),
              )
                )),
          fluidRow(
            tags$form(
              class = "form-inline",
            column(
              width = 3,
              offset = 1,
              sliderInput(
                inputId = "r1",
                label = "Ratio of strata 1's standard deviation to strata 3's, \\(r_{1}\\)",
                min = 0.5,
                max = 2,
                value = 1,
                step = 0.1
              )
            ),
            column(
              width = 3,
              offset = 1,
              sliderInput(
                inputId = "r2",
                label = "Ratio of strata 2's standard deviation to strata 3's, \\(r_{2}\\)",
                min = 0.5,
                max = 2,
                value = 1,
                step = 0.1
              )
            ),
            column(
              width = 3,
              offset = 1,
              p("Strata 3's standard deviation "),
              p("standard deviation = 5")
            )
          )
          ),
          br(),
          uiOutput("test"),
          uiOutput("initSummary"),
          br(),
          h3("Step 2: Pick a Allocation Type to Explore"),
          ## Inset Tabs ------------------------------------------------------------
          tabsetPanel(
            id = "models",
            type = "tabs",
            ##### Simple case Tab ----------------------------------------------
            tabPanel(
              title = "Simple Allocation",
              br(),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    h3("Step 3: Add Factors"),
                    sliderInput(
                      inputId = "a1",
                      label = "Fraction of observations allocated to stratum 1,
                      \\(a_{1}\\) ",
                      min = 0.1,
                      max = 0.8,
                      step = 0.01,
                      value = 0.33
                    ),
                    sliderInput(
                      inputId = "a2",
                      label = "Fraction of observations allocated to stratum 2,
                      \\(a_{2}\\) ",
                      min = 0.1,
                      max = 0.8,
                      step = 0.01,
                      value = 0.33
                    )
                  )
                ),
                column(
                  width = 8,
                  offset = 0,
                  plotOutput("SimplePlot")
                )
                ),
              h2("Challenge:"),
              p("What happen when bounded error approaches to 0?")
              ),
            ##### Cost-based Tab -------------------------------
            tabPanel(
              title = "Cost-based Allocation",
              br(),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    h3("Step 3: Add Factors"),
                    sliderInput(
                      inputId = "c1",
                      label = "Cost of obtaining a single observation from the 
                      first stratum, \\(c_{1}\\) ",
                      min = 10,
                      max = 50,
                      step = 1,
                      value = 20
                    ),
                    sliderInput(
                      inputId = "c2",
                      label = "Cost of obtaining a single observation from the 
                      second stratum, \\(c_{2}\\) ",
                      min = 10,
                      max = 50,
                      step = 1,
                      value = 20
                    ),
                    sliderInput(
                      inputId = "c3",
                      label = "Cost of obtaining a single observation from the 
                      thrid stratum, \\(c_{3}\\)",
                      min = 10,
                      max = 50,
                      step = 1,
                      value = 20
                    )
                  )
                ),
                column(
                  width = 8,
                  offset = 0,
                  plotOutput("CostbasedPlot")
                )
              )
            ),
            ##### Neyman Tab ---------------------------------
            tabPanel(
              title = "Neyman Allocation",
              br(),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    h3("Step 3: Add Factors"),
                    p("Neyman allocation is the cost-based allocation when
                      cost of obtaining a single observation from each stratum are 
                      equal or unknow. Therefore, there is no addition factor to add
                      in this step.") 
                )
                ),
                column(
                  width = 8,
                  offset = 0,
                  plotOutput("NeymanPlot")
                )
              )
            ),
            ##### Budget Tab -------------------------------
            tabPanel(
              title = "Budget Allocation",
              br(),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    h3("Step 3: Add Factors"),
                    sliderInput(
                      inputId = "budgetc1",
                      label = "Cost of obtaining a single observation from the 
                      first stratum, \\(c_{1}\\)",
                      min = 10,
                      max = 50,
                      step = 1,
                      value = 20
                    ),
                    sliderInput(
                      inputId = "budgetc2",
                      label = "Cost of obtaining a single observation from the 
                      second stratum, \\(c_{2}\\) ",
                      min = 10,
                      max = 50,
                      step = 1,
                      value = 20
                    ),
                    sliderInput(
                      inputId = "budgetc3",
                      label = "Cost of obtaining a single observation from the 
                      thrid stratum, \\(c_{3}\\) ",
                      min = 10,
                      max = 50,
                      step = 1,
                      value = 20
                    ),
                    # sliderInput(
                    #   inputId = "budgetRange",
                    #   label = "Min and Max Total Budget",
                    #   min = 500,
                    #   max = 1500,
                    #   value = c(600, 800)
                    # ),
                    sliderInput(
                      inputId = "targetBudget",
                      label = "Target Budget",
                      min = 500,
                      max = 1500,
                      value = 800
                    )
                  )
                ),
                column(
                  width = 8,
                  offset = 0,
                  plotOutput("BudgetPlot")
                ),
                br(),
                uiOutput("BoundederrorSummary")
                # column(
                #   width = 8,
                #   offset = 0,
                #   plotOutput("BudgetPlot2")
                # )
              )
            )
          )
        ),
      
        #### Set up the References Page-REQUIRED
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.6.1). [R package]. Available from:
            https://CRAN.R-project.org/package=shinyBS"), 
          p(class = "hangingindent",
            "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0).
            [R Package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"),
          p(class = "hangingindent",
            "Chang, W. and Borges, R. B. (2018). shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from:
            https://CRAN.R-project.org/package=shinydashboard"),
          p(class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019). shiny: Web application framework for R. (v1.4.0)
            [R Package]. Available from: https://CRAN.R-project.org/package=shiny"),
          p(class = "hangingindent",
            "de Vries, A., Schloerke, B. and Russell, K. (2019).
            sortable: Drag-and-Drop in 'shiny' Apps with 'SortableJS'. (v0.4.2) [R package]
            Avaliable from: https://CRAN.R-project.org/package=sortable"),
          p(class = "hangingindent",
            "Kassambara, A. (2020). rstatix: Pipe-Friendly Framework for Basic
            Statistical Tests. (v0.6.0) [R package] Avaliable from:
            https://CRAN.R-project.org/package=rstatix"),
          p(class = "hangingindent",
            "Perrier, V., Meyer, F. and Granjon, D. (2020). shinyWidgets:
            Custom Inputs Widgets for Shiny. (v0.5.3) [R package]
            Avaliable from: https://CRAN.R-project.org/package=shinyWidgets"),
          p(class = "hangingindent",
            "Scheaffer, R. L., Mendenhall, W., & Ott, L. (1990). Elementary survey sampling. Boston: PWS-Kent."),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define the server ----
  server <- function(input, output, session) {
  ###Button----
  observeEvent(
    eventExpr = input$go1,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore"
      )
    }
  )

  
  ### Error message----
  observeEvent(
    eventExpr = c(input$N1,input$N2) ,
    handlerExpr = {
      if(input$N1+input$N2>500 ){
        sendSweetAlert(
          session = session,
          type = "error",
          title = "Error: Please check your inputs for N1 and N2" ,
          text = tags$div(
            p("N1 + N2 must not exceed 500" )
          ),
          html = TRUE 
        )
      }
    }
  )
  observeEvent(
    eventExpr = c(input$a1,input$a2) ,
    handlerExpr = {
      if(input$a1+input$a2>0.99 ){
        sendSweetAlert(
          session = session,
          type = "error",
          title = "Error: Please check your inputs for a1 and a2" ,
          text = tags$div(
            p("a1 + a2 must not exceed 0.99" )
          ),
          html = TRUE 
        )
      }
    }
  )
# Create the summary sentence ----------------
  
  observeEvent(
    eventExpr = c(input$N1, input$N2),
    handlerExpr = {
      output$N3Summary <- renderUI({
        paste0(" \\(N_{3}\\" =  600-input$N1-input$N2)
      })
    }
  )
  
    observeEvent(
    eventExpr = c(input$N1, input$N2, input$r1, input$r2),
    handlerExpr = {
      output$initSummary <- renderUI({
        paste0(" We are starting with total population size of 600. ",
               " We have the number of sampling unit in stratum 1 = ", input$N1,
               " , the number of sampling unit in stratum 2 = ", input$N2,
               " and the the number of sampling unit in stratum 3 = ", 
               600-input$N1-input$N2,".",
               " The third stratum's standard deviation is 5. ", 
               " The first and second stratum's standard deviations are ",
               5*input$r1," and ", 5*input$r2, " respectively.")
      })
    }
  )
    
  observeEvent(
      eventExpr = c(input$N1, input$N2, input$r1, input$r2, input$budgetc1, 
                    input$budgetc2,input$budgetc3, input$targetBudget),
      handlerExpr = {
        output$BoundederrorSummary <- renderUI({
          paste0("When the costs of obtaining a single observation from the 
                  stratum equals to ", input$budgetc1,", " ,input$budgetc2,", and ",
                 input$budgetc3, ", the total budget of ", input$targetBudget, 
                 " will give the total sample size
                 of ... at ... error bound" )
        })
      }
    )
    
  ### Plots----
  output$SimplePlot <- renderPlot(
    expr = {
      ggplot(
        data = data.frame(
          B = seq(from = 0, to = 5, by = 0.5)
        ),
        mapping = aes(x = B)
      ) +
        stat_function(
          fun = SimpleCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = 5,
            sigRatios = c(input$r1, input$r2, 1),
            allocations = c(input$a1, input$a2),
            target = input$a1
          ),
          # xlim = c(1,5),
          size = 1.2,
          mapping = aes(color = "group1", linetype = "group1")
        )  +
        stat_function(
          fun = SimpleCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = 5,
            sigRatios = c(input$r1, input$r2, 1),
            allocation = c(input$a1,input$a2),
            target = input$a2
          ),
          # xlim = c(1,5),
          size = 1.2,
          mapping = aes(color = "group2", linetype = "group2")
        ) +
        stat_function(
          fun = SimpleCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = 5,
            sigRatios = c(input$r1, input$r2, 1),
            allocations = c(input$a1,input$a2),
            target = 1-input$a1-input$a2
          ),
          # xlim = c(1,5),
          size = 1.2,
          mapping = aes(color = "group3", linetype = "group3")
        )+
        scale_x_continuous(
          limits = c(0, 5),
          expand = c(0, 0)
          ) +
        scale_y_continuous(
          limits = c(0, 300),
          expand = c(0, 0)) +
        labs(
          color = "strata",
          linetype = "strata"
        )+
        ggtitle("Simple Allocation") +
        xlab("Bounded error") +
        ylab("Sample size") +
        theme_bw()+
        theme(
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 20, hjust = 0.5)
        )
    },
    alt = "A plot of a set of sample size using simple allocation method"
  )
  
  
  output$CostbasedPlot <- renderPlot(
    expr = {
      ggplot(
        data = data.frame(
          B = seq(from = 0, to = 5, by = 0.5)
        ),
        mapping = aes(x = B)
      ) +
        stat_function(
          fun = CostbasedCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = 5,
            sigRatios = c(input$r1, input$r2, 1),
            costs = c(input$c1, input$c2, input$c3),
            target = 1
          ),
          size = 1.2,
          mapping = aes(color = "group1", linetype = "group1")
        )  +
        stat_function(
          fun = CostbasedCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = 5,
            sigRatios = c(input$r1, input$r2, 1),
            costs = c(input$c1, input$c2, input$c3),
            target = 2
          ),
          size = 1.2,
          mapping = aes(color = "group2", linetype = "group2")
        )  +
        stat_function(
          fun = CostbasedCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = 5,
            sigRatios = c(input$r1, input$r2, 1),
            costs = c(input$c1, input$c2, input$c3),
            target = 3
          ),
          size = 1.2,
          mapping = aes(color = "group3", linetype = "group3")
        ) +
        scale_x_continuous(
          limits = c(0, 5),
          expand = c(0, 0)
        ) +
        scale_y_continuous(
          limits = c(0, NA),
          expand = c(0, 0)
        ) +
        labs(
          color = "strata",
          linetype = "strata"
        )+
        ggtitle("Cost-based Allocation") +
        xlab("Bounded error") +
        ylab("Sample size") +
        theme_bw()+
        theme(
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 20, hjust = 0.5)
        )
    },
    alt = "A plot of a set of sample size using cost-based allocation method"
  )
  
  output$NeymanPlot <- renderPlot(
    expr = {
      ggplot(
        data = data.frame(
          B = seq(from = 0, to = 5, by = 0.5)
        ),
        mapping = aes(x = B)
      ) +
        stat_function(
          fun = NeymanCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = 5,
            sigRatios = c(input$r1, input$r2, 1),
            target = 1
          ),
          size = 1.2,
          mapping = aes(color = "group1", linetype = "group1")
        )  +
        stat_function(
          fun = NeymanCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = 5,
            sigRatios = c(input$r1, input$r2, 1),
            target = 2
          ),
          size = 1.2,
          mapping = aes(color = "group2", linetype = "group2")
        )  +
        stat_function(
          fun = NeymanCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = 5,
            sigRatios = c(input$r1, input$r2, 1),
            target = 3
          ),
          size = 1.2,
          mapping = aes(color = "group3", linetype = "group3")
        ) +
        scale_x_continuous(
          limits = c(0, 5),
          expand = c(0, 0)
        ) +
        scale_y_continuous(
          limits = c(0, NA),
          expand = c(0, 0)
        ) +
        labs(
          color = "strata",
          linetype = "strata"
        )+
        ggtitle("Neyman Allocation") +
        xlab("Bounded error") +
        ylab("Sample size") +
        theme_bw()+
        theme(
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 20, hjust = 0.5)
        )
    },
    alt = "A plot of a set of sample size using Neyman allocation method"
  )
  output$BudgetPlot <- renderPlot(
    expr = {
      ggplot(
        data = data.frame(
          x = seq(from = 500, to = input$targetBudget, by = 100)
        ),
        mapping = aes(x = x)
      )+
        stat_function(
          fun = BudgetCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = 5,
            sigRatios = c(input$r1, input$r2, 1),
            costs = c(input$budgetc1, input$budgetc2, input$budgetc3),
            target = 1
          ),
          size = 1.2,
          mapping = aes(color = "group1", linetype = "group1")
        )  +
        stat_function(
          fun = BudgetCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = 5,
            sigRatios = c(input$r1, input$r2, 1),
            costs = c(input$budgetc1, input$budgetc2, input$budgetc3),
            target = 2
          ),
          size = 1.2,
          mapping = aes(color = "group2", linetype = "group2")
        )  +
        stat_function(
          fun = BudgetCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = 5,
            sigRatios = c(input$r1, input$r2, 1),
            costs = c(input$budgetc1, input$budgetc2, input$budgetc3),
            target = 3
          ),
          size = 1.2,
          mapping = aes(color = "group3", linetype = "group3")
        ) +
        abline(v = input$targetBudget)+
        scale_x_continuous(
          limits = c(500, 1500),
          expand = expansion(mult = 0, add = 100)
        ) +
        scale_y_continuous(
          limits = c(0, 125),
          expand = c(0, 0)) +
        labs(
          color = "strata",
          linetype = "strata"
        )+
        ggtitle("Budget Allocation") +
        xlab("Budget") +
        ylab("Sample size") +
        theme_bw()+
        theme(
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 20, hjust = 0.5)
        )
    },
    alt = "A plot of a set of sample size using budget allocation method"
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
