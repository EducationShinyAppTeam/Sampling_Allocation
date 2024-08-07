# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(shinyWidgets)
library(dplyr)
library(ggplot2)


# Global Constants and Functions ----
fixedStdDev <- 5
PropCal <- function(B,sigma, N = 600, sizes, sigRatios, allocations, target){
  sizes <- c(sizes, N - sum(sizes))
  sigmas <- sigma * sigRatios
  allocations <- c(allocations, 1 - sum(allocations))
  numerator <- sum(sizes^2 * sigmas^2 / allocations)
  denominator <- (N^2) * (B^2 / 4) + sum(sizes * (sigmas^2))
  n <- numerator / denominator
  nTarget <- target*n
  return(nTarget)
}
CostbasedCal <- function(B,sigma, N = 600, sizes, sigRatios, costs, target){
  sizes <- c(sizes, N - sum(sizes))
  sigmas <- sigma * sigRatios
  numerator <- (sum(sizes * sigmas / sqrt(costs))) * (sum(sizes * sigmas * sqrt(costs)))
  denominator <- N^2 * (B^2 / 4) + sum(sizes * sigmas^2)
  n <- numerator / denominator
  nTarget <- n * (sizes[target]*sigmas[target]/sqrt(costs[target]))/sum(sizes * sigmas / sqrt(costs))
  return(nTarget)
}
NeymanCal <- function(B,sigma, N = 600, sizes, sigRatios, target){
  sizes <- c(sizes, N - sum(sizes))
  sigmas <- sigma * sigRatios
  numerator <- (sum(sizes * sigmas))^2
  denominator <- N^2 * (B^2 / 4) + sum(sizes * sigmas^2)
  n <- numerator / denominator
  nTarget <- n * (sizes[target]*sigmas[target])/sum(sizes * sigmas)
  return(nTarget)
}
budgetCalc <- function(Budget, sigma, N = 600, sizes, sigRatios, costs, target){
  sizes <- c(sizes, N - sum(sizes))
  sigmas <- sigma * sigRatios
  numerators <- sizes * sigmas / sqrt(costs)
  denominator <- sum(sizes * sigmas / sqrt(costs))
  allocations <- numerators / denominator
  n <- Budget / sum(costs * allocations)
  nTarget <- n * allocations[target]
  return(nTarget)
}
errorBoundCalc <- function(sampleSizes, variances){
  varainces <- fixedStdDev^2
  return(sqrt(sum(variances * sampleSizes) / sum(sampleSizes)))
}

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "red",
    ## Header ----
    dashboardHeader(
      title = "Sampling Allocation",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Sampling_Allocation")
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          id = "home",
          href = 'https://shinyapps.science.psu.edu/',
          icon("house")
        )
      )
    ),
    ## Sidebar Menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Challenge", tabName = "challenge", icon = icon("cogs")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Sampling Allocation"),
          p("This app introduces different methods for the sampling allocation problem."),
          h2("Instructions"),
          tags$ol(
            tags$li("Click the Prerequisites button to enter the prerequisites page."),
            tags$li("In the Explore section, view and compare the different
                    allocation methods."),
            tags$li("The Challenge page provides ideas for exploration.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "prerequisites1",
              label = "Prerequisites",
              icon = icon("bolt"),
              size = "large")
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p("This version of the app was originally developed and coded by
            Phichchaya Sutaporn, with updates in 2024 by Xin Yun.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 07/5/2024 by Xin Yun.")
          )
        ),
        ### Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          h2("Prerequisites"),
          box(
            title = strong("What is allocation?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Often a population can be divided into \\(L\\) more homogenous
            subpopulations called", tags$em("strata."), "In stratified random
            sampling with a sample size \\(n\\), there are many ways to divide
            \\(n\\) into the individual stratum sample sizes, \\(n_{1}, n_{2},
            \\ldots,n_{L}\\). Sampling allocation methods provide a procedure for
            dividing a sample among the strata."
          ),
          box(
            title = strong("What is the goal of sampling allocation methods?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "The main objective of allocation methods is to produce the most
            precise estimate at the minimum cost."
          ),
          box(
            title = strong("What are the factors that affect the allocation scheme?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "There are 2 main factors affecting the allocation scheme.",
            tags$ol(
              tags$li(
                tags$strong("Variability of observations within each stratum."),
                tags$br(),
                "The more variability there is, the larger \\(n_{i}\\) we need
                to estimate parameters, with given precision."
              ),
              tags$li(
                strong("Cost of obtaining an observation from each stratum."),
                tags$br(),
                "The more expensive it is to get an observation, the smaller
                \\(n_{i}\\) will be for a given stratum to minimize cost."
              )
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
                  tags$th("Allocation Type/Scheme", style = "text-align: center;"),
                  tags$th("Assumption", style = "text-align: center;"),
                  tags$th("Result", style = "text-align: center;"),
                  align = "center"
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$th("Proportional", scope = "row", style = "text-align: center;"),
                  tags$td("\\(a_{i} = N_{i}/N\\) are known."),
                  tags$td("\\(n_{i} = a_{i}*n\\) for the \\(i^{th}\\) strata."),
                  align = "center"
                ),
                tags$tr(
                  tags$th("Cost-Based", scope = "row", style = "text-align: center;"),
                  tags$td("\\(c_{i}\\) are known."),
                  tags$td("Different \\(n_{i}\\) for each stratum at the minimum
                          cost."),
                  align = "center"
                ),
                tags$tr(
                  tags$th("Neyman", scope = "row", style = "text-align: center;" ),
                  tags$td("\\(c_{i}\\) are equal but within-strata variances are
                          different."),
                  tags$td("Different \\(n_{i}\\) for each stratum providing the
                          best precision."),
                  align = "center"
                ),
                tags$tr(
                  tags$th("Budget", scope = "row", style = "text-align: center;"),
                  tags$td("This is a cost-based allocation given a limited budget."),
                  tags$td("Different \\(n_{i}\\) for each stratum at the
                          minimum cost within a limited budget."),
                  align = "center"
                )
              )
            ),
            br(),
            p("Where"),
            tags$ul(
              style = "list-style: none;",
              tags$li("\\(N_{i}\\) denotes the size of strata \\(i\\)."),
              tags$li("\\(a_{i}\\) denotes the proportion of the population in each strata."),
              tags$li("\\(c_{i}\\) denotes the cost of obtaining a single
                      observation from the \\(i^{th}\\) stratum.")
            )
          )
        ),
        ### Explore Page ----
        tabItem(
          tabName = "explore",
          h2("Exploring Sampling Allocations"),
          p("On this page, you will explore how each type of allocation method works.
            Please follow the steps below by using the sliders to adjust the
            parameters. Then observe the plots of sample size when using a
            different type of allocation. As you adjust the sliders, think about
            how each one affects the sample size."),
          p(),
          br(),
          h3("Step 1: Set Initial Values"),
          p("There is a total of \\(N = 600\\) sampling units in the population
            and there are \\(L = 3\\) strata. The standard deviation for the third
            statum is fixed at 5."),
          p("Note that when you are setting the number of sampling units in the
            populations for strata 1 and 2, please allow stratum 3 to have at
            least 100 sampling units."),
          fluidRow(
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
              p("Number of sampling unit in stratum 3, \\(N_3\\)"),
              uiOutput("N3Summary"),
            )
          ),
          br(),
          fluidRow(
            column(
              width = 3,
              offset = 1,
              sliderInput(
                inputId = "r1",
                label = "Ratio of stratum 1's standard deviation to stratum 3's, \\(r_{1}\\)",
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
                label = "Ratio of stratum 2's standard deviation to stratum 3's, \\(r_{2}\\)",
                min = 0.5,
                max = 2,
                value = 1,
                step = 0.1
              )
            ),
            column(
              width = 3,
              offset = 1,
              p("Stratum 3's standard deviation "),
              p("standard deviation = 5")
            )
          ),
          br(),
          uiOutput("initSummary"),
          br(),
          h3("Step 2: Pick an Allocation Type to Explore"),
          ## Inset Tabs
          tabsetPanel(
            id = "models",
            type = "tabs",
            #### Proportional Tab ----
            tabPanel(
              title = "Proportional Allocation",
              br(),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    h3("Step 3: Additional Factors"),
                    uiOutput("a1Summary"),
                    uiOutput("a2Summary"),
                    uiOutput("a3Summary")
                  )
                ),
                column(
                  width = 8,
                  offset = 0,
                  plotOutput("PropPlot")
                )
              )
            ),
            #### Cost-based Tab ----
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
                      pre = "$",
                      min = 10,
                      max = 50,
                      step = 1,
                      value = 20
                    ),
                    sliderInput(
                      inputId = "c2",
                      label = "Cost of obtaining a single observation from the
                      second stratum, \\(c_{2}\\) ",
                      pre = "$",
                      min = 10,
                      max = 50,
                      step = 1,
                      value = 20
                    ),
                    sliderInput(
                      inputId = "c3",
                      label = "Cost of obtaining a single observation from the
                      third stratum, \\(c_{3}\\)",
                      pre = "$",
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
            #### Neyman Tab ----
            tabPanel(
              title = "Neyman Allocation",
              br(),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    h3("Step 3: Add Factors"),
                    p("Neyman allocation is the cost-based allocation when
                      costs of obtaining a single observation from each stratum are
                      equal. Therefore, there are no addition factors to add
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
            #### Budget Tab ----
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
                      pre = "$",
                      min = 10,
                      max = 50,
                      step = 1,
                      value = 20
                    ),
                    sliderInput(
                      inputId = "budgetc2",
                      label = "Cost of obtaining a single observation from the
                      second stratum, \\(c_{2}\\) ",
                      pre = "$",
                      min = 10,
                      max = 50,
                      step = 1,
                      value = 20
                    ),
                    sliderInput(
                      inputId = "budgetc3",
                      label = "Cost of obtaining a single observation from the
                      third stratum, \\(c_{3}\\) ",
                      pre = "$",
                      min = 10,
                      max = 50,
                      step = 1,
                      value = 20
                    ),
                    sliderInput(
                      inputId = "targetBudget",
                      label = "Target Budget",
                      pre = "$",
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
                uiOutput("BoundederrorSummary"),
                br(),
                p("Footnote: Rounding may cause some values to be off by 1 in some situations.")
              )
            )
          )
        ),
        ### Challenge page ----
        tabItem(
          tabName = "challenge",
          h2("Challenge Questions"),
          p("To check your understanding of sampling allocation,
            try these challenge questions. You may expand the boxes to see answers.
            Use the Explore page to help but try not to look at the answers
            until you try the questions on your own."),
          br(),
          box(
            title = tags$strong("What happens when the bounded error approaches 0?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Zero error can only happen with a census (i.e. when sample size =
            population size)."
          ),
          box(
            title = tags$strong("How does the optimal sample allocation depend
                                on the cost of obtaining an observation?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Higher costs result in a smaller allocation going to that group
            (essentially proportional to the square root of the cost)."
          ),
          box(
            title = tags$strong("If the cost of obtaining an observation from one
                                stratum is four times the cost for the other strata,
                                how much bigger should the sample size be?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Double the size assuming the standard deviations within strata
            are all the same."
          ),
          box(
            title = strong("Suppose the cost of obtaining an observation from
                           one stratum is four times the cost for the other strata.
                           Can you find the within-strata standard deviations
                           that make the optimal allocation the same for all strata?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "This type of balance would occur when the standard deviation is
            doubled compared to the other groups (because sample allocation goes
            with the variance). "
          ),
          box(
            title = strong("How does the sample size behave as a function of the
                           budget?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Sample size will be a linear function of the budget for each stratum. "
          ),
          box(
            title = strong("Suppose costs and within-group standard deviations are
                           the same for all strata. Should the sample sizes
                           always be the same?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "No – population sizes can play a role if a small error forces the
            use of a large proportion of the population be sampled. "
          )
        ),
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v0.61.1). [R package]. Available from:
            https://CRAN.R-project.org/package=shinyBS"),
          p(class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (2024). boastUtils: BOAST Utilities.
            (v0.1.12.2). [R Package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"),
          p(class = "hangingindent",
            "Chang, W. and Borges, R. B. (2021). shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.2) [R Package]. Available from:
            https://CRAN.R-project.org/package=shinydashboard"),
          p(class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie,
            Y., Allen, J., McPherson, J., Dipert, A., and Borges, B. (2024).
            shiny: Web application framework for R. (v1.8.1.1) [R Package].
            Available from: https://CRAN.R-project.org/package=shiny"),
          p(class = "hangingindent",
            "Perrier, V., Meyer, F. and Granjon, D. (2024). shinyWidgets:
            Custom Inputs Widgets for Shiny. (v0.8.6) [R package]
            Avaliable from: https://CRAN.R-project.org/package=shinyWidgets"),
          p(class = "hangingindent",
            "Scheaffer, R. L., Mendenhall, W., & Ott, L. (1990). Elementary
            Survey Sampling. Boston: PWS-Kent."
          ),
          p(class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            Springer-Verlag:New York. (v 3.5.1) [R package]. Available from
            https://ggplot2.tidyverse.org"),
          p(class = "hangingindent",
            "Wickham, H., François, R., Henry, L., Müller, K., and Vaughan, D.
            (2023). dplyr: A grammar of data manipulation. (v 1.1.4). [R package].
            Availble from https://CRAN.R-project.org/package=dplyr"
          ),
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
  ## Go Button ----
  observeEvent(
    eventExpr = input$prerequisites1,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
      )
    }
  )
  ## Info button---
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "Use this application to explore sampling allocation methods."
      )
    }
  )

  ## Sample Size Error Message ----
  observeEvent(
    eventExpr = c(input$N1, input$N2) ,
    handlerExpr = {
      if (input$N1 + input$N2 > 500 ) {
        sendSweetAlert(
          session = session,
          type = "error",
          title = "Error: Please check your inputs for N1 and N2" ,
          text = tags$div(
            p("N1 + N2 must not exceed 500")
          ),
          html = TRUE
        )
      }
    }
  )

  ## Create the Summary Information ----
  ### Step 1 Message ----
  observeEvent(
    eventExpr = c(input$N1, input$N2),
    handlerExpr = {
      output$N3Summary <- renderUI(
        expr = {
          p(withMathJax("\\(N_{3}=", (600 - input$N1 - input$N2), "\\)"))
        }
      )
    }
  )

  ### Step 1 Summary ----
  observeEvent(
    eventExpr = c(input$N1, input$N2, input$r1, input$r2),
    handlerExpr = {
      output$initSummary <- renderUI(
        expr = {
          p("We are starting with a total population size of 600 and have three
              strata. The number of sampling units in Stratum 1 is", input$N1,
            "while the number of sampling units in Stratum 2 is", input$N2,
            "and the number of sampling units in Stratum 3 is",
            600 - input$N1 - input$N2, ". The third stratum's standard deviation
              is 5. The first and second stratum's standard deviations are",
            5*input$r1, "and", 5*input$r2, ", respectively.")
        }
      )
    }
  )
  ### Proportional Messages ----
  observeEvent(
    eventExpr = c(input$N1),
    handlerExpr = {
      output$a1Summary <- renderUI(
        expr = {
          p("The fraction of the sample allocated to Stratum 1 is",
            round(input$N1/600, digits = 2))
        }
      )
    }
  )
  observeEvent(
    eventExpr = c(input$N2),
    handlerExpr = {
      output$a2Summary <- renderUI(
        expr = {
          p("The fraction of the sample allocated to Stratum 2 is",
            round(input$N2/600, digits = 2))
        }
      )
    }
  )
  observeEvent(
    eventExpr = c(input$N1, input$N2),
    handlerExpr = {
      output$a3Summary <- renderUI(
        expr = {
          p("The fraction of the sample allocated to Stratum 3 is",
            round(1 - (input$N1/600) - (input$N2/600), digits = 2))
        }
      )
    }
  )

  ### Budget Messages ----
  observeEvent(
    eventExpr = c(input$N1, input$N2, input$r1, input$r2,
                  input$budgetc1, input$budgetc2, input$budgetc3,
                  input$targetBudget),
    handlerExpr = {
      sampleSizes <- budgetCalc(
        Budget = input$targetBudget,
        N = 600,
        sizes = c(input$N1, input$N2),
        fixedStdDev,
        sigRatios = c(input$r1, input$r2, 1),
        costs = c(input$budgetc1, input$budgetc2,input$budgetc3),
        target = 1:3
      )

      totalSampleSize <- sum(sampleSizes)
      errorBound <- errorBoundCalc(
        sampleSizes = sampleSizes,
        variances = (fixedStdDev * c(input$r1, input$r2, 1))^2
      )

      output$BoundederrorSummary <- renderUI(
        expr = {
          p("The costs of obtaining a single observation are $", input$budgetc1,
            "for Stratum 1, $", input$budgetc2, "for Stratum 2, and $",
            input$budgetc3, "for Stratum 3; we have a total budget of $",
            input$targetBudget, "(as shown by the black vertical line). In this
            case, we have a total sample size of", floor(totalSampleSize),
            "where the sample size of Stratum 1 is", floor(sampleSizes[1]),
            ", the sample size of Stratum 2 is", round(sampleSizes[2]),
            "and the sample size of stratum 3 is", ceiling(sampleSizes[3]),
            "with an error bound of", round(errorBound, digits = 2), ".")
        }
      )
    }
  )

  ## Plots----
  ### Proportional ----
  output$PropPlot <- renderPlot(
    expr = {
      validate(
        need(
          input$N1 >= 100 & input$N2 >= 100 & (600 - input$N1 - input$N2) >= 100,
          message = "Please adjust number of sampling unit in strata 1 and 2.
          Note: There should be a minimum of 100 sampling units in each stratum."
        )
      )
      ggplot(
        data = data.frame(
          B = seq(from = 0, to = 5, by = 0.5)
        ),
        mapping = aes(x = B)
      ) +
        stat_function(
          fun = PropCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = fixedStdDev,
            sigRatios = c(input$r1, input$r2, 1),
            allocations = c(input$N1/600, input$N2/600),
            target = input$N1/600
          ),
          linewidth = 2,
          mapping = aes(color = "Stratum 1", linetype = "Stratum 1")
        )  +
        stat_function(
          fun = PropCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = fixedStdDev,
            sigRatios = c(input$r1, input$r2, 1),
            allocation = c(input$N1/600, input$N2/600),
            target = input$N2/600
          ),
          size = 2,
          mapping = aes(color = "Stratum 2", linetype = "Stratum 2")
        ) +
        stat_function(
          fun = PropCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = fixedStdDev,
            sigRatios = c(input$r1, input$r2, 1),
            allocations = c(input$N1/600, input$N2/600),
            target = 1 - input$N1/600 - input$N2/600
          ),
          size = 2,
          mapping = aes(color = "Stratum 3", linetype = "Stratum 3")
        ) +
        scale_x_continuous(
          limits = c(0, 5),
          expand = c(0, 0)
        ) +
        scale_y_continuous(
          limits = c(0, 300),
          expand = c(0, 0)) +
        scale_color_manual(
          name = "Strata",
          values = psuPalette[c(2, 3, 1)]
        ) +
        scale_linetype_manual(
          name = "Strata",
          values = c(1, 2, 3)
        ) +
        ggtitle("Proportion Allocation") +
        xlab("Bounded error") +
        ylab("Sample size") +
        theme_bw() +
        theme(
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.title = element_text(size = 15),
          legend.key.size = unit(1,"in"),
          legend.position = "bottom",
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 20, hjust = 0.5)
        )
    },
    alt = "A plot of a set of sample size using proportional allocation method"
  )

  ### Cost-based ----
  output$CostbasedPlot <- renderPlot(
    expr = {
      validate(
        need(
          input$N1 >= 100 & input$N2 >= 100 & (600 - input$N1 - input$N2) >= 100,
          message = "Please adjust the number of sampling units in strata 1 and 2.
          Note: There should be a minimum of 100 sampling units in each stratum"
        )
      )
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
            sigma = fixedStdDev,
            sigRatios = c(input$r1, input$r2, 1),
            costs = c(input$c1, input$c2, input$c3),
            target = 1
          ),
          linewidth = 2,
          mapping = aes(color = "Stratum 1", linetype = "Stratum 1")
        )  +
        stat_function(
          fun = CostbasedCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = fixedStdDev,
            sigRatios = c(input$r1, input$r2, 1),
            costs = c(input$c1, input$c2, input$c3),
            target = 2
          ),
          size = 2,
          mapping = aes(color = "Stratum 2", linetype = "Stratum 2")
        )  +
        stat_function(
          fun = CostbasedCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = fixedStdDev,
            sigRatios = c(input$r1, input$r2, 1),
            costs = c(input$c1, input$c2, input$c3),
            target = 3
          ),
          size = 2,
          mapping = aes(color = "Stratum 3", linetype = "Stratum 3")
        ) +
        scale_x_continuous(
          limits = c(0, 5),
          expand = c(0, 0)
        ) +
        scale_y_continuous(
          limits = c(0, NA),
          expand = c(0, 0)
        ) +
        scale_color_manual(
          name = "Strata",
          values = psuPalette[c(2, 3, 1)]
        ) +
        scale_linetype_manual(
          name = "Strata",
          values = c(1, 2, 3)
        ) +
        ggtitle("Cost-based Allocation") +
        xlab("Bounded error") +
        ylab("Sample size") +
        theme_bw() +
        theme(
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.key.size = unit(1,"in"),
          legend.position = "bottom",
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 20, hjust = 0.5)
        )
    },
    alt = "A plot of a set of sample size using cost-based allocation method"
  )

  ### Neyman -----
  output$NeymanPlot <- renderPlot(
    expr = {
      validate(
        need(
          input$N1 >= 100 & input$N2 >= 100 & (600 - input$N1 - input$N2) >= 100,
          message = "Please adjust number of sampling unit in strata 1 and 2.
          Note: There should be a minimum of 100 sampling units in each stratum"
        )
      )
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
            sigma = fixedStdDev,
            sigRatios = c(input$r1, input$r2, 1),
            target = 1
          ),
          linewidth = 2,
          mapping = aes(color = "Stratum 1", linetype = "Stratum 1")
        )  +
        stat_function(
          fun = NeymanCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = fixedStdDev,
            sigRatios = c(input$r1, input$r2, 1),
            target = 2
          ),
          size = 2,
          mapping = aes(color = "Stratum 2", linetype = "Stratum 2")
        )  +
        stat_function(
          fun = NeymanCal,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = fixedStdDev,
            sigRatios = c(input$r1, input$r2, 1),
            target = 3
          ),
          size = 2,
          mapping = aes(color = "Stratum 3", linetype = "Stratum 3")
        ) +
        scale_x_continuous(
          limits = c(0, 5),
          expand = c(0, 0)
        ) +
        scale_y_continuous(
          limits = c(0, NA),
          expand = c(0, 0)
        ) +
        scale_color_manual(
          name = "Strata",
          values = psuPalette[c(2, 3, 1)]
        ) +
        scale_linetype_manual(
          name = "Strata",
          values = c(1, 2, 3)
        ) +
        ggtitle("Neyman Allocation") +
        xlab("Bounded error") +
        ylab("Sample size") +
        theme_bw() +
        theme(
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.key.size = unit(1,"in"),
          legend.position = "bottom",
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 20, hjust = 0.5)
        )
    },
    alt = "A plot of a set of sample size using Neyman allocation method"
  )

  ### Budget ----
  output$BudgetPlot <- renderPlot(
    expr = {
      validate(
        need(
          input$N1 >= 100 & input$N2 >= 100 & (600 - input$N1 - input$N2) >= 100,
          message = "Please adjust number of sampling unit in strata 1 and 2.
          Note: There should be a minimum of 100 sampling units in each stratum"
        )
      )
      ggplot(
        data = data.frame(
          x = seq(from = 500, to = input$targetBudget, by = 100)
        ),
        mapping = aes(x = x)
      ) +
        stat_function(
          fun = budgetCalc,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = fixedStdDev,
            sigRatios = c(input$r1, input$r2, 1),
            costs = c(input$budgetc1, input$budgetc2, input$budgetc3),
            target = 1
          ),
          linewidth = 2,
          mapping = aes(color = "Stratum 1", linetype = "Stratum 1")
        )  +
        stat_function(
          fun = budgetCalc,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = fixedStdDev,
            sigRatios = c(input$r1, input$r2, 1),
            costs = c(input$budgetc1, input$budgetc2, input$budgetc3),
            target = 2
          ),
          size = 2,
          mapping = aes(color = "Stratum 2", linetype = "Stratum 2")
        )  +
        stat_function(
          fun = budgetCalc,
          args = list(
            sizes = c(input$N1, input$N2),
            sigma = fixedStdDev,
            sigRatios = c(input$r1, input$r2, 1),
            costs = c(input$budgetc1, input$budgetc2, input$budgetc3),
            target = 3
          ),
          size = 2,
          mapping = aes(color = "Stratum 3", linetype = "Stratum 3")
        ) +
        geom_vline(xintercept = input$targetBudget, size = 1) +
        scale_x_continuous(
          limits = c(500, 1500),
          expand = expansion(mult = 0, add = 100)
        ) +
        scale_y_continuous(
          limits = c(0, 125),
          expand = c(0, 0)) +
        scale_color_manual(
          name = "Strata",
          values = psuPalette[c(2, 3, 1)]
        ) +
        scale_linetype_manual(
          name = "Strata",
          values = c(1, 2, 3)
        ) +
        ggtitle("Budget Allocation") +
        xlab("Budget ($)") +
        ylab("Sample size") +
        theme_bw() +
        theme(
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.key.size = unit(1,"in"),
          legend.position = "bottom",
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
