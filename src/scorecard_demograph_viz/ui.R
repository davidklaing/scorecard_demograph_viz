library(tidyverse)
library(shiny)
library(stringr)
library(shinydashboard)
library(shinyBS)

# Load the data.
micro <- read_csv("scorecard_micro_preprocessed.csv")

# Define the variables.
vars <- c("Share of students over 23 at entry",
          "Share of female students",
          "Share of married students",
          "Share of dependent students",
          "Share of veteran students",
          "Share of first generation students",
          "Share of students who received a federal loan while in school",
          "Share of students who received a Pell Grant while in school")

# Define UI for dashboard.
dashboardPage(
        skin = "yellow",
        dashboardHeader(title="US College Demographics",
                        titleWidth = 300),
        dashboardSidebar(
                width = 300,
                selectInput('state', 
                            label = 'Select state',
                            append(list("All"), unique(micro$State)),
                            selected = "Alaska"),
                bsTooltip("state", "The blue barchart will show the school rankings in the selected state.",
                          "right", options = list(container = "body")),
                selectInput("variable",
                            label = 'Select variable:',
                            choices = vars),
                bsTooltip("variable", "This allows you to compare school rankings according to a variable of your choice.",
                          "right", options = list(container = "body")),
                selectInput("school",
                            label = 'Select school:',
                            choices = "",
                            selectize = FALSE,
                            selected = ''),
                bsTooltip("school", "This adds a vertical line through both plots, corresponding to the value of the selected variable for the school of your choice.",
                          "right", options = list(container = "body")),
                textAreaInput("clickinfo",
                              NULL,
                              "Feel free to take notes here:\n- \n- \n-",
                              width = '100%',
                              height = 200),
                actionButton("help_button", "Help!", icon = icon("question-circle")),
                tags$button(
                        id = 'close',
                        type = "button",
                        class = "btn action-button",
                        onclick = "window.close()",
                        "Close"
                )
        ),
        
        dashboardBody(
                fluidRow(
                        bsModal("modal", "Help", "help_button", size = "large",
                                htmlOutput("helpPage")),
                        bsAlert("welcome1"),
                        bsCollapse(id = "collapseExample", multiple = TRUE,
                                   bsCollapsePanel("Nationwide distribution",
                                                   plotOutput("hist_plot"),
                                                   bsTooltip("hist_plot", "This is the nationwide distribution for the variable you selected.",
                                                             placement = "bottom",
                                                             options = list(container = "body")),
                                                   style = "primary")
                                   ),
                        bsAlert("welcome2"),
                        bsCollapse(id = "collapseExample", multiple = TRUE,
                                   bsCollapsePanel("Rankings in the selected state",
                                                   uiOutput("barchart1"),
                                                   bsTooltip("barchart1", "Click one of the blue bars to select that school.",
                                                             "top", options = list(container = "body")),
                                                   uiOutput("slider"),
                                                   bsTooltip("slider", "Control the number of schools that appear in the barchart above.",
                                                             "top", options = list(container = "body")),
                                                   style = "primary")
                                   ),
                        bsAlert("welcome3")
                )
        )
)