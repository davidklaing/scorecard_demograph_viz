library(tidyverse)
library(magrittr)
library(shiny)
library(scales)
library(stringr)
library(shinyBS)


# Load the data.
micro <- read_csv("scorecard_micro_preprocessed.csv")

# ggName -> changes a string so it is enclosed in back-ticks.
# This can be used to make column names that have spaces (blanks)
# or non-letter characters acceptable to ggplot2.
# This version of the function is vectorized with sapply.
# Found here: http://stackoverflow.com/questions/13445435/ggplot2-aes-string-fails-to-handle-names-starting-with-numbers-or-containing-s
ggname <- function(x) {
        if (class(x) != "character") {
                return(x)
        }
        y <- sapply(x, function(s) {
                if (!grepl("^`", s)) {
                        s <- paste("`", s, sep="", collapse="")
                }
                if (!grepl("`$", s)) {
                        s <- paste(s, "`", sep="", collapse="")
                }
        }
        )
        y 
}

# Define server logic.
shinyServer(function(input, output, session) {
        
        # Write the welcome text.
        createAlert(session, "welcome1", alertId = "exampleAlert1", title = "Welcome!",
                    content = paste0("This app is for exploring the demographics of post-secondary schools in the United States. It's especially useful for (a) understanding the nationwide distribution, (b) seeing how specific schools compare to the nationwide distribution, and (c) finding outliers. <br/><br/>",
                                    "Click the button below that says 'Nationwide distribution', and scroll down for further instructions. ",
                                    "(Close this message; you can reread it by pressing the 'Help' button in the sidebar.)"
                    ),
                    style = NULL, dismiss = TRUE, append = TRUE)
        
        createAlert(session, "welcome2", alertId = "exampleAlert2", title = "",
                    content = paste0(
                            "Each of the orange bars shows you the number of schools in the United States which fall in the range shown on the x-axis. For example, the default variable is 'Share of students over 23 at entry'. The left-most orange bar shows that there are around 260 schools nationwide where 0-5% of students are over age 23 when they begin.<br/><br/>",
                            "Select a school using the drop-down menu on the left. A vertical line will be drawn through the histogram to help you see how that school compares to the nationwide distribution.<br/><br/>",
                            "Click 'Nationwide distribution' again to hide the histogram, then click 'Rankings in the selected state' to open the second plot. (Again, close this message; you can reread it by pressing the 'Help' button in the sidebar.) <br/><br/>"
                    ))
        
        createAlert(session, "welcome3", alertId = "exampleAlert3", title = "",
                    content = paste0(
                            "This plot shows you a list of schools in the selected state. They are ordered by their rank on the selected variable. For example, the default plot shows you the three schools in Alaska which have the highest shares of students over age 23 at entry. <br/><br/>",
                            "Click one of the blue bars to update the selected school. A vertical line will be drawn through the plot to help you compare the selected school to the other schools in the state. If you reopen the first plot, you'll see that the line has been updated there too.<br/><br/>",
                            "Move the slider above to control how many schools are shown in the list. <br/><br/>",
                            "Open up both plots, close this message, and play! <br/><br/>",
                            "To view these instructions again, just click the 'Help' button in the sidebar."
                    ))
        
        # Update the state.
        state <- reactive({
                input$state
        })
        
        # Update the variable.
        variable <- reactive({
                input$variable
        })
        
        # Update the school.
        school <- reactive({
                return(input$school)
        }) 
        
        # Update the options for the school based on the state.
        outvar <- reactive({
                shares <- micro[variable()]
                if (state() == "All") {
                        mm <- micro$`Institution name`[!is.na(shares)]
                } else {
                        mm <- micro$`Institution name`[micro$State == state() & !is.na(shares)]
                }
                options = c("NULL")
                append(options, unique(mm))
        })
        
        # Update the selection of the school.
        observe({
                updateSelectInput(session, "school",
                                  choices=outvar())
        })
        
        # Update the slider.
        inSlider <- reactive({
                input$inSlider
        })
        
        # Update the slider input.
        output$slider <- renderUI({
                shares <- micro[variable()]
                if (state() == "All") {
                        mm <- micro$`Institution name`[!is.na(shares)]
                } else {
                        mm <- micro$`Institution name`[micro$State == state() & !is.na(shares)]
                }
                maxslid <- min(length(unique(mm)), 100)
                sliderInput("inSlider",
                            "Select range of statewide rankings to view:",
                            min=1,
                            max=maxslid,
                            value=c(1,3),
                            width='100%',
                            round = TRUE,
                            step = 1)
        })
        
        # Update the height of the bar chart.
        PlotHeight <- reactive({
                80+50*(inSlider()[2]-inSlider()[1])
        })
        
        # Get the indices for the value of the selected school for the variable of interest.
        rowindex <- reactive({
                which(micro$`Institution name` == school())
        })
        colindex <- reactive({
                which(colnames(micro) == variable())
        })
        
        # Get the top schools.
        rankings_in_this_state <- reactive({
                
                if(is.null(my_click())) {
                        
                        if (state() == "All") {
                                m <- select_(micro, "`Institution name`", ggname(variable()), "City")
                        } else {
                                m <- filter(micro, State == state()) %>% select_("`Institution name`", ggname(variable()), "City")
                        }
                        shares <- m[variable()]
                        m <- m[!is.na(shares),]
                        m <- arrange_(m, ggname(variable()), "`Institution name`")
                        m <- select_(m, "`Institution name`", "City", ggname(variable()))
                        m <- m %>% arrange(-row_number())
                        m$Rank <- rownames(m)
                        m$alpha <- rep("1", length(m$Rank))
                        m$alpha[m$`Institution name`==school()] <- "2"
                        m <- select_(m, "Rank", "`Institution name`", ggname(variable()), "alpha")
                        m$`Institution name` <- factor(m$`Institution name`, levels = rev(m$`Institution name`))
                        if(nrow(m)>(inSlider()[2]-1)){
                                m[inSlider()[1]:inSlider()[2],]
                        } else {m}
                } else {
                        # Update the alpha values for the schools in the selected state.
                        if (state() == "All") {
                                m <- select_(micro, "`Institution name`", ggname(variable()), "City")
                        } else {
                                m <- filter(micro, State == state()) %>% select_("`Institution name`", ggname(variable()), "City")
                        }
                        shares <- m[variable()]
                        m <- m[!is.na(shares),]
                        m <- arrange_(m, ggname(variable()), "`Institution name`")
                        m <- select_(m, "`Institution name`", "City", ggname(variable()))
                        m <- m %>% arrange(-row_number())
                        m$Rank <- rownames(m)
                        keeprows <- round(my_click()$y) == inSlider()[2] + 1 - as.numeric(m$Rank)  ## Keep only the row that was clicked.
                        np <- m[keeprows, 4]  ## Get the school click input.
                        m$alpha <- rep("1", length(m$Rank))
                        m$alpha[m$Rank==as.numeric(np[[1,1]])] <- "2"
                        m <- select_(m, "Rank", "`Institution name`", ggname(variable()), "alpha")
                        m$`Institution name` <- factor(m$`Institution name`, levels = rev(m$`Institution name`))
                        if(nrow(m)>(inSlider()[2]-1)){
                                m[inSlider()[1]:inSlider()[2],]
                        } else {m}
                }
        })
        
        # Get the rank of the selected school.
        its_rank <- reactive({
                if (state() == "All") {
                        m <- select_(micro, "`Institution name`", ggname(variable()), "City")
                } else {
                        m <- filter(micro, State == state()) %>% select_("`Institution name`", ggname(variable()), "City")
                }
                shares <- m[variable()]
                m <- m[!is.na(shares),]
                m <- arrange_(m, ggname(variable()), "`Institution name`")
                m <- select(m, `Institution name`)
                m <- m %>% arrange(-row_number())
                m$Rank <- rownames(m)
                m <- select(m, Rank, `Institution name`)
                as.numeric(m$Rank[m$`Institution name` == school()])
        })
        
        # Render the barchart.
        output$barchart1 <- renderUI({
                plotOutput("barchart",
                           height = PlotHeight(),
                           width = "100%",
                           click = clickOpts("myplot_click"))
        })
        
        # Create the click variable.
        my_click <- reactive({
                input$myplot_click
        })
        
        observe({
                # Make the histogram.
                output$hist_plot <- renderPlot({
                        ggplot(micro) +
                                geom_histogram(aes_string(x = ggname(variable())),
                                               binwidth = 0.05,
                                               boundary = 0.5,
                                               alpha = 0.5,
                                               fill = "orange") +
                                geom_vline(xintercept = as.numeric(micro[rowindex()[1], colindex()[1]]),
                                           color = "blue",
                                           size = 1.5) +
                                annotate("text",
                                         x = as.numeric(micro[rowindex()[1],colindex()[1]]),
                                         y = 0,
                                         vjust = 1.3,
                                         hjust = 1,
                                         label = paste0(percent(as.numeric(micro[rowindex()[1],colindex()[1]]))),
                                         color = "blue",
                                         angle = 0,
                                         size = 4.5) +
                                labs(title = "",
                                     x = variable(),
                                     y = "Number of schools in range, nationwide") +
                                theme(axis.text = element_text(size = 12),
                                      axis.title = element_text(size = 13, face = "bold"),
                                      title = element_text(size = 13, face = "bold")) +
                                scale_x_continuous(labels=percent, limits=c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1))
                })
        })
        
        # Add text to the help page.
        output$helpPage <- renderUI({
                HTML(
                        paste0("This app is for exploring the demographics of post-secondary schools in the United States. It's especially useful for (a) understanding the nationwide distribution, (b) seeing how specific schools compare to the nationwide distribution, and (c) finding outliers. <br/><br/>",
                               "The first plot is the nationwide distribution of schools for the selected variable. Each of the orange bars shows you the number of schools in the United States which fall in the range shown on the x-axis. For example, the default variable is 'Share of students over 23 at entry'. The left-most orange bar shows that there are around 260 schools nationwide where 0-5% of students are over age 23 when they begin.<br/><br/>",
                               "Select a school using the drop-down menu on the left. A vertical line will be drawn through the histogram to help you see how that school compares to the nationwide distribution.<br/><br/>",
                               "The second plot shows you a list of schools in the selected state. They are ordered by their rank on the selected variable. For example, the default plot shows you the three schools in Alaska which have the highest shares of students over age 23 at entry. <br/><br/>",
                               "Click one of the blue bars to update the selected school. A vertical line will be drawn through both plots to help you compare the selected school to the other schools in the nation and in the state. <br/><br/>",
                               "Move the slider to control how many schools are shown in the list."
                        )
                )
        })
        
        # Remake the plots when the user clicks the barchart..
        observeEvent(my_click(), {
                
                keeprows <- round(my_click()$y) == inSlider()[2] + 1 - as.numeric(rankings_in_this_state()$Rank)  ## Keep only the row that was clicked.
                np <- rankings_in_this_state()[keeprows, 1]  ## Get the school click input.
                extra <- rankings_in_this_state()[keeprows, 3]  ## Get the school click input's intercept value.
                
                school <- reactive({
                        np[[1,1]]
                })
                
                updateSelectInput(session, "school", selected = np[[1,1]])
                
                # Update the alpha values for the schools in the selected state.
                rankings_in_this_state <- reactive({
                        if (state() == "All") {
                                m <- select_(micro, "`Institution name`", ggname(variable()), "City")
                        } else {
                                m <- filter(micro, State == state()) %>% select_("`Institution name`", ggname(variable()), "City")
                        }
                        shares <- m[variable()]
                        m <- m[!is.na(shares),]
                        m <- arrange_(m, ggname(variable()), "`Institution name`")
                        m <- select_(m, "`Institution name`", "City", ggname(variable()))
                        m <- m %>% arrange(-row_number())
                        m$Rank <- rownames(m)
                        m$alpha <- rep("1", length(m$Rank))
                        m$alpha[m$Rank==as.numeric(np[[1,1]])] <- "2"
                        m <- select_(m, "Rank", "`Institution name`", ggname(variable()), "alpha")
                        m$`Institution name` <- factor(m$`Institution name`, levels = rev(m$`Institution name`))
                        if(nrow(m)>(inSlider()[2]-1)){
                                m[inSlider()[1]:inSlider()[2],]
                        } else {m}
                })
                
                
                # Remake the barchart with the line added and the clicked school selected.
                output$barchart <- renderPlot({
                        alphas <- c("1" = "0.3", "2" = "0.6")
                        ggplot(rankings_in_this_state()) +
                                geom_bar(aes_string(x = "`Institution name`",
                                                    y = ggname(variable()),
                                                    alpha = "alpha"),
                                         stat = "identity",
                                         fill = "blue") +
                                geom_hline(yintercept = as.numeric(extra),
                                           color = "blue") +
                                coord_flip() +
                                theme(axis.text = element_text(size = 12),
                                      axis.title = element_text(size = 13, face = "bold"),
                                      title = element_text(size = 13, face = "bold")) +
                                labs(title = paste0(state()),
                                     x = "Institution name",
                                     y = variable()) +
                                scale_y_continuous(labels=percent, limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
                                geom_text(aes(x=`Institution name`,
                                              y=0, label=paste0(Rank, ": ", `Institution name`),
                                              hjust=0,
                                              size = 4.5)) +
                                geom_text(aes_string(x="`Institution name`",
                                                     y="1",
                                                     label=paste0("percent(as.numeric(",ggname(variable()),"))"),
                                                     hjust="1"),
                                          color = "blue") +
                                scale_alpha_manual(values = alphas) +
                                theme(axis.text.y=element_text(color = "white", size = 33, angle = 90),
                                      legend.position = "none")
                })
                
                # Remake the histogram with the vertical line corresponding to the clicked school.
                output$hist_plot <- renderPlot({
                        ggplot(micro) +
                                geom_histogram(aes_string(x = ggname(variable())),
                                               binwidth = 0.05,
                                               boundary = 0.5,
                                               alpha = 0.5,
                                               fill = "orange") +
                                geom_vline(xintercept = as.numeric(extra),
                                           color = "blue") +
                                annotate("text",
                                         x = as.numeric(extra),
                                         y = 0,
                                         vjust = 1.3,
                                         hjust = 1,
                                         label = paste0(percent(as.numeric(extra))),
                                         color = "blue",
                                         angle = 0,
                                         size = 4.5) +
                                labs(title = "",
                                     x = variable(),
                                     y = "Number of schools in range, nationwide") +
                                theme(axis.text = element_text(size = 12),
                                      axis.title = element_text(size = 13, face = "bold"),
                                      title = element_text(size = 13, face = "bold")) +
                                scale_x_continuous(labels=percent, limits=c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1))
                })
        })
        
        # Remake the plots when the user selects a new school using the dropdown.
        observeEvent(school(), {
                
                # Remake the barchart with the line added and the clicked school selected.
                output$barchart <- renderPlot({
                        alphas <- c("1" = "0.3", "2" = "0.6")
                        ggplot(rankings_in_this_state()) +
                                geom_bar(aes_string(x = "`Institution name`",
                                                    y = ggname(variable()),
                                                    alpha = "alpha"),
                                         stat = "identity",
                                         fill = "blue") +
                                geom_hline(yintercept = as.numeric(micro[rowindex()[1],colindex()[1]]),
                                           color = "blue") +
                                coord_flip() +
                                theme(axis.text = element_text(size = 12),
                                      axis.title = element_text(size = 13, face = "bold"),
                                      title = element_text(size = 13, face = "bold")) +
                                labs(title = paste0(state()),
                                     x = "Institution name",
                                     y = variable()) +
                                scale_y_continuous(labels=percent, limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
                                geom_text(aes(x=`Institution name`,
                                              y=0, label=paste0(Rank, ": ", `Institution name`),
                                              hjust=0,
                                              size = 4.5)) +
                                geom_text(aes_string(x="`Institution name`",
                                                     y="1",
                                                     label=paste0("percent(as.numeric(",ggname(variable()),"))"),
                                                     hjust="1"),
                                          color = "blue") +
                                scale_alpha_manual(values = alphas) +
                                theme(axis.text.y=element_text(color = "white", size = 33, angle = 90),
                                      legend.position = "none")
                })
                
                # Remake the histogram with the vertical line corresponding to the clicked school.
                output$hist_plot <- renderPlot({
                        ggplot(micro) +
                                geom_histogram(aes_string(x = ggname(variable())),
                                               binwidth = 0.05,
                                               boundary = 0.5,
                                               alpha = 0.5,
                                               fill = "orange") +
                                geom_vline(xintercept = as.numeric(micro[rowindex()[1],colindex()[1]]),
                                           color = "blue") +
                                annotate("text",
                                         x = as.numeric(micro[rowindex()[1],colindex()[1]]),
                                         y = 0,
                                         vjust = 1.3,
                                         hjust = 1,
                                         label = paste0(percent(as.numeric(micro[rowindex()[1],colindex()[1]]))),
                                         color = "blue",
                                         angle = 0,
                                         size = 4.5) +
                                labs(title = "",
                                     x = variable(),
                                     y = "Number of schools in range, nationwide") +
                                theme(axis.text = element_text(size = 12),
                                      axis.title = element_text(size = 13, face = "bold"),
                                      title = element_text(size = 13, face = "bold")) +
                                scale_x_continuous(labels=percent, limits=c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1))
                })
        })
        
        # Remake the plots when the user changes the variable, so that the click event disappears.
        observeEvent(variable(), {
                
                # Update the alpha values for the schools in the selected state.
                rankings_in_this_state <- reactive({
                        if (state() == "All") {
                                m <- select_(micro, "`Institution name`", ggname(variable()), "City")
                        } else {
                                m <- filter(micro, State == state()) %>% select_("`Institution name`", ggname(variable()), "City")
                        }
                        shares <- m[variable()]
                        m <- m[!is.na(shares),]
                        m <- arrange_(m, ggname(variable()), "`Institution name`")
                        m <- select_(m, "`Institution name`", "City", ggname(variable()))
                        m <- m %>% arrange(-row_number())
                        m$Rank <- rownames(m)
                        m$alpha <- rep("1", length(m$Rank))
                        #m$alpha[m$Rank==as.numeric(np[[1,1]])] <- "2"
                        m <- select_(m, "Rank", "`Institution name`", ggname(variable()), "alpha")
                        m$`Institution name` <- factor(m$`Institution name`, levels = rev(m$`Institution name`))
                        if(nrow(m)>(inSlider()[2]-1)){
                                m[inSlider()[1]:inSlider()[2],]
                        } else {m}
                })
                
                # Remake the barchart without the line and without the highlighted bar.
                output$barchart <- renderPlot({
                        alphas <- c("1" = "0.3", "2" = "0.6")
                        ggplot(rankings_in_this_state()) +
                                geom_bar(aes_string(x = "`Institution name`",
                                                    y = ggname(variable()),
                                                    alpha = "alpha"),
                                         stat = "identity",
                                         fill = "blue") +
                                geom_hline(yintercept = as.numeric(micro[rowindex()[1], colindex()[1]]),
                                           color = "blue",
                                           size = 1.5) +
                                coord_flip() +
                                theme(axis.text = element_text(size = 12),
                                      axis.title = element_text(size = 13, face = "bold"),
                                      title = element_text(size = 13, face = "bold")) +
                                labs(title = paste0(state()),
                                     x = "Institution name",
                                     y = variable()) +
                                scale_y_continuous(labels=percent, limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
                                geom_text(aes(x=`Institution name`,
                                              y=0, label=paste0(Rank, ": ", `Institution name`),
                                              hjust=0,
                                              size = 4.5)) +
                                geom_text(aes_string(x="`Institution name`",
                                                     y="1",
                                                     label=paste0("percent(as.numeric(",ggname(variable()),"))"),
                                                     hjust="1"),
                                          color = "blue") +
                                scale_alpha_manual(values = alphas) +
                                theme(axis.text.y=element_text(color = "white", size = 33, angle = 90),
                                      legend.position = "none")
                })
                
                # Remake the histogram without the vertical line.
                output$hist_plot <- renderPlot({
                        ggplot(micro) +
                                geom_histogram(aes_string(x = ggname(variable())),
                                               binwidth = 0.05,
                                               boundary = 0.5,
                                               alpha = 0.5,
                                               fill = "orange") +
                                geom_vline(xintercept = as.numeric(micro[rowindex()[1], colindex()[1]]),
                                           color = "blue",
                                           size = 1.5) +
                                annotate("text",
                                         x = as.numeric(micro[rowindex()[1],colindex()[1]]),
                                         y = 0,
                                         vjust = 1.3,
                                         hjust = 1,
                                         label = paste0(percent(as.numeric(micro[rowindex()[1],colindex()[1]]))),
                                         color = "blue",
                                         angle = 0,
                                         size = 4.5) +
                                labs(title = "",
                                     x = variable(),
                                     y = "Number of schools in range, nationwide") +
                                theme(axis.text = element_text(size = 12),
                                      axis.title = element_text(size = 13, face = "bold"),
                                      title = element_text(size = 13, face = "bold")) +
                                scale_x_continuous(labels=percent, limits=c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1))
                })
        })
        
        # Remake the plots when the user changes the state, so that the click event disappears.
        observeEvent(state(), {
                
                # Update the alpha values for the schools in the selected state.
                rankings_in_this_state <- reactive({
                        if (state() == "All") {
                                m <- select_(micro, "`Institution name`", ggname(variable()), "City")
                        } else {
                                m <- filter(micro, State == state()) %>% select_("`Institution name`", ggname(variable()), "City")
                        }
                        shares <- m[variable()]
                        m <- m[!is.na(shares),]
                        m <- arrange_(m, ggname(variable()), "`Institution name`")
                        m <- select_(m, "`Institution name`", "City", ggname(variable()))
                        m <- m %>% arrange(-row_number())
                        m$Rank <- rownames(m)
                        m$alpha <- rep("1", length(m$Rank))
                        m <- select_(m, "Rank", "`Institution name`", ggname(variable()), "alpha")
                        m$`Institution name` <- factor(m$`Institution name`, levels = rev(m$`Institution name`))
                        if(nrow(m)>(inSlider()[2]-1)){
                                m[inSlider()[1]:inSlider()[2],]
                        } else {m}
                })
                
                # Remake the barchart without the line or the highlighted bar.
                output$barchart <- renderPlot({
                        alphas <- c("1" = "0.3", "2" = "0.6")
                        ggplot(rankings_in_this_state()) +
                                geom_bar(aes_string(x = "`Institution name`",
                                                    y = ggname(variable()),
                                                    alpha = "alpha"),
                                         stat = "identity",
                                         fill = "blue") +
                                geom_hline(yintercept = as.numeric(micro[rowindex()[1], colindex()[1]]),
                                           color = "blue",
                                           size = 1.5) +
                                coord_flip() +
                                theme(axis.text = element_text(size = 12),
                                      axis.title = element_text(size = 13, face = "bold"),
                                      title = element_text(size = 13, face = "bold")) +
                                labs(title = paste0(state()),
                                     x = "Institution name",
                                     y = variable()) +
                                scale_y_continuous(labels=percent, limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
                                geom_text(aes(x=`Institution name`,
                                              y=0,
                                              label=paste0(Rank, ": ", `Institution name`),
                                              hjust=0,
                                              size = 4.5)) +
                                geom_text(aes_string(x="`Institution name`",
                                                     y="1",
                                                     label=paste0("percent(as.numeric(",ggname(variable()),"))"),
                                                     hjust="1"),
                                          color = "blue") +
                                scale_alpha_manual(values = alphas) +
                                theme(axis.text.y=element_text(color = "white", size = 33, angle = 90),
                                      legend.position = "none")
                })
                
                # Remake the histogram without the vertical line.
                output$hist_plot <- renderPlot({
                        
                        ggplot(micro) +
                                geom_histogram(aes_string(x = ggname(variable())),
                                               binwidth = 0.05,
                                               boundary = 0.5,
                                               alpha = 0.5,
                                               fill = "orange") +
                                geom_vline(xintercept = as.numeric(micro[rowindex()[1], colindex()[1]]),
                                           color = "blue",
                                           size = 1.5) +
                                annotate("text",
                                         x = as.numeric(micro[rowindex()[1],colindex()[1]]),
                                         y = 0,
                                         vjust = 1.3,
                                         hjust = 1,
                                         label = paste0(percent(as.numeric(micro[rowindex()[1],colindex()[1]]))),
                                         color = "blue",
                                         angle = 0,
                                         size = 4.5) +
                                labs(title = "",
                                     x = variable(),
                                     y = "Number of schools in range, nationwide") +
                                theme(axis.text = element_text(size = 12),
                                      axis.title = element_text(size = 13, face = "bold"),
                                      title = element_text(size = 13, face = "bold")) +
                                scale_x_continuous(labels=percent, limits=c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1))
                })
        })
        
        # If the user clicks the close button, stop the app.
        observe({
                if (input$close > 0) stopApp()
        })
})
