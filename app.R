#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# app.R
library(shiny)
library(shinydashboard)
library(tidyverse)
library(fmsb)    # for radar charts

# 1. Load data (put CSV in the same folder as app.R)
data <- read_csv("student_depression_dataset.csv")

# 2. Precompute age groups
data <- data %>%
  mutate(
    AgeGroup = case_when(
      Age < 20           ~ "Under 20",
      Age >=20 & Age <25 ~ "20–25",
      Age >=25 & Age <30 ~ "25–30",
      TRUE               ~ "Over 30"
    ),
    Depressed  = Depression == 1,
    Suicidal   = `Have you ever had suicidal thoughts ?` == "Yes",
    FamHist    = `Family History of Mental Illness` == "Yes",
    Over8Hrs   = `Work/Study Hours` > 8,
    Dissatisfied = (Study.Satisfaction <= 3 | Job.Satisfaction <= 3)
  )

ui <- dashboardPage(
  dashboardHeader(title = "Student Depression Dashboard"),
  dashboardSidebar(
    selectInput("city",   "Choose city:",   choices = c("All", unique(data$City)),   selected = "All"),
    selectInput("gender", "Choose gender:", choices = c("All", unique(data$Gender)), selected = "All")
  ),
  dashboardBody(
    # Top row: five value boxes
    fluidRow(
      valueBoxOutput("vbDep"),
      valueBoxOutput("vbSui"),
      valueBoxOutput("vbFam"),
      valueBoxOutput("vbDis"),
      valueBoxOutput("vbOver8")
    ),
    # Row of bar plots
    fluidRow(
      box(plotOutput("barCity"),   title = "By City",            width = 6),
      box(plotOutput("barAge"),    title = "By Age Group",       width = 6)
    ),
    fluidRow(
      box(plotOutput("barEdu"),    title = "By Education Level",  width = 6),
      box(plotOutput("radarEdu"),  title = "Avg Dep. by Education",width = 6)
    ),
    # Row of pie charts
    fluidRow(
      box(plotOutput("pieFam"),    title = "Family History %",    width = 6),
      box(plotOutput("pieHours"),  title = "Work-Hours %",         width = 6)
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive subset
  df <- reactive({
    d <- data
    if (input$city   != "All")    d <- filter(d, City == input$city)
    if (input$gender != "All")    d <- filter(d, Gender == input$gender)
    d
  })
  
  # Value‐boxes
  output$vbDep <- renderValueBox({
    pct <- round(mean(df()$Depressed)*100)
    valueBox(paste0(pct, "%"), "people have depression", icon = icon("sad-tear"))
  })
  output$vbSui <- renderValueBox({
    pct <- round(mean(df()$Suicidal)*100)
    valueBox(paste0(pct, "%"), "people have suicidal thoughts", icon = icon("skull"))
  })
  output$vbFam <- renderValueBox({
    depressed <- filter(df(), Depressed)
    pct <- if (nrow(depressed)>0) round(mean(depressed$FamHist)*100) else 0
    valueBox(paste0(pct, "%"), "depressed people w/ family history", icon = icon("users"))
  })
  output$vbDis <- renderValueBox({
    pct <- round(mean(df()$Dissatisfied)*100)
    valueBox(paste0(pct, "%"), "not satisfied w/ work/study", icon = icon("frown"))
  })
  output$vbOver8 <- renderValueBox({
    pct <- round(mean(df()$Over8Hrs)*100)
    valueBox(paste0(pct, "%"), "work > 8 hours", icon = icon("clock"))
  })
  
  # Bar: depression & suicidal by city
  output$barCity <- renderPlot({
    df() %>%
      count(City, Depressed, name="n") %>%
      ggplot(aes(n, City, fill = Depressed)) +
      geom_col(position="dodge") +
      labs(x = NULL, y = NULL, fill = NULL)
  })
  
  # Bar: by age group
  output$barAge <- renderPlot({
    df() %>%
      count(AgeGroup, Depressed, name="n") %>%
      ggplot(aes(n, AgeGroup, fill = Depressed)) +
      geom_col(position="dodge") +
      labs(x = NULL, y = NULL, fill = NULL)
  })
  
  # Bar: by education (Degree)
  output$barEdu <- renderPlot({
    df() %>%
      count(Degree, Depressed, name="n") %>%
      ggplot(aes(n, Degree, fill = Depressed)) +
      geom_col(position="dodge") +
      labs(x = NULL, y = NULL, fill = NULL)
  })
  
  # Radar: average depression rate by education
  output$radarEdu <- renderPlot({
    rates <- df() %>%
      group_by(Degree) %>%
      summarize(rate = mean(Depressed)*100) %>%
      pivot_wider(names_from = Degree, values_from = rate)
    
    # fmsb needs 2 extra rows for max/min
    maxmin <- tibble(across(everything(), ~c(100, 0)))
    dat    <- bind_rows(maxmin, rates)
    rownames(dat) <- c("Max","Min","Rate")
    
    fmsb::radarchart(
      dat,
      axistype = 1,
      pcol     = "darkgreen",
      pfcol    = scales::alpha("green", .4),
      plwd     = 2,
      cglcol   = "grey",
      title    = "Depression % by Degree"
    )
  })
  
  # Pie: family history among depressed
  output$pieFam <- renderPlot({
    depressed <- filter(df(), Depressed)
    tbl <- depressed %>%
      count(FamHist, name="n") %>%
      mutate(pct = n/sum(n)*100,
             label = paste0(ifelse(FamHist,"Yes","No"), ": ", round(pct), "%"))
    ggplot(tbl, aes(x="", y=pct, fill=FamHist)) +
      geom_col(width = 1) +
      coord_polar(theta="y") +
      geom_text(aes(label=label), position = position_stack(vjust=0.5)) +
      theme_void() +
      labs(fill = NULL)
  })
  
  # Pie: work-hours categories among all
  output$pieHours <- renderPlot({
    tbl <- df() %>%
      mutate(HoursCat = case_when(
        `Work/Study Hours` < 4   ~ "Less than 4",
        `Work/Study Hours` <= 8  ~ "4–8",
        TRUE                     ~ "More than 8"
      )) %>%
      count(HoursCat, name="n") %>%
      mutate(pct = n/sum(n)*100,
             label = paste0(HoursCat, ": ", round(pct), "%"))
    ggplot(tbl, aes(x="", y=pct, fill=HoursCat)) +
      geom_col(width = 1) +
      coord_polar(theta="y") +
      geom_text(aes(label=label), position = position_stack(vjust=0.5)) +
      theme_void() +
      labs(fill = NULL)
  })
}

shinyApp(ui, server)
