# app.R

library(shiny)
library(shinydashboard)
library(tidyverse)
library(janitor)
library(fmsb)

custom_pal <- c("#C9E6F0", "#78B3CE", "#FFA55D", "#71C0BB", "#DDEB9D")

data <- read_csv("student_depression_dataset.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    age_group = case_when(
      age < 20             ~ "Under 20",
      age >= 20 & age < 25 ~ "20–25",
      age >= 25 & age < 30 ~ "25–30",
      TRUE                 ~ "Over 30"
    ),
    depressed    = depression == 1,
    suicidal     = have_you_ever_had_suicidal_thoughts == "Yes",
    fam_hist     = family_history_of_mental_illness == "Yes",
    over_8hrs    = work_study_hours > 8,
    dissatisfied = study_satisfaction <= 3 | job_satisfaction <= 3,
    financial_stress = as.numeric(financial_stress)
  )

ui <- dashboardPage(
  dashboardHeader(title = "Student Depression Dashboard"),
  dashboardSidebar(
    selectInput("city",         "Choose city:",         choices = c("All", unique(data$city)),   selected = "All"),
    selectInput("gender",       "Choose gender:",       choices = c("All", unique(data$gender)), selected = "All"),
    selectInput("degreeFilter", "Choose degree:",       choices = c("All", unique(data$degree)), selected = "All")
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .stat-card {
        background-color: #C5E1ED;
        border: 1px solid #213448;
        border-radius: 4px;
        height: 140px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        margin-bottom: 20px;
      }
      .stat-card h3 {
        margin: 0;
        color: #213448;
        font-weight: bold;
        font-size: 2em;
      }
      .stat-card p {
        margin: 0;
        color: #213448;
        font-size: 1em;
        text-align: center;
      }
      .box .box-header .box-title { 
        font-weight: bold; 
        font-size: 16px; 
      }
    "))),
    uiOutput("cards"),
    
    fluidRow(
      box(title = "Depression Rate by Age Group",          plotOutput("barAgeRate"),        width = 4),
      box(title = "Avg Dep. by Degree (Radar Chart)",      plotOutput("radarEdu"),          width = 4),
      box(title = "Depression Rate by Suicidal Thoughts",  plotOutput("barSuicidalRate"),   width = 4)
    ),
    fluidRow(
      box(title = "Suicidal Thoughts among Depressed Individuals", plotOutput("pieDepressedSuicidal"), width = 4),
      box(title = "Depression Rate by Family History",               plotOutput("barFamHistRate"),      width = 4),
      box(title = "Depression Rate by Daily Work/Study Hours",       plotOutput("barWorkHoursRate"),    width = 4)
    ),
    fluidRow(
      box(title = "Depression Rate vs Financial Stress Level",           plotOutput("lineFinStress"), width = 6),
      box(title = "Academic Pressure Distribution by Depression Status", plotOutput("boxAcadPress"),  width = 6)
    )
  )
)

server <- function(input, output, session) {
  df <- reactive({
    d <- data
    if (input$city   != "All")       d <- filter(d, city   == input$city)
    if (input$gender != "All")       d <- filter(d, gender == input$gender)
    if (input$degreeFilter != "All") d <- filter(d, degree == input$degreeFilter)
    d
  })
  
  output$cards <- renderUI({
    d <- df()
    stats <- list(
      list(pct = round(mean(d$depressed)*100),              txt = "Overall Depression Rate"),
      list(pct = round(mean(d$suicidal)*100),               txt = "Overall Suicidal Thought Rate"),
      list(pct = round(mean(d$fam_hist & d$depressed)*100), txt = "Depressed with Family History"),
      list(pct = round(mean(d$dissatisfied)*100),           txt = "Dissatisfied in Work/Study"),
      list(pct = round(mean(d$over_8hrs)*100),              txt = "Work/Study > 8 Hours")
    )
    fluidRow(
      lapply(stats, function(s) {
        column(2, 
               div(class = "stat-card", 
                   h3(paste0(s$pct, "%")), 
                   p(s$txt)))
      })
    )
  })
  
  output$barAgeRate <- renderPlot({
    df() %>%
      group_by(age_group) %>%
      summarise(rate = mean(depressed)*100, .groups = "drop") %>%
      ggplot(aes(x = age_group, y = rate, fill = age_group)) +
      geom_col(color = "black") +
      scale_fill_manual(values = custom_pal) +
      theme_minimal() +
      labs(x = "Age Group", y = "Depression Rate (%)", fill = "Age Group") +
      geom_text(aes(label = paste0(round(rate), "%")), vjust = -0.5, fontface = "bold") +
      theme(
        axis.title   = element_text(size = 14, face = "bold"),
        axis.text    = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text  = element_text(size = 12, face = "bold"),
        plot.title   = element_blank()
      )
  })
  
  output$radarEdu <- renderPlot({
    rates_tbl <- df() %>%
      group_by(degree) %>%
      summarise(rate = mean(depressed)*100, .groups = "drop") %>%
      pivot_wider(names_from = degree, values_from = rate)
    rates_df <- as.data.frame(rates_tbl)
    
    vars <- names(rates_df)
    if (length(vars) < 3) {
      plot.new()
      text(0.5, 0.5, "Not enough degree levels to plot", cex = 1.2, font = 2)
      return()
    }
    max_row <- setNames(as.list(rep(100, length(vars))), vars)
    min_row <- setNames(as.list(rep(0,   length(vars))), vars)
    dat <- rbind(max_row, min_row, rates_df)
    rownames(dat) <- c("Max", "Min", "Rate")
    
    fmsb::radarchart(
      dat,
      axistype = 1,
      pcol     = "#213448",
      pfcol    = scales::alpha("#213448", 0.4),
      plwd     = 2,
      cglcol   = "#71C0BB",
      title    = ""
    )
  })
  
  output$barSuicidalRate <- renderPlot({
    df() %>%
      group_by(suicidal) %>%
      summarise(rate = mean(depressed)*100, .groups = "drop") %>%
      mutate(label = ifelse(suicidal, "Had Suicidal Thoughts", "No Suicidal Thoughts")) %>%
      ggplot(aes(x = label, y = rate, fill = label)) +
      geom_col(color = "black") +
      scale_fill_manual(values = c("#FFA55D", "#78B3CE")) +
      theme_minimal() +
      labs(x = "", y = "Depression Rate (%)", fill = "") +
      geom_text(aes(label = paste0(round(rate), "%")), vjust = -0.5, fontface = "bold") +
      theme(
        axis.title     = element_text(size = 14, face = "bold"),
        axis.text      = element_text(size = 12, face = "bold"),
        legend.position = "none",
        plot.title     = element_blank()
      )
  })
  
  output$pieDepressedSuicidal <- renderPlot({
    dep <- filter(df(), depressed)
    tbl <- dep %>%
      count(suicidal, name = "n") %>%
      mutate(
        pct   = n / sum(n) * 100,
        label = ifelse(suicidal,
                       paste0("Suicidal: ", round(pct), "%"),
                       paste0("Not Suicidal: ", round(pct), "%"))
      )
    ggplot(tbl, aes(x = "", y = pct, fill = label)) +
      geom_col(width = 1, color = "black") +
      coord_polar(theta = "y") +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5),
                size = 4, fontface = "bold") +
      scale_fill_manual(values = c("#71C0BB", "#DDEB9D")) +
      theme_void() +
      theme(
        legend.title = element_text(size = 14, face = "bold"),
        legend.text  = element_text(size = 12, face = "bold"),
        plot.title   = element_blank()
      )
  })
  
  output$barFamHistRate <- renderPlot({
    df() %>%
      group_by(fam_hist) %>%
      summarise(rate = mean(depressed)*100, .groups = "drop") %>%
      mutate(label = ifelse(fam_hist, "Family History: Yes", "Family History: No")) %>%
      ggplot(aes(x = label, y = rate, fill = label)) +
      geom_col(color = "black") +
      scale_fill_manual(values = c("#DDEB9D", "#71C0BB")) +
      theme_minimal() +
      labs(x = "", y = "Depression Rate (%)", fill = "") +
      geom_text(aes(label = paste0(round(rate), "%")), vjust = -0.5, fontface = "bold") +
      theme(
        axis.title     = element_text(size = 14, face = "bold"),
        axis.text      = element_text(size = 12, face = "bold"),
        legend.position = "none",
        plot.title     = element_blank()
      )
  })
  
  output$barWorkHoursRate <- renderPlot({
    df() %>%
      mutate(hours_cat = case_when(
        work_study_hours < 4   ~ "Less than 4 hours",
        work_study_hours <= 8  ~ "Between 4 and 8 hours",
        TRUE                   ~ "More than 8 hours"
      )) %>%
      group_by(hours_cat) %>%
      summarise(rate = mean(depressed)*100, .groups = "drop") %>%
      ggplot(aes(x = hours_cat, y = rate, fill = hours_cat)) +
      geom_col(color = "black") +
      scale_fill_manual(values = custom_pal) +
      theme_minimal() +
      labs(x = "Work/Study Hours per Day", y = "Depression Rate (%)", fill = "Category") +
      geom_text(aes(label = paste0(round(rate), "%")), vjust = -0.5, fontface = "bold") +
      theme(
        axis.title     = element_text(size = 14, face = "bold"),
        axis.text      = element_text(size = 12, face = "bold"),
        legend.title   = element_text(size = 14, face = "bold"),
        legend.text    = element_text(size = 12, face = "bold"),
        plot.title     = element_blank()
      )
  })
  
  output$lineFinStress <- renderPlot({
    df() %>%
      filter(!is.na(financial_stress)) %>%
      group_by(financial_stress) %>%
      summarise(dep_rate = mean(depressed)*100, .groups = "drop") %>%
      ggplot(aes(x = as.factor(financial_stress), y = dep_rate, group = 1)) +
      geom_line(color = "#71C0BB", size = 1) +
      geom_point(color = "#71C0BB", size = 3) +
      theme_minimal() +
      labs(x = "Financial Stress Level (1–5)", y = "Depression Rate (%)") +
      geom_text(aes(label = paste0(round(dep_rate), "%")), vjust = -1, fontface = "bold") +
      theme(
        axis.title     = element_text(size = 14, face = "bold"),
        axis.text      = element_text(size = 12, face = "bold"),
        legend.position = "none",
        plot.title     = element_blank()
      )
  })
  
  output$boxAcadPress <- renderPlot({
    df() %>%
      ggplot(aes(x = as.factor(depressed), y = academic_pressure, fill = as.factor(depressed))) +
      geom_boxplot() +
      scale_x_discrete(labels = c("0" = "No Depression", "1" = "Depression")) +
      scale_fill_manual(values = c("#78B3CE", "#FFA55D")) +
      theme_minimal() +
      labs(x = "Depression Status", y = "Academic Pressure (0–5)", fill = "") +
      theme(
        axis.title     = element_text(size = 14, face = "bold"),
        axis.text      = element_text(size = 12, face = "bold"),
        legend.position = "none",
        plot.title     = element_blank()
      )
  })
}

shinyApp(ui, server)
