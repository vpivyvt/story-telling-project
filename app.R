library(shiny)
library(shinydashboard)
library(tidyverse)
library(janitor)
library(fmsb)

custom_pal <- c("#C9E6F0", "#78B3CE", "#F96E2A", "#27548A", "#FDE7BB")

data <- read_csv("student_depression_dataset 2.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    age_group    = case_when(
      age < 20             ~ "Under 20",
      age >= 20 & age < 25 ~ "20–25",
      age >= 25 & age < 30 ~ "25–30",
      TRUE                 ~ "Over 30"
    ),
    depressed    = depression == 1,
    suicidal     = have_you_ever_had_suicidal_thoughts == "Yes",
    fam_hist     = family_history_of_mental_illness == "Yes",
    over_8hrs    = work_study_hours > 8,
    dissatisfied = study_satisfaction <= 3 | job_satisfaction <= 3
  )

ui <- dashboardPage(
  dashboardHeader(title = "Student Depression"),
  dashboardSidebar(
    selectInput("city", "Choose city:", choices = c("All", unique(data$city)), selected = "All"),
    selectInput("gender", "Choose gender:", choices = c("All", unique(data$gender)), selected = "All")
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
      .box .box-header .box-title { font-weight: bold; }
    "))),
    uiOutput("cards"),
    fluidRow(
      box(title = "By City", plotOutput("barCity"), width = 4),
      box(title = "By Age Group", plotOutput("barAge"), width = 4),
      box(title = "By Education Level", plotOutput("barEdu"), width = 4)
    ),
    fluidRow(
      box(title = "Avg Dep. by Degree", plotOutput("radarEdu"), width = 4),
      box(title = "Family History %", plotOutput("pieFam"), width = 4),
      box(title = "Work-Hours %", plotOutput("pieHours"), width = 4)
    )
  )
)

server <- function(input, output, session) {
  df <- reactive({
    d <- data
    if (input$city   != "All") d <- filter(d, city   == input$city)
    if (input$gender != "All") d <- filter(d, gender == input$gender)
    d
  })
  
  output$cards <- renderUI({
    d <- df()
    stats <- list(
      list(pct = round(mean(d$depressed)*100),    txt = "people has depression"),
      list(pct = round(mean(d$suicidal)*100),     txt = "people has suicidal thought"),
      list(pct = round(mean(d$fam_hist[d$depressed])*100), txt = "people with family member has mental problem"),
      list(pct = round(mean(d$dissatisfied)*100), txt = "dont feel satisfied in work/study"),
      list(pct = round(mean(d$over_8hrs)*100),    txt = "work more than 8 hours")
    )
    fluidRow(
      lapply(stats, function(s) {
        column(2, div(class = "stat-card", h3(paste0(s$pct, "%")), p(s$txt)))
      })
    )
  })
  
  output$barCity <- renderPlot({
    df() %>%
      count(city, depressed, name = "n") %>%
      ggplot(aes(n, city, fill = as.factor(depressed))) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = custom_pal) +
      theme_minimal() + labs(x = NULL, y = NULL, fill = NULL)
  })
  
  output$barAge <- renderPlot({
    df() %>%
      count(age_group, depressed, name = "n") %>%
      ggplot(aes(n, age_group, fill = as.factor(depressed))) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = custom_pal) +
      theme_minimal() + labs(x = NULL, y = NULL, fill = NULL)
  })
  
  output$barEdu <- renderPlot({
    df() %>%
      count(degree, depressed, name = "n") %>%
      ggplot(aes(n, degree, fill = as.factor(depressed))) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = custom_pal) +
      theme_minimal() + labs(x = NULL, y = NULL, fill = NULL)
  })
  
  output$radarEdu <- renderPlot({
    rates_tbl <- df() %>%
      group_by(degree) %>%
      summarise(rate = mean(depressed)*100, .groups = "drop") %>%
      pivot_wider(names_from = degree, values_from = rate)
    rates_df <- as.data.frame(rates_tbl)
    vars <- names(rates_df)
    max_row <- setNames(as.list(rep(100, length(vars))), vars)
    min_row <- setNames(as.list(rep(0,   length(vars))), vars)
    dat <- rbind(max_row, min_row, rates_df)
    rownames(dat) <- c("Max", "Min", "Rate")
    fmsb::radarchart(dat,
                     axistype = 1,
                     pcol     = "#213448",
                     pfcol    = scales::alpha("#213448", 0.4),
                     plwd     = 2,
                     cglcol   = "#27548A",
                     title    = "Depression % by Degree"
    )
  })
  
  output$pieFam <- renderPlot({
    dep <- filter(df(), depressed)
    tbl <- dep %>% count(fam_hist, name = "n") %>%
      mutate(pct = n/sum(n)*100, label = paste0(ifelse(fam_hist,"Yes","No"),": ",round(pct),"%"))
    ggplot(tbl, aes(x="", y=pct, fill=as.factor(fam_hist))) +
      geom_col(width = 1) + coord_polar(theta="y") +
      geom_text(aes(label=label), position=position_stack(vjust=0.5)) +
      scale_fill_manual(values=custom_pal[c(3,1)]) + theme_void()
  })
  
  output$pieHours <- renderPlot({
    tbl <- df() %>% mutate(hours_cat = case_when(
      work_study_hours < 4   ~ "Less than 4",
      work_study_hours <= 8  ~ "4–8",
      TRUE                   ~ "More than 8"
    )) %>% count(hours_cat, name="n") %>%
      mutate(pct = n/sum(n)*100, label = paste0(hours_cat,": ",round(pct),"%"))
    ggplot(tbl, aes(x="", y=pct, fill=hours_cat)) +
      geom_col(width = 1) + coord_polar(theta="y") +
      geom_text(aes(label=label), position=position_stack(vjust=0.5)) +
      scale_fill_manual(values=custom_pal) + theme_void()
  })
}

shinyApp(ui, server)
