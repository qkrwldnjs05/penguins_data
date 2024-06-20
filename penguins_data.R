
library(shiny)
library(DT)
library(ggplot2)
library(palmerpenguins)
library(dplyr)
library(credentials)
# UI 정의

git add penguins_data
ui <- fluidPage(
  titlePanel("펭귄 데이터 분석"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("species", "펭귄 종류를 선택하세요",
                         choices = list("Adelie" = "Adelie", "Gentoo" = "Gentoo", "Chinstrap" = "Chinstrap"),
                         selected = "Adelie"),
      
      selectInput("xvar", "x축을 선택하세요.",
                  choices = names(penguins[, sapply(penguins, is.numeric)])),
      
      selectInput("yvar", "y축을 선택하세요.",
                  choices = names(penguins[, sapply(penguins, is.numeric)])),
      
      sliderInput("point_size", "점 크기를 선택하세요", min = 1, max = 10, value = 5)
    ),
    
    mainPanel(
      DTOutput("table"),
      plotOutput("scatterPlot")
    )
  )
)

# 서버 로직 정의
server <- function(input, output) {
  filteredData <- reactive({
    penguins %>%
      filter(species %in% input$species)
  })
  
  output$table <- renderDT({
    datatable(filteredData())
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(filteredData(), aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(aes(shape = sex, color = species), size = input$point_size) +
      scale_shape_manual(values = c(16, 17, NA)) + # 원형(16), 삼각형(17), NA(비어있는 상태)
      scale_color_manual(values = c("Adelie" = "red", "Gentoo" = "blue", "Chinstrap" = "green")) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "grey90", color = NA), # 패널 배경 설정
        plot.background = element_rect(fill = "grey90", color = NA), # 플롯 배경 설정
        panel.grid.major = element_line(color = "white", size = 0.8), # 주요 그리드 선 설정
        panel.grid.minor = element_line(color = "white", size = 0.4), # 보조 그리드 선 설정
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
  })
}

shinyApp(ui = ui, server = server)


library(credentials)
