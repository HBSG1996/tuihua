library(shiny)

ui <- fluidPage(
  titlePanel(title=h1("退化演示",align="lefet"),windowTitle="窗口"),
  sidebarLayout(
    sidebarPanel(
      h3("控制"),
      br(),
      sliderInput("ylim","磨耗",min=0,max=20,step=0.1,value=c(10)),
      sliderInput("xlim","里程",min=0,max=100,step=1,value=c(50)),
      sliderInput("vl","预测线",min=0,max=100,step=1,value=c(25)),
      selectInput("per","预测区间",choices = list("80%"=0.9,"60%"=0.8,"40%"=0.7),selected="80%"),
      selectInput("ys","预测里程",choices = c(25,30,40),selected=25)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "图片",
          p(plotOutput("tu",height = 550))
        ),
        tabPanel(
          "结果",
          verbatimTextOutput("su")
        )
      )
    )
  )
)