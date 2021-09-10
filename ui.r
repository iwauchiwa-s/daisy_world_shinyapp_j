library(shiny)
library(shinyWidgets)

# Define UI
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("デイジーワールド シミュレーター"),
  
  # Sidebar with sliders
  sidebarPanel(
    
    tags$h3("パラメータの設定"),
            
    selectInput("ssys", "太陽定数のシナリオ", 
        choices = list("固定" = 1, "直線的変化" = 2,
        "周期的変化" = 3), selected = 1),
        
  	conditionalPanel(condition = "input.ssys == 1",
  		setSliderColor(c("DeepPink "), c(1)),
    	sliderInput("Scfc", "太陽定数の倍率:", 
                min=0.5, max=2, value=1, step=0.1)
  	),

  	conditionalPanel(condition = "input.ssys == 2",
  		setSliderColor(c("DeepPink ", "DeepPink ", "DeepPink "), c(1, 2, 3)),
    	sliderInput("Scf1", "太陽定数の倍率の初期値:", 
                min=0.5, max=2, value=0.6, step=0.1),
    	sliderInput("Scstp", "太陽定数の倍率の変化率 (1000年あたり):", 
                min=-0.015, max=0.1, value=0.005, step=0.005)
  	),

  	conditionalPanel(condition = "input.ssys == 3",
  		setSliderColor(c("DeepPink ", "DeepPink ", "DeepPink ", "DeepPink ", "DeepPink "), c(1, 2, 3, 4, 5)),
    	sliderInput("Scf_amp", "太陽定数の倍率の振幅:", 
                min=0, max=0.4, value=0.3, step=0.05),
    	sliderInput("Scf_frq", "周期 (×1000年):", 
                min=10, max=100, value=45, step=1)
  	),

    sliderInput("Ra", "ヒナギクのない土地の反射率:", 
                min=0, max=1, value=0.5, step=0.05),
    
    sliderInput("Rw", "白ヒナギクの反射率:", 
                min=0, max=1, value=0.75, step=0.05),
     
    sliderInput("Rb", "黒ヒナギクの反射率:", 
                min=0, max=1, value=0.25, step=0.05),
    
    
    
        
    br(),

    tags$br(),
    h5("Created by:"),
    h5("xxxxxxx"),
    h5("xxxxxxx")
   ),
  
  # Show plots
  mainPanel(
    h3("ガイア仮説を理解するためのウェブアプリ"),
    plotOutput("graph0"),
    plotOutput("graph1"),
    plotOutput("graph2"),
    plotOutput("graph3"),
    img(src = "daisy_world_img_j.png")
  )
))
