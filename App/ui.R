shinyUI(fluidPage(
  
  # enter the title
  column(8,offset = 3, 
         titlePanel("Weight allocation for Group sequential Holm procedure")),
  hr(),
  br(),
  # titlePanel("Weight allocation for weighted Holm procedure"),
  uiOutput('Hypothesis'),
  fluidRow(offset=200,
    column(6, offset=4, tags$h3("Enter your parameters here")),
    # tags$h4("Enter your parameters here"),
           br(),
    column(3, offset = 0,
           numericInput("mu1Input",HTML("&mu;<sub>1</sup>") , value = 0.25),
           numericInput("mu3Input",HTML("&mu;<sub>3</sup>") , value = 0.25),
           numericInput('NInput', 'Sample size', value = 100, min = 2)
    ),
    column(3, offset = 0,
           numericInput("sigma1Input",HTML("&sigma;<sub>1</sup>") , value = 1),
           numericInput("sigma3Input",HTML("&sigma;<sub>3</sup>") , value = 1),
           numericInput('alphaInput','Type I error', value=0.025, min=0, max=1)
    ),
    column(3, offset = 0,
           numericInput("mu2Input",HTML("&mu;<sub>2</sup>") , value = 0),
           numericInput("mu4Input",HTML("&mu;<sub>4</sup>") , value = 0),
           selectInput('MethodInput', 'Boundary', c('OF', 'Pocock'), selected = 'OF')
    ),
    
    column(3,offset = 0,
           numericInput("sigma2Input",HTML("&sigma;<sub>2</sup>") , value = 1),
           numericInput("sigma4Input",HTML("&sigma;<sub>4</sup>") , value = 1),
           textInput('w1listInput', 'Enter weight 1 (comma delimited)', value="0.1,0.2,0.3,0.4,0.5,0.6, 0.7, 0.8,0.9")
    )
  ),
  
  # actionButton("RunInput", "Run Simulation", width='50%'),
  column(5, offset=3, 
         actionButton("RunInput", "Run Simulation", icon("paper-plane"), width = '100%',
                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
  hr(),
  br(),
  # provide the option for users to select which terms to plot
  column(12, tags$h4(uiOutput("Plotterms"))),
  hr(),
  br(),
  column(10,offset=3, plotOutput("PLOTout", width = "60%")),
  fluidRow(
    column(3,offset=4, uiOutput("plotdownload")),
    column(3,uiOutput('Datadown') )
  )
  # uiOutput("plotdownload"),
  # uiOutput('Datadown')
  
)
)