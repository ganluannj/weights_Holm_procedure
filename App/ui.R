shinyUI(fluidPage(
  # enter the title
  titlePanel("Weight allocation for weighted Holm procedure"),
  
  fluidRow(
    tags$h4("Enter your parameters here"),
           br(),
    column(2, offset = 0,
           numericInput("mu1Input",HTML("&mu;<sub>1</sup>") , value = 0.25),
           numericInput("mu3Input",HTML("&mu;<sub>3</sup>") , value = 0.25),
           numericInput('NInput', 'Sample size', value = 100, min = 2)
    ),
    column(2, offset = 0,
           numericInput("sigma1Input",HTML("&sigma;<sub>1</sup>") , value = 1),
           numericInput("sigma3Input",HTML("&sigma;<sub>3</sup>") , value = 1),
           numericInput('alphaInput','Type I error', value=0.025, min=0, max=1)
    ),
    column(2, offset = 0,
           numericInput("mu2Input",HTML("&mu;<sub>2</sup>") , value = 0),
           numericInput("mu4Input",HTML("&mu;<sub>4</sup>") , value = 0),
           selectInput('MethodInput', 'Boundary', c('OF', 'Pocock'), selected = 'OF')
    ),
    
    column(2,offset = 0,
           numericInput("sigma2Input",HTML("&sigma;<sub>2</sup>") , value = 1),
           numericInput("sigma4Input",HTML("&sigma;<sub>4</sup>") , value = 1),
           numericInput("IterationsInput", 'Simulations number', value=10, min=5)
    )
  ),
  
  textInput('w1listInput', 'Enter weight 1 (comma delimited)', value="0.3,0.5, 0.7",
            width = '50%'),
  actionButton("RunInput", "Run Simulation", width='50%'),
  br(),
  # provide the option for users to select which terms to plot
  uiOutput("Plotterms"),
  plotOutput("PLOTout")
)
)