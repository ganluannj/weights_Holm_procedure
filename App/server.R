library(shiny)
library(DT)
Decision<-function(T1, T2, B1, B2, B, Nmax=1){
  # we can reject both hypothesis at interim analysis
  if ((T1[1]>B1[1]) & (T2[1]>B2[1])){
    # this is the case that we can reject both hypothesis at interim alalysis
    D<-c(1,1)
    N<-Nmax/2
  }
  
  # we can reject hypothesis 1 at stage 1 
  # but not hypothesis 1 with B2
  if ((T1[1]>B1[1]) & (T2[1]<B2[1])){
    # then we need to use B for hypotheis 2
    if (T2[1]>B[1]){
      # here we can reject hypothesis 2 at stage 1
      D<-c(1,1)
      N<-Nmax/2
    }
    else {
      # we need to go to the final stage
      N<-Nmax
      if (T2[2]>B[2]){
        # reject hypothesis 2 at final stage
        D<-c(1,1)
      }
      else {
        # we cannot reject hypothesis 2
        D<-c(1,0)
      }
    }
  }
  
  # we can reject hypothesis 2 at stage 1 
  # but not hypothesis 1 with B1
  if ((T1[1]<B1[1]) & (T2[1]>B2[1])){
    # then we need to use B for hypotheis 1
    if (T1[1]>B[1]){
      # here we can reject hypothesis 1 at stage 1
      D<-c(1,1)
      N<-Nmax/2
    }
    else {
      # we need to go to the final stage
      N<-Nmax
      if (T1[2]>B[2]){
        # reject hypothesis 1 at final stage
        D<-c(1,1)
      }
      else {
        # we cannot reject hypothesis 2
        D<-c(0,1)
      }
    }
  }
  
  # we cannot reject either hypothesis at stage 1
  if (T1[1]<B1[1] & T2[1]<B2[1]){
    N<-Nmax
    # if we cannot reject either hypothesis with B1/B2
    if (T1[2]<B1[2] & T2[2]<B2[2]){ D<-c(0,0)}
    # if we can reject both hypothesis with B1/B2
    if (T1[2]>B1[2] & T2[2]>B2[2]){ D<-c(1,1)}
    # if we can reject hypothesis 1 with B1, but not hypothesis 2 with B2
    if (T1[2]>B1[2] & T2[2]<B2[2]){
      # we need to compare T2[2] with B[2]
      if (T2[2]>B[2]){D<-c(1,1)}
      else {D<-c(1,0)}
    }
    # if we can reject hypothesis 2 with B2, but not hypothesis 1 with B1
    if (T1[2]<B1[2] & T2[2]>B2[2]){
      # we need to compare T1[2] with B[2]
      if (T1[2]>B[2]){D<-c(1,1)}
      else {D<-c(0,1)}
    }
  }
  return(list(D, N))
}


# define a function to generate simulation data and 
# perform t test output the critical value
Critical<-function(mu1, mu2, sigma1,sigma2, N, testtype){
  # this function will generate N data from N(mu1, sigmasq), label X1
  # and another N data from N(mu2, sigmasq), label X2
  # N need to be even 
  # use half and whole of the data to perform t test
  # output the critical value for one side t test with X1>X2
  halfN=N/2
  X1<-rnorm(N, mean=mu1, sd=sigma1)
  X2<-rnorm(N, mean=mu2, sd=sigma2)
  
  Test1<-t.test(X1[1:halfN], X2[1:halfN], alternative='greater', 
                var.equal = TRUE)
  Test2<-t.test(X1, X2, alternative='greater', var.equal = TRUE)
  return(c(Test1$statistic, Test2$statistic))
}

# create a function to combine all above together
Simulation<-function(mu1, mu2=0, mu3, mu4=0, sigma1=1, sigma2=1,
                     sigma3=1, sigma4=1, N, w1=0.5,w2=0.5, alpha=0.025, Method='OF', 
                     Iterations=200){
  # data from group i follows N(mui, sigmai^2), i=1,2,3,4
  # each group has N data
  # first generate critical values for hypothesis 1 and 2
  # w1 is the weight for hypothesis 1
  # w2 is the weight for hypothesis 2
  # alpha is the type I error
  # Method ='OF' or 'Pocock','OF' for O'Brien-Fleming and 'Pocock' for Pocock
  # Iterations is how many iterations we want to run
  
  Rej_both=0
  Rej_1_only=0
  Rej_2_only=0
  No_rej=0
  Stopatmid=0
  library(gsDesign)
  for (i in 1:Iterations){
    T1=Critical(mu1=mu1, mu2=mu2, sigma1 = sigma1, sigma2=sigma2, N=N)
    T2=Critical(mu1=mu3, mu2=mu4, sigma1 = sigma3, sigma2=sigma4, N=N)
    # genrerate boundaries with weighted alpha and with whole alpha
    K=2
    B1<-gsDesign(k=K, alpha=alpha*w1,test.type=1,sfu=Method)$upper$bound
    B2<-gsDesign(k=K, alpha=alpha*w2,test.type=1,sfu=Method)$upper$bound
    B<- gsDesign(k=K, alpha=alpha,test.type=1,sfu=Method)$upper$bound
    # perform hypothesis testing
    Temp=Decision(T1=T1, T2=T2, B1=B1, B2=B2, B=B, Nmax=1)
    Rej=Temp[[1]]
    if (Rej[1]==1 & Rej[2]==1) {Rej_both =Rej_both+1}
    if (Rej[1]==1 & Rej[2]==0) {Rej_1_only = Rej_1_only+1}
    if (Rej[1]==0 & Rej[2]==1) {Rej_2_only = Rej_2_only+1}
    if (Rej[1]==0 & Rej[2]==0) {No_rej = No_rej+1}
    if (Temp[[2]]==0.5) {Stopatmid=Stopatmid +1}
  }
  # initialize a dataframe to store the result
  return(data.frame('Rej_both'=Rej_both/Iterations, 
                    'Rej_1_only'=Rej_1_only/Iterations, 
                    'Rej_2_only'=Rej_2_only/Iterations,
                    'No_rej'=No_rej/Iterations, 
                    'Stopatmid'=Stopatmid/Iterations))
}

Combine<-function(w1list,mu1, mu2=0, mu3, mu4=0, sigma1=1, sigma2=1,
                  sigma3=1, sigma4=1, N, alpha=0.025, Method='OF', 
                  Iterations=200 )
{
  LEN=length(w1list)
  # generate a dataframe to store the result
  # value in Result represent the probability
  Result<-data.frame('w1'=w1list, 'w2'=1-w1list, 'Rej_both'=rep(0, LEN), 
                     'Rej_1_only'=rep(0, LEN), 'Rej_2_only'=rep(0, LEN),
                     'No_rej'=rep(0, LEN), 'Stopatmid'=rep(0, LEN),
                     'Rej_atleast_one'=rep(0, LEN),'Rej_1'=rep(0,LEN), 
                     'Rej_2'=rep(0, LEN))
  for (j in 1:LEN){
    Result[j,3:7]<-Simulation(mu1=mu1, mu2=mu2, mu3=mu3, mu4=mu4, 
                              sigma1=sigma1, sigma2=sigma2, 
                              sigma3=sigma3, sigma4=sigma4, N=N, 
                              w1=w1list[j],w2=1-w1list[j], alpha=alpha, 
                              Method=Method, Iterations=Iterations)
  }
  # calculate the probability of reject at least one
  Result['Rej_atleast_one']=Result['Rej_both']+Result['Rej_1_only']+Result['Rej_2_only']
  Result['Rej_1']=Result['Rej_both']+Result['Rej_1_only']
  Result['Rej_2']=Result['Rej_both']+Result['Rej_2_only']
  
  # reshape Result for plotting
  library("tidyverse")
  Result<-Result %>%
    gather(key = "variable", value = "probability", -w1, -w2)
  return(Result)
}

Calculation<-function(w1list,mu1, mu2=0, mu3, mu4=0, sigma1=1, sigma2=1,
                      sigma3=1, sigma4=1, N, alpha=0.05, Method='OF')
{
  LEN=length(w1list)
  # generate a dataframe to store the result
  # value in Result represent the probability
  Result<-data.frame('w1'=w1list, 'w2'=1-w1list, 'Rej_both'=rep(0, LEN), 
                     'Rej_1_only'=rep(0, LEN), 'Rej_2_only'=rep(0, LEN),
                     'No_rej'=rep(0, LEN),
                     'Rej_atleast_one'=rep(0, LEN),'Rej_1'=rep(0,LEN), 
                     'Rej_2'=rep(0, LEN))
  
  Delta1=mu1-mu2
  Delta2=mu3-mu4
  Mean1=c(sqrt(N)*Delta1/2, sqrt(N)*Delta1/sqrt(2))
  Mean2=c(sqrt(N)*Delta2/2, sqrt(N)*Delta2/sqrt(2))
  V<-c(1, sqrt(1/2), sqrt(1/2),1)
  M<-matrix(V, nrow=2)
  B<-gsDesign(k=2, alpha=alpha,test.type=1,sfu=Method)$upper$bound
  
  for (i in 1:LEN){
    w1=w1list[i]
    w2=1-w1
    B1<-gsDesign(k=2, alpha=alpha*w1,test.type=1,sfu=Method)$upper$bound
    B2<-gsDesign(k=2, alpha=alpha*w2,test.type=1,sfu=Method)$upper$bound
    
    ###############################################################################
    ############### Reject at least one ##########################################
    ###############                     #########################################
    ###########################################################################
    
    ## probability of not rejecting hypothesis 1
    p1<-pmvnorm(lower=c(-Inf, -Inf), upper = B1, mean=Mean1, sigma=M)[[1]]
    ## probability of not rejecting hypothesis 2
    p2<-pmvnorm(lower=c(-Inf, -Inf), upper = B2, mean=Mean2, sigma=M)[[1]]
    ## probability of rejecting at least one
    patleast1<-1-p1*p2
    Result[i,'Rej_atleast_one']<-patleast1
    
    ###############################################################################
    ############### Reject only 1 (A)     ##########################################
    ###############                     #########################################
    ###########################################################################
    
    ## first the probabilty of rejecting hypothesis 1 at middle
    p1<-pmvnorm(lower=c(B1[1], -Inf), upper = c(Inf, Inf), mean=Mean1, sigma=M)[[1]]
    p2<-pmvnorm(lower=c(-Inf, -Inf), upper = B, mean=Mean2, sigma=M)[[1]]
    pmid<-p1*p2
    ## first the probabilty of rejecting hypothesis 1 at final
    p1<-pmvnorm(lower=c(-Inf, B1[2]), upper = c(B1[1], Inf), mean=Mean1, sigma=M)[[1]]
    p2<-pmvnorm(lower=c(-Inf, -Inf), upper = c(B2[1], B[2]), mean=Mean2, sigma=M)[[1]]
    pfinal<-p1*p2
    ponlyA<-pmid+pfinal
    Result[i,'Rej_1_only']<-ponlyA
    
    ###############################################################################
    ############### Reject only 2 (B)       ##########################################
    ###############                     #########################################
    ###########################################################################
    
    ## first the probabilty of rejecting hypothesis 2 at middle
    p1<-pmvnorm(lower=c(-Inf, -Inf), upper = B, mean=Mean1, sigma=M)[[1]]
    p2<-pmvnorm(lower=c(B2[1], -Inf), upper = c(Inf, Inf), mean=Mean2, sigma=M)[[1]]
    pmid<-p1*p2
    ## first the probabilty of rejecting hypothesis 2 at final
    p1<-pmvnorm(lower=c(-Inf, -Inf), upper = c(B1[1], B[2]), mean=Mean1, sigma=M)[[1]]
    p2<-pmvnorm(lower=c(-Inf, B2[2]), upper = c(B2[1], Inf), mean=Mean2, sigma=M)[[1]]
    pfinal<-p1*p2
    ponlyB<-pmid+pfinal
    Result[i,'Rej_2_only']<-ponlyB
    
    #################################
    Rejboth<-patleast1-ponlyA-ponlyB
    Rej1<-ponlyA+Rejboth
    Rej2<-ponlyB+Rejboth
    Result[i,'Rej_both']<-Rejboth
    Result[i,'Rej_1']<-Rej1
    Result[i,'Rej_2']<-Rej2
    Result[i, 'No_rej']<-1-patleast1
  }
  
  # reshape Result for plotting
  library("tidyverse")
  Result<-Result %>%
    gather(key = "variable", value = "probability", -w1, -w2)
  return(Result)
}

# generate a function for plotting
PLOT<-function(df){
  library(ggplot2)
  p<-ggplot(df, aes(x=w1, y=probability))
  p<-p+geom_line(aes(colour=variable), size=1.2)
  p<-p+geom_point(aes(colour=variable), size=3)
  p<-p+ggtitle("Probability vs weight A")
  p<-p+theme(plot.title = element_text(hjust = 0.5),
             legend.text = element_text( size=14),
             legend.position="bottom",
             axis.text=element_text(size=14),
             axis.title=element_text(size=14))
  p<-p+theme(legend.title = element_blank()) 
  p<-p+theme(axis.title.y = element_text(size=14))
  p<-p+theme(axis.title.x = element_text(size=14))
  p<-p+xlab('Weight A')
  p<-p+ylim(0,1)
  return(p)
}

# get each elements from a list
GET<-function(X){
  LEN<-length(X)
  temp<-c()
  for (i in 1:LEN){
    temp<-append(temp, X[i])
  }
  return(temp)
}


# x <- as.numeric(unlist(strsplit(input$vec1,",")))
shinyServer(function(input, output) {
  
  output$Hypothesis <- renderUI({
    withMathJax(
      helpText('Data $$\\large{X_{i} \\sim N(\\mu_i, \\sigma^2_i), i=1,2,3,4 }$$'),
      helpText('Hypothesis A $$\\large{H_0: \\mu_1=\\mu_2, 
               H_{\\alpha}: \\mu_1>\\mu_2 }$$', width='40%'),
      helpText('Hypothesis B $$\\large{H_0: \\mu_3=\\mu_4, 
               H_{\\alpha}: \\mu_3>\\mu_4 }$$')
    )
  })
  # create a variable to make sure the plot will disappear if you change the input
  # also the plot will not appear unitl you hit the run button
  Hv<-reactiveValues(doPlot = FALSE)
  
  observeEvent(input$RunInput, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    Hv$doPlot <- input$RunInput
  })
  
  observeEvent(input$MethodInput,{Hv$doPlot <- FALSE})
  observeEvent(input$w1listInput,{Hv$doPlot <- FALSE})
  observeEvent(input$sigma2Input,{Hv$doPlot <- FALSE})
  observeEvent(input$sigma4Input,{Hv$doPlot <- FALSE})
  observeEvent(input$mu1Input,{Hv$doPlot <- FALSE})
  observeEvent(input$mu3Input,{Hv$doPlot <- FALSE})
  observeEvent(input$NInput,{Hv$doPlot <- FALSE})
  observeEvent(input$sigma1Input,{Hv$doPlot <- FALSE})
  observeEvent(input$sigma3Input,{Hv$doPlot <- FALSE})
  observeEvent(input$alphaInput,{Hv$doPlot <- FALSE})
  observeEvent(input$mu2Input,{Hv$doPlot <- FALSE})
  observeEvent(input$mu4Input,{Hv$doPlot <- FALSE})
  
  # define a function to convert textinput to numberic vector
  Convert<-function(X){
    return(as.numeric(unlist(strsplit(X,","))))
  }
  W1list<-reactive({
    req(input$w1listInput)
    Convert(input$w1listInput)
  })
  
  # create a function to run simulation and return the result
  Simu<-function(){
      isolate({
        R<-Calculation(w1list=W1list(),mu1=input$mu1Input, mu2=input$mu2Input, 
                                    mu3=input$mu3Input, mu4=input$mu4Input, 
                                    sigma1=input$sigma1Input, sigma2=input$sigma2Input, 
                                    sigma3=input$sigma3Input, sigma4=input$sigma4Input,
                                    N=input$NInput, alpha=input$alphaInput, Method=input$MethodInput)
        })
  }
  
  #get the data
  DF<-reactive({
    if (!(Hv$doPlot==FALSE)){
      Simu()
    }
  })
  
  # provide the option to select the term to show
  output$Plotterms<-renderUI({
    req(DF())
    if (!(Hv$doPlot==FALSE)){
      checkboxGroupInput('PlottermsInput', 'Select the term(s) to plot',
                         choiceNames = c("Reject both hypotheses",
                                        "Reject at least one hypothesis",
                                        "Reject hypothesis A",
                                        "Reject hypothesis B",
                                        "No Rejection"),
                         choiceValues =c('Rej_both', 'Rej_atleast_one',"Rej_1",
                                        "Rej_2","No_rej"),
                         selected = 'Rej_both', inline = TRUE)
    }
  })

  # start to generate the plot
  # get the part of data to plot
  
  Var<-reactive({
    req(input$PlottermsInput)
    GET(input$PlottermsInput)
  })

  Pdata<-reactive({
    req(DF())
    DF()[DF()$variable %in% Var(),]
  })
  
  P<-function(){
    isolate({return(PLOT(df=Pdata()))})
    }
  
  output$PLOTout<-renderPlot({
    req(Pdata())
    PLOT(df=Pdata())
  })
  
  # download option for plot
  output$plotdownload<-renderUI({
    req(Pdata())
    downloadButton('PREplotdownload', "Download the plot")
  })
  
  output$PREplotdownload <- downloadHandler(
    filename <- paste0("plot",".jpeg"),
    content <- function(file) {
      req(Pdata())
      ggsave(file, plot=P(), width=4, height=6, units='in')
    })
  
  # download option for data
  output$Datadown<-renderUI({
    req(Pdata())
    downloadButton('Datadownload', "Download the data")
  })
  
  output$Datadownload<-downloadHandler(
    filename = function(){paste("Data", ".csv")},
    content =function(file){
      write.csv(DF(), file, row.names = FALSE)
    })
  
})
