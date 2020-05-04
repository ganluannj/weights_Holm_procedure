
# let's consider the case where K=2
# assume we have calculated the test statistics T1, T2
# T1 and T2 are both vectors with 2 elements 
# the first element is the test statistics of interim analysis 
# the second element is the test statistics of final analysis
# let Nmax be the max number of sample size
# let N be the real sample size needed
# let D (a vector of lenth 2)be the decision for hypothesis 1 and 2
# D[1]=0, accept hypothesis 1, D[1]=1, reject hypothesis 1
# D[2]=0, accept hypothesis 2, D[2]=1, reject hypothesis 2

# if we use GSHv (group sequential Holm variable)
# this means if we reject hypothesis 1 at interim stage,
# we will use boundaries in B instead of B2 for hypothesis 2 
# for interim analysis and for the final analysis (if necessary)
# similar for hypothesis 1 if we reject hypothesis 2 at interim stage
# if we reject hypothesis 1 at final stage, 
# then we will use boundaries in B instead of B2 for final analysis 
# for hypothesis 2
# similar for hypothesis 1 if we reject hypothesis 2 at final stage

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



# generate a function for plotting
PLOT<-function(df){
  library(ggplot2)
  p<-ggplot(df, aes(x=w1, y=probability))
  p<-p+geom_line(aes(colour=variable), size=1.2)
  p<-p+geom_point(aes(colour=variable), size=3)
  p<-p+ggtitle("Probability vs weight1")
  p<-p+theme(plot.title = element_text(hjust = 0.5),
             legend.text = element_text( size=12),
             legend.position="bottom")
  p<-p+theme(legend.title = element_blank()) 
  p<-p+theme(axis.title.x = element_text(size=14))
  p<-p+theme(axis.title.y = element_text(size=14))
  p<-p+ylim(0,1)
  p
}



# run simulations
mu1=0.4
mu2=0
mu3=0.3
mu4=0
sigma1=sigma2=sigma3=sigma4=1
alpha=0.025
# set the number of data for each group
N=100
# which boundries to use, 'OF' or 'Pocock'
Method='OF'
# how many iterations/simulations to run
Iterations=100
# the list of w1 we want to test
w1list=c(0.1,0.2, 0.9)

Test<-Combine(w1list=w1list, mu1=1, mu3=0.2, N=20, Iterations = 50)
DF<-Test[Test$variable %in% c('Stopatmid'),]

PLOT(df=DF)