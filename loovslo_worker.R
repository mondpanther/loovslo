# routines to maintain the loovslo roulette
library(compiler)


# Draw dataset ####
drawdata=function(FIXFLAG=c("K","L")){
  library(dplyr)
  

  zf=data.frame(g=0:n_g) %>% mutate(z=rnorm(n())) # draw geographic z shocks
  
  #### initialise data frame
  df=data.frame(id=1:firms) %>% mutate(
                                            nu=signu*rnorm(n()),
                                            xi=sigxi*rnorm(n()),
                                        epschi=sigepschi*rnorm(n()),
                                          epsx=sigepsx*rnorm(n()),
                                          
                                         g=runif(n()), 
                                         l=runif(n())) %>%
     mutate(g=as.integer(g*n_g)) %>% inner_join(zf) %>% group_by(g) %>% arrange(g,l) %>% 
    
     mutate(                            
           nn=1:n(),
           chi=epschi+nu,
           chibar=chi,
           #chi=purrr::accumulate(chi, ~ (((.x*(nn-1)) * phichibar +.y)/nn)),
           #chi=purrr::accumulate(chi, ~ .x* phichibar * .nn  +.y),
           #chi=ifelse(nn>1, ((lag(chi)) * (nn-1) * phichibar + epschi)/nn, chi),
           
           #chi=purrr::accumulate2(chi,1:(n()-1), ~ ..1 * phichibar/..3 + ..2),
           
           
           x=epsx+nu+phiz*z,              # initialise x
           y=betax*x+betachi*chi+xi+nu,   # initialise y
           ybar=y,
           #test=1,
           #test=purrr::accumulate2(test,1:(n()-1), ~ ..1 *1/..3 + ..2)#+test
           #test=ifelse(nn>1,(accumulate(test,~.x+y)-test)*0.5+test,test)
             
     )

     
 
  #<<<< recursive looop <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  #dfr=df
  maxx=max(df$nn)
  for(i in 1:maxx){
    df=df %>% mutate(ybar=cummean(y), chibar=cummean(chi)) %>% 
                mutate(chi=ifelse(nn>1, lag(chibar) * phichibar+epschi+nu, chi),
                         y=ifelse(nn>1, betax*x+betachi*chi+lag(ybar)   * phiybar+xi+nu, y) )
  
  }  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  #### add LOO instruments
  df=df %>% mutate(chiLOO=mean(chi)-chi/n(),
                   xLOO=mean(x)-x/n(),
                   L1ybar=lag(ybar),
                   L1chibar=lag(chibar))
  
  
  lm(y~x+chi,df) %>% summary()
  

  return(df)
  
}



########### Estimation #########################################################

esti=function(rlabel,df){
  library(fixest)
  library(purrr)
  
  
  df=df %>% filter(nn>1)

  #### ols  
  reg1=feols(y~x+chi, df, warn=F,note=F)
  #reg1=lm(LHS~z1+z2+z3+z4+z5+z6+z7+factor(t),df)
  
  betax_ols  =reg1[["coefficients"]][["x"]]
  betachi_ols=reg1[["coefficients"]][["chi"]]

  
  #### loo
  reg1=feols(y~1 | x+chi~xLOO+chiLOO, df, warn=F,note=F)
  #reg1=lm(LHS~z1+z2+z3+z4+z5+z6+z7+factor(t),df)
  
  betax_LOO  =reg1[["coefficients"]][["fit_x"]]
  betachi_LOO=reg1[["coefficients"]][["fit_chi"]]
  
  
  #### lo
  reg1=feols(y~1 | x+chi~xLOO+L1chibar, df, warn=F,note=F)
  #reg1=lm(LHS~z1+z2+z3+z4+z5+z6+z7+factor(t),df)
  
  betax_LO=reg1[["coefficients"]][["fit_x"]]
  betachi_LO=reg1[["coefficients"]][["fit_chi"]]
  
  
  #### lox
  reg1=feols(y~L1ybar | x+chi~xLOO+L1chibar, df, warn=F,note=F)
  #reg1=lm(LHS~z1+z2+z3+z4+z5+z6+z7+factor(t),df)
  
  betax_LOX=reg1[["coefficients"]][["fit_x"]]
  betachi_LOX=reg1[["coefficients"]][["fit_chi"]]
  
  
  

  esti=data.frame(rlabel=rlabel,
                  betax_ols=betax_ols,
                  betachi_ols=betachi_ols,
                  
                  betax_LOO=betax_LOO,
                  betachi_LOO=betachi_LOO,
                  
                  betax_LO=betax_LO,
                  betachi_LO=betachi_LO,
                  
                  betax_LOX=betax_LOX,
                  betachi_LOX=betachi_LOX)
                  
  
}


drawdata=cmpfun(drawdata)
esti=cmpfun(esti)


# Run montecarlo ####
monte=function(params){
  print("Hello in monet") #%>% withS
  
  #message(paste('ok made it this far with x='))
  # recover parameters
  for(pp in names(params)){
    expr=paste0(pp,"=",params[[pp]])
    eval(parse(text=expr))
    #print(expr)
  }


  
#xxx=function(){  
  library(foreach)
  library(doParallel)
  
  cores=detectCores()
  #cl <- makeCluster(cores[1]-2) #not to overload your computer
  cl <- makeCluster(as.integer(cores/2)) #not to overload your computer
  
  combiner=function(final,temp){
    library(dplyr)
    final=bind_rows(final,temp)
    return(final)
  }
  
  registerDoParallel(cl)
  
  
  #repli=10
  rr_df <- foreach(cou=1:repli, 
                   .combine=combiner, 
                   .export=c(names(params),"drawdata","esti"),
                   #.export=c("philam","phia","alphL","alphM","periods","firms","gam"),
                   .packages=c("stringr","fixest","dplyr","purrr"),
                   .inorder=F) %dopar% {
                     cdf=drawdata()  # get current data frame
                     esti(cou,cdf)
                     #print(summary(cdf))
                     
                   }
  #stop cluster
  stopCluster(cl)
  
  
  
  summary=rr_df %>% summary() 
  

  
  return(rr_df)
  
  
}

