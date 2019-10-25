#' @title nlsregression
#' 
#' @description Creates regression parameter estimates and plots with any function you want
#' that has no more than two independent variables
#' 
#' 
#' @param func function that shall be fitted. Function should contain the
#' dependent variable y and and the independent variable x, eventually a second
#' independent variable z. All other unknowns are treated as parameters that
#' are estimated.
#' @param y dependent variable,vector
#' @param x independent variable,vector
#' @param z optional independent variable,vector
#' @param startvalues the optimization algorithm may require starting values
#' for the fitting procedure. provide them in a list with the parameter names:
#' e.g. list(a=3,b=2)
#' @param weight optional weight,vector
#' @param weighting if weighting is TRUE, the fit will minimize the weighted residuals
#' @param xlab name of x axis in plot
#' @param ylab name of y axis in plot
#' @param header plot function main argument
#' @param z_plot_lines vector>1 of values for z you want to be plotted into the
#' graph
#' @param weightcolorpoints if TRUE, the points are clustered into three
#' quantiles according to their weight and coloured lighter for low weights.
#' @param x_log10 allows log10 scale for X axis if set to TRUE. Only changes the picture, not
#' the regression!
#' @param plot_x_function depreciated, please do not enter into function call. 
#' @param toPlot "all", "frame" (axis etc), "observations" (points), "regressionline" (line), "infos" (parameters, R2)
#' @param regressioncolor color of regression line and paramter text
#' @param weight_threshold if numeric, all countries below this threshold will be excluded (e.g. to exclude minor islands)
#' @param ... will be passed on to function nls
#' @return A nice picture and regression parameters or eventually some errors.
#' @author Benjamin Leon Bodirsky, Susanne Rolinski, Xiaoxi Wang
#' @examples
#' \dontrun{
#' x=1:10
#' y=(1:10)^2+1
#' z=c(10:1)
#' 
#' # one independent variable
#' nlsregression(func=y~a*x+b,y=y,x=x,startvalues=list(a=1,b=1))
#' # two independent variables
#' nlsregression(func=y~a*x^1.1+b*z+c*x,y=y,x=x,z=z,startvalues=list(a=1,b=1,c=0))
#' # no fit because residuals are zero (excluded from the nls makers due
#'  to statistical reasons)
#' nlsregression(func=y~x^a+b,y=y,x=x,z=z,startvalues=list(a=1,b=1,c=0))
#' 
#' DNase1 <- subset(DNase, Run == 1)
#' DNase1$sets<- c(rep(1,8),rep(2,8))
#' nlsregression(func=y~a*x+b,y=DNase1$density,x=DNase1$conc,startvalues=list(a=1,b=1))
#' nlsregression(func=y~a*x+b*z,y=DNase1$density,x=DNase1$conc,z=DNase1$sets,
#' startvalues=list(a=0.1344,b=0.2597))
#' nlsregression(func=y~a*x+b*z,y=DNase1$density,x=DNase1$conc,z=DNase1$sets,
#' startvalues=list(a=0.1344,b=0.2597),plot_x_function=log)
#' }
#' @export
#' @importFrom stats shapiro.test
#' @importFrom stats cor
#' @importFrom stats lm
#' @importFrom stats logLik
#' @importFrom stats nls
#' @importFrom stats predict
#' @importFrom stats resid
#' @importFrom stats var
#' @importFrom stats weighted.mean
#' @importFrom stats weights
#' @importFrom graphics plot abline legend lines points text curve
#' @importFrom grDevices colorRampPalette
#' @importFrom lmtest bptest
#' @importFrom boot corr
#' @importFrom nlstools confint2

nlsregression <- function(func, # y ~ a*x/b+x
                          y, x, z=NULL, 
                          startvalues=NULL, 
                          weight=NULL, 
                          weighting=TRUE, 
                          xlab=NULL, 
                          ylab="y",
                          header=NULL,
                          z_plot_lines=NULL,
                          weightcolorpoints=TRUE,
                          x_log10=FALSE,
                          toPlot="all",
                          plot_x_function="ignore",
                          regressioncolor="blue",
                          weight_threshold=NULL,
                          ...)
{
  rounding_helper<-function(x) {
    if ((x>1000)|(x< -1000)) {
      x=format(signif(x,4), scientific=TRUE)
    } else if ((x<0.01)&(x> -0.01)){
      x=format(signif(x,4), scientific=TRUE)
    } else if(x>0){
      x<-format(signif(x,4), scientific=FALSE)
    }
    return(x)
  }

  nlsplotlines<-function(function2,regressioncolor,z_plot_lines,x_log10){

    if(x_log10==TRUE){
      revert=function(x){10^x}
      }else {
        revert=function(x){x}
    }
    if(is.null(z_plot_lines)) {
      curve(function2(revert(x)),add = T,col=regressioncolor,lty=1,lwd=3)
    } else {
      for (i in 1:length(z_plot_lines)) {
        curve(function2(revert(x),z=z_plot_lines[i]),add = T,col=regressioncolor[i],lty=1,lwd=3)
      } 
    } 
    
    

  }
  
  nlsplotinfos<-function(combined, z_plot_lines, plot_x_function, regressioncolor, returnvalue, toPlot){
    
    if("infos1"%in%toPlot) {  ### if several functions shall be plotted into the same plot
      spacecorrection=0.05
    } else if ("infos2"%in%toPlot){
      spacecorrection=0.1
    } else if ("infos3"%in%toPlot){
      spacecorrection=0.15
    } else {
      spacecorrection=0
    }
    
    if(!is.null(z_plot_lines)){
      text(labels = paste0("z:"), 
           col=regressioncolor, 
           x=min(plot_x_function(combined$x),na.rm=T),
           y=max(combined$y,na.rm=T)*(0.9),
           pos=4) 
      
      for (i in 1:length(z_plot_lines)) {
        text(labels = paste0(z_plot_lines[i]), 
             col=regressioncolor[i], 
             x=min(plot_x_function(combined$x),na.rm=T)+
               max(plot_x_function(combined$x),na.rm=T)*0.05,
             y=max(combined$y,na.rm=T)*(0.95-(i*0.05)),pos=4) 
      }
    }
    
    ###bottomright
    text(labels = returnvalue$formula,     
         col=regressioncolor, 
         x=max(plot_x_function(combined$x),na.rm=T),
         y=min(combined$y,na.rm=T)+ max(combined$y,na.rm=T)*spacecorrection,
         pos=2)   
    
    ### topleft
    text(labels = paste0("Efron's weighted adj. R2: ",
                         round(returnvalue$efrons_weighted_adj_r2,2)),
         col=regressioncolor, 
         x=min(plot_x_function(combined$x),na.rm=T),
         y=max(combined$y*(1-spacecorrection),na.rm=T),
         pos=4) 
    
    ## topright
    text(labels = paste0("S: ",rounding_helper(returnvalue$standarderror)),    
         col=regressioncolor,
         x=max(plot_x_function(combined$x),na.rm=T),
         y=max(combined$y*(1-spacecorrection),na.rm=T),
         pos=2)
    
    #bottomleft
    text(labels = paste0(returnvalue$observations," observations"),  
         col=regressioncolor,
         x=min(plot_x_function(combined$x),na.rm=T),
         y=min(combined$y,na.rm=T)+ max(combined$y,na.rm=T)*spacecorrection,
         pos=4) 
  }
  
  nlsplotframe <- function(combined, xlab, ylab, plot_x_function, header){
    if(is.null(xlab)) {
      if(is.primitive(plot_x_function)) {
        #xlab=paste0(as.character(substitute(plot_x_function)),"(",xlab,")")
        xlab=gsub(".Primitive(\"", "", format(plot_x_function), fixed = TRUE)
        xlab=gsub("\")", "", xlab)
        xlab<-paste0(xlab,"(x)")
      } else {
        xlab=gsub(" ", "", format(plot_x_function)[[3]], fixed = TRUE)
      }
    }
    plot(combined$y~plot_x_function(combined$x),type="n",
         xlab=xlab,ylab=ylab,
         main=header)
  }
  
  nlsplotpoints<-function(combined, plot_x_function, z_plot_lines, colors_f){
    
    weight = combined$weight
    
    weight_classes<-list()
    if(!all(weight==1)&(weightcolorpoints==TRUE)) { 
      weight_classes[[1]]<-quantile(weight,na.rm=T,probs=c(0,0.333))
      weight_classes[[2]]<-quantile(weight,na.rm=T,probs=c(0.333,0.666))
      weight_classes[[3]]<-quantile(weight,na.rm=T,probs=c(0.666,1))
      color_order=c(3,2,1)
    } else {
      weight_classes[[1]]<-quantile(weight,na.rm=T,probs=c(0,1))
      color_order=c(1)
    }
    
    if(!("z"%in%names(combined))) {
      colors_points=c("#000000","#595959","#ADADAD")
      for (weight_class_x in (1:length(weight_classes))) {
        weight_class_x_elements<-(weight>=weight_classes[[weight_class_x]][1])&(weight<=weight_classes[[weight_class_x]][2])
        tmp<-which(weight_class_x_elements)
        points(combined$y[tmp]~plot_x_function(combined$x[tmp]),pch=1,cex=0.5,col=colors_points[color_order[weight_class_x]])
      }
    }else {
      
      for (weight_class_x in (1:length(weight_classes)))  {
        weight_class_x_elements<-(weight>=weight_classes[[weight_class_x]][1])&(weight<=weight_classes[[weight_class_x]][2])
        colors_points<-colors_f[[color_order[weight_class_x]]](length(z_plot_lines)*2-1)[(1:(length(z_plot_lines)-1))*2]
        for (i in 1:(length(z_plot_lines)-1))
        {
          tmp<-which((combined$z>=z_plot_lines[i])&(combined$z<=z_plot_lines[i+1])&weight_class_x_elements)
          points(combined$y[tmp]~plot_x_function(combined$x[tmp]),pch=1,cex=0.5,col=colors_points[i])
        }
        colors_startend<-colors_f[[color_order[weight_class_x]]](length(z_plot_lines)*2-1)[c(1,length(z_plot_lines)*2-1)]
        points(combined$y[which(combined$z==z_plot_lines[1])]~plot_x_function(combined$x[which(combined$z==z_plot_lines[1])]),pch=1,cex=0.5,col=colors_startend[1])
        points(combined$y[which(combined$z==tail(z_plot_lines,n=1))]~plot_x_function(combined$x[which(combined$z==tail(z_plot_lines,n=1))]),pch=1,cex=0.5,col=colors_startend[2])
      }
    }
    
  }
  
  transform_formula_to_function<-function(func,z){
    funcarguments<-all.vars(func)[-1] #names(formals(func))
    funcparas<-funcarguments[which(!funcarguments%in%c("x","y","z"))]
    funcvars = all.vars(func)[-1][1]
    if(length(func[[3]])>1) {
      numberofparas=length(all.vars(func)[-1])
      if(numberofparas==1){numberofparas=2}
      for (i in 2 : numberofparas) {
        tmp=all.vars(func)[-1][i]
        if(is.na(tmp)){tmp=NULL}
        funcvars = c(funcvars,tmp)
      }
      if(!is.null(z)){
        if(!"z"%in%funcvars){funcvars<-c(funcvars,"z")}
      }
    }
    if(length(func[[3]])==1)  {
      func=paste0("function(",paste0(funcvars,collapse = ","),"){",as.character(func[[3]]),"}")
    } else {
      func=paste0("function(",paste0(funcvars,collapse = ","),"){",format(func[[3]]),"}")
    }
    func=eval(parse(text=func))
    return(func)
  }
  
  efrons_pseudo_r2<-function(model){
    pred <- predict(model)
    n <- length(pred)
    res <- resid(model)
    w <- weights(model)
    if (is.null(w)) w <- rep(1, n)
    rss <- sum(w * res ^ 2)
    resp <- pred + res
    center <- weighted.mean(resp, w)
    r.df <- summary(model)$df[2]
    int.df <- 1
    tss <- sum(w * (resp - center)^2)
    r.sq <- 1 - rss/tss
    adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
    out <- list(pseudo.R.squared = r.sq,
                adj.R.squared = adj.r.sq)
    return(out)
  }
  
 
  if(is.magpie(x)){x<-as.vector(x)}
  if(is.magpie(y)){y<-as.vector(y)}
  if(is.magpie(z)){z<-as.vector(z)}
  if(is.magpie(weight)){weight<-as.vector(weight)}
  
  
  if(plot_x_function!="ignore"){stop("argument plot_x_function is depreciated, please remove")}
  if(x_log10==TRUE){
    plot_x_function=log10
  } else {
    plot_x_function=function(x){x}
  }
  
  
  funcarguments<-all.vars(func)[-1] #names(formals(func))
  funcparas<-funcarguments[which(!funcarguments%in%c("x","y","z"))]
  funcvars = all.vars(func)[-1][1]
  
  func <- transform_formula_to_function(func,z)


  
  # remove all NA
  naVec = y * x
  if (!is.null(z))
  {
    naVec = naVec * z
  }
  if (!is.null(weight))
  {
    if(!is.null(weight_threshold)){
      weight[weight<weight_threshold]<-NA
    }
    naVec = naVec * weight
  }
  for(i in length(naVec):1) {
    if(is.na(naVec[i]))
    {
      y = y[-i]
      x = x[-i]
      if (!is.null(z))
      {
        z = z[-i]
      }
      if (!is.null(weight))
      {
        weight = weight[-i]
      }
    }
  }
  
  #
  
  
  if (is.null(weight))  {
    weight<- rep(1,length(x))
  }
  
  if (is.null(startvalues))  {
    startvalues<-as.list(rep(1,length(funcparas)))
    names(startvalues)<-funcparas
  }
  
  
  if(is.null(z)){
    combined <- data.frame(y=y,x=x,weight=weight)
    formula1<-as.formula(paste("y ~ func(x=x,", paste(funcparas,collapse = ","),")"))
    z_plot_lines = NULL
    colors_f=NULL
    
  } else {
    combined <- data.frame(y=y,x=x,z=z,weight=weight)
    formula1<-as.formula(paste("y ~ func(x=x, z=z,", paste(funcparas,collapse = ","),")"))
    if(is.null(z_plot_lines)) {
      z_plot_lines<-quantile(z)
    }
    colors_f<-list()
    colors_f[[1]]<-colorRampPalette(c("#0000FF", "#FF0000"))
    colors_f[[2]]<-colorRampPalette(c("#5050FF", "#FF5050"))
    colors_f[[3]]<-colorRampPalette(c("#A0A0FF", "#FFA0A0"))
    
    colors       <- colors_f[[1]](length(z_plot_lines)*2-1)
    regressioncolor <- colors[(1:(length(z_plot_lines)))*2-1]
  }
  
    
  ### Plotting points
  if(any(c("all","frame")%in%toPlot))    {
    nlsplotframe(combined=combined, 
                 xlab=xlab, 
                 ylab=ylab, 
                 plot_x_function=plot_x_function, 
                 header=header)
  }
  
  if(any(c("all","observations")%in%toPlot))    {
    nlsplotpoints(combined = combined, 
                  plot_x_function=plot_x_function, 
                  z_plot_lines=z_plot_lines,
                  colors_f=colors_f)
  }
  
  
  ### regression
  if (weighting) {
    
    tt<-try(nls(formula1,
                data=combined,
                start=startvalues,
                weights=weight,
                ...),TRUE)    
  } else {
    
    tt<-try(nls(formula1,
                data=combined,
                start=startvalues,
                ...),TRUE) 
  }
  
  if (inherits(tt,"try-error")) {
    print(tt)
  } else {
    if (weighting)  {
      opt <- nls(formula1,
                 data=combined,
                 start=startvalues,
                 weights=weight,...
      )
    } else {
      opt <- nls(formula1,
                 data=combined,
                 start=startvalues,...)
    }
  }
  
  if (!inherits(tt, "try-error")){
    
    ### statistics
    copt <- cbind(summary(opt)$coefficients,confint2(opt))
    para <- as.numeric(copt[,"Estimate"])
    ss   <- summary(opt)$convInfo$stopCode    
    obs <- length(summary(opt)$residual)
    loglik <- logLik(opt)
    
    if(length(length(summary(opt)$residual))>3&length(summary(opt)$residual)<5000){
      norm_test <- shapiro.test(summary(opt)$residual)[["p.value"]] 
    } else {
      norm_test <- NULL
    }
    # bp_test <- bptest(formula1,data=combined)[["p.value"]]
    robust_out <- robust_vce(opt)
    
    prediction = predict(opt)
    observation=y
    lm_obs_vs_est = lm(prediction~observation,weights=weight)
    
    
    ###R2
    # Coefficient of determination
    pseudo_R2_unweighted <- max(cor(observation,prediction),0)^2
    pseudo_R2_weighted <- max(corr(matrix(data = c(observation,prediction),ncol = 2),w = combined$weight),0)^2
    pseudo_explained_sd = (var(observation)-var(prediction-observation))/var(observation)
    efrons_r2 = efrons_pseudo_r2(opt)
    
    standarderror=(sum((prediction-observation)^2)/length(observation))^0.5
    
    
    ### transforming formulas into expression or functions
    
    formula2<-gsub(" ", "", format(func)[[3]], fixed = TRUE)
    for (i in 1:length(funcparas))     {
      formula2 <- gsub(funcparas[i], rounding_helper(para[i]), formula2, fixed = TRUE)
    }
    
    function2<-eval(parse(text=paste0("y~",formula2)))
    function2 <- transform_formula_to_function(function2,z)
    
    formula2<-paste0("y=",formula2)
    
    
    returnvalue <- list(opt=opt,
                        coefficients=copt,
                        formula=formula2,
                        function2=function2,
                        lm_observed_vs_estimated=lm_obs_vs_est,
                        pseudo_R2_unweighted=pseudo_R2_unweighted,
                        efrons_weighted_r2=efrons_r2$pseudo.R.squared, 
                        efrons_weighted_adj_r2=efrons_r2$adj.R.squared,
                        standarderror=standarderror,
                        observations = obs,
                        loglik = loglik,
                        norm_test = norm_test,
                        # bp_test = bp_test,
                        robust_out = robust_out
    )
    
    if(any(c("all","regression")%in%toPlot))    {    
      nlsplotlines(function2,
                   regressioncolor=regressioncolor, 
                   z_plot_lines=z_plot_lines,
                   x_log10=x_log10)
    }
    
    if(any(c("all","infos","infos1","infos2","infos3")%in%toPlot))    {
      nlsplotinfos(combined=combined, 
                   z_plot_lines=z_plot_lines, 
                   plot_x_function=plot_x_function, 
                   regressioncolor=regressioncolor, 
                   returnvalue=returnvalue,
                   toPlot=toPlot)
    }
    
  } else  {
    returnvalue <- list(modelVSdata="no fit",
                   opt="no fit",r2=0)
  }
  return(returnvalue)
}


