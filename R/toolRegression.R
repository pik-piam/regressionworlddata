#' @title toolRegression
#' @description Regression model for the correlation of a denominator and quotient to the GDP, allowing for an additional driver z next to income.
#'
#' @param denominator denominator of the dependent variable that shall be estimated using the regression
#' @param quotient quotient of the dependent variable that shall be estimated using the regression
#' @param func functional relation for the regression, shall be in the format y~f(x,...) with x being gdp, y being denominator/quotient, and f() being any type of functional relationship. ... can inlcude either z or parameters to be estimated. 
#' @param x independet variable, by default income
#' @param z additional independet variable
#' @param xlab name of x axis
#' @param ylab name of y axis
#' @param data data can be provided if Data shall not be derived by moinput:::calcCollectFoodDemandRegressionData()
#' @param countries_nlsAddLines the number of weightiest countries or the name of countries that shall be plotted by lines in the plot
#' @param weight the weight
#' @param x_log10 passed on to nlsregression()
#' @param ... further attributes that will be handed on to nlsregression(): 
#' 
#' An additional explanatory variable z can be added.
#' A regression model has to be chosen.
#' Startvalues can be predetermained.
#'
#' @return regression plot and the parameters from nlsregression
#' @author Antonia Walther, Benjamin Leon Bodirsky
#' 
#' 
#' 
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ 
#' 
#' toolRegression(denominator=livestock,
#'                      func=y~(a*x)/(b+x),
#'                      z=NULL,
#'                      startvalues=list(a=1100,b=7770)
#'                      )
#'                      
#' toolRegression(denominator=findset("kap"),
#'                      quotient=findset("kfo"),
#'                      func=y~(a*x)/(b+x),
#'                      z=NULL,
#'                      startvalues=list(a=0.5,b=7770)
#'                      )
#'                      
#' }
#' @importFrom magpiesets findset
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' 
#' @export

toolRegression<-function(denominator, 
                               quotient = NULL,
                               func=y~(a*x)/(b+x),
                               x="IHME_USD05_PPP_pc",
                               z=NULL,
                               ylab = NULL,
                               xlab=NULL,
                               data = NULL,
                               countries_nlsAddLines=NULL,
                               weight="pop",
                               x_log10=FALSE,
                               ...
                               )
{
  if (is.null(data)){ 
    data<-toolCollectRegressionVariables(indicators=c(denominator,quotient,x,z,weight))
  }
  
  if(is.null(xlab)){
    if(length(x)>1){
      xlab<-deparse(substitute(x))
    }else {
      xlab<-x  
    }
  }
  
  denom_name <- deparse(substitute(denominator))
  quot_name <- deparse(substitute(quotient))

  if(!all(denominator%in%getNames(data))){
    denominator <- eval(parse(text = denominator))
    denom<-findset(denominator)
  } else {
    denom=denominator
  }
  
  if(!all(quotient%in%getNames(data))){
    quotient <- eval(parse(text = quotient))
    quot<- findset(quotient)
  } else {
    quot=quotient
  }
  
  
  if(is.null(quotient)){
    quot=1
  } else {
    quot = dimSums(data[,,quot],dim=3)
  }
  
  if(is.null(weight)){
    weight=1
  } else {
    weight = dimSums(data[,,weight],dim=3)
  }
  
  denom = dimSums(data[,,denom],dim=3)
  
  #gdp per capita ausrechnen und z(urban oder education shr) ausrechnen
  driv1<-dimSums(data[,,x],dim=3)
  
  if(length(x)>1){driv1=dimSums(driv1,dim=3)}
  
  if(is.null(z))
  {
    driv2 <- NULL
  } else {
    driv2<-data[,,z]
  }
  
  if (is.null(ylab))
  {
    if(!is.null(quotient)){
      ylab <- paste("(",denom_name,")/(",quot_name,")")
    } else {
      ylab <- paste(denom_name)
    }
  } 
  
  
  out<-nlsregression(
    func=func,
    y=as.vector(denom/quot),
    x=as.vector(driv1),
    z=as.vector(driv2),
    weight = as.vector(weight), 
    xlab=xlab, 
    ylab=ylab,
    x_log10=x_log10,
    ...
    )
 
  if(!is.null(countries_nlsAddLines)){
    
    # nice color algorithm from Jelena-bioinf in stackoverflow
    n <- length(countries_nlsAddLines)
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    
    nlsAddLines(y = denom/quot,
                x=driv1,
                weight=weight,
                countries = countries_nlsAddLines, 
                colors = col_vector,
                x_log10=x_log10)
  }
  return(out)
}
