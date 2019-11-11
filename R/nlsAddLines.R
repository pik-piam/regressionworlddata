#' @title nlsAddLines
#' @description Adds lines of specific countries into the plot of the function nlsregression. 
#' nlsregression has to be based on magpie objects for x,y,weight
#'
#' @param x magpie object with x values
#' @param y magpie object with y values
#' @param weight magpie object with weight
#' @param x_log10 same as in nlsregression
#' @param colors colors of the lines
#' @param countries Choice of countries
#' @param labels If TRUE, the region, staryear and endyear will be plotted to each line.
#' @seealso
#' \code{\link{nlsregression}}
#'
#' @return vector with ISO-countrycodes
#' @author Benjamin Leon Bodirsky
#' @export
#' @examples
#' 
#' \dontrun{ 
#' data(population_magpie)
#' nlsregression(y=population_magpie[,,1],x=population_magpie[,,2],
#' weight = population_magpie[,,1],func = y~a*x+b)
#' nlsAddLines(y=population_magpie[,,1],x=population_magpie[,,2],
#' weight = population_magpie[,,1],countries=1:3,colors=1:3)
#' }
#' @importFrom graphics lines text
#' 
#' 
nlsAddLines<-function(y,x,countries=1:5,weight=NULL,x_log10=FALSE,colors="black",labels=TRUE){
  if(x_log10==TRUE){
    plot_x_function=log10
  } else {
    plot_x_function=function(x){x}
  }
  if (all(is.numeric(countries))){
    if (is.null(weight)){"If no specific countries are selected, weight has to be provided"}
    countries<-getRegions(sort(dimSums(weight,dim=c(2,3)),decreasing = T)[countries])
  }
  if (length(colors)==1){
    colors=rep(x = colors,length(countries))
  }
  for (i in length(countries):1) {
    country<-countries[i]
    lines(y[country,,] ~ plot_x_function(x[country,,]),lwd = 2,col=colors[i])
    if(labels==TRUE){
      maxyear=getYears(x)[length(getYears(x))]
      text(x = plot_x_function(as.vector(x[country,,])[1]),y = as.vector(y[country,,])[1],labels = paste(country,getYears(x[,1,])),col=colors[i],cex=1)
      text(x = plot_x_function(as.vector(x[country,maxyear,])),y = as.vector(y[country,maxyear,]),labels = paste(country,maxyear),col=colors[i],cex=1)
    }
  }
}


