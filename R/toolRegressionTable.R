#' @title toolRegressionTable
#' @description creates Regression for selected options and saves calculated parametes inside the table.
#'
#' @param scenario vector. Default "SSP2". Can be "SSP1", "SSP2", "SSP3", "SSP4", "SSP5" or "mix" and describes the overall scenario of the projection.
#' @param x Indep Var
#' @param denominator vector. Default NA. Specific fooddenominator share to make projection for.
#' @param quotient vector. Default is population ("pop")
#' @param z other independent variables
#' @param start_1 Default NA. Startvalue for 1st parameter.
#' @param start_2 Default NA. Startvalue for 2nd parameter.
#' @param start_3 Default NA. Startvalue for 3rd parameter.
#' @param start_4 Default NA. Startvalue for 4th parameter.
#' @param start_5 Default NA. Startvalue for 5th parameter.
#' @param start_6 Default NA. Startvalue for 6th parameter.
#' @param regression_database_file file with regressions to calculate
#' @param return_value Default to False. This is to stop printing the updated dataset on console. If you'd like to keep the updated dataset as an object, set this to true.
#'
#' @return data frame with additional rows containing parameters of newly calculated regression.
#' @author Abhijeet Mishra, Eleonora Martinelli
#' 
#' @import utils
#' @importFrom stats as.formula
#' @seealso \code{\link{toolRegression}}
#' @export

toolRegressionTable <- function(           scenario="SSP2",
                                           x="IHME_USD05_PPP_pc",
                                           denominator=NA,
                                           z=NA,
                                           regression_database_file = "scenario_database_regressionworlddata.csv",
                                           quotient = "pop",
                                           start_1 = NA,
                                           start_2 = NA,
                                           start_3 = NA,
                                           start_4 = NA,
                                           start_5 = NA,
                                           start_6 = NA,
                                           return_value=FALSE)
{
  t1 <- Sys.time()
  table_database <- toolGetMapping(type = "settings", name = regression_database_file)

  table <- table_database
  x_IndepVar <- x

  toPlot=TRUE

  version <- gsub(pattern = "-",replacement = "",Sys.time())
  version <- gsub(pattern = " ",replacement = "_",version)
  version <- gsub(pattern = ":",replacement = "",version)

  
  new_table <- table
  
  for (i in 1:nrow(table))
  {
    print(paste(i, "of", nrow(table)))

    denom <- denominator
    quot <- quotient
    
    weight <- table[i,"weight"]
    if (is.null(weight)) {
      weight <- quot
    }
    if(!denom %in% unique(table$denominator)) 
    {
      denominator = eval(denom)
    } else {
      denominator = denom
    }
    if(quot=="NULL"){
      quotient<-NULL
    } else {
      if(!quot %in% getNames(data)) 
      {
        quotient = eval(quot)
      } else {
        quotient = quot
      }
      
    }

    func <- as.formula(table[i,"functional_form"])
    yname <- paste(denom,"per",quot)
    
    if(denom == "kfo"){
      denominator <- findset(denom)
    }
    
    if(quot == "kfo"){
      quotient <- findset(quot)
    }
    
    regrData <- toolRegression(denominator,quotient,func=func,
                               x = x,weight=weight,ylab = yname,toPlot=toPlot)
    
    if (regrData[1]=="no fit")
    {
      allParas <-rep("no fit",6)
      R2 <- "no fit"
      R2adj <- "no fit"
    } else {
      allParas <-rep(NA,6)
      for (i_para in 1:6)
      {
        allParas[i_para] <- regrData$opt$m$getPars()[i_para]
      }
      R2 <- regrData$modelVSdata[8]
      R2adj <- regrData$modelVSdata[9]
    }
    
    if (x_IndepVar == "IHME_USD05_PPP_pc"){
      x_IndepVar <- "gdp"
    }
    para_start_index <- which(colnames(new_table)=="para_1")
    para_end_index <- which(colnames(new_table)=="para_6")
    
    newRow <- unlist(c(paste0(x_IndepVar,"_",version),scenario,denom,quot,new_table[i,"functional_form"],z,
                       allParas,
                       R2,R2adj,
                       new_table[i,para_start_index:para_end_index]))
    new_table <- rbind(new_table, newRow)
  }
  new_table <- rbind(table_database,new_table)
  new_table = unique(new_table)
  
  filename <- paste0(getConfig("mappingfolder"),"/settings/",regression_database_file)
  write.csv(new_table,filename, row.names = FALSE,quote = F)
  
  t2 <- Sys.time()
  cat("\nDatabase updated successfully in",round(t2-t1,digits = 2),"seconds.\n")
  if(return_value){
    return(new_table)
  }
}
