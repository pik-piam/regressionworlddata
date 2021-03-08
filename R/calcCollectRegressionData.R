#' @title calcCollectRegressionData
#' @description collects regression data using un-converted raw data sources, and crops the data that only joint years and countries are selected.
#'
#' @param datasources All datasources that shall be returned. Due to the cropping of data which is not present in all datasources, reducing the number of datasources will increase the number of observations.
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Eleonora Martinelli, Abhijeet Mishra, Xiaoxi Wang
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("CollectRegressionData",aggregate=F)
#' }
#' 

#' @importFrom countrycode countrycode
#' @importFrom magpiesets findset
#' @importFrom mrcommons toolFAOcombine
#' @importFrom stats quantile
#' @importFrom stats runif
#' @import magclass
#' @import madrat
#' @export

calcCollectRegressionData <- function(datasources){
  
  combined<-list()
  
  if ("wooddemand" %in% datasources) {
    wooddemand <- collapseNames(calcOutput("TimberDemand",aggregate = F)[,,"domestic_supply"])
    combined$wooddemand<-wooddemand
  }
    
  if ("SelfSuff" %in% datasources) {
    SelfSuff_w <- calcOutput("TradeSelfSuff",convert = F)[,,"wood"]
    SelfSuff_wf <- calcOutput("TradeSelfSuff",convert = F)[,,"woodfuel"]
    SelfSuff <- mbind(SelfSuff_w,SelfSuff_wf)
    SelfSuff <- SelfSuff[,sort(getYears(SelfSuff)),]
    SelfSuff <- SelfSuff[which(!is.na(dimnames(SelfSuff)[[1]])),,]
    getNames(SelfSuff) <- paste0("ss_",getNames(SelfSuff))
    combined$SelfSuff<-SelfSuff
  }
    
  if ("timber_demand" %in% datasources) {
    timber_demand <- collapseNames(calcOutput("TimberDemand",aggregate = FALSE)[,,"domestic_supply"])
    combined$forestry<-timber_demand
  }
    
  if ("forest_area" %in% datasources) {
    FA_LUI <- calcOutput("LanduseInitialisation",aggregate = FALSE)
    
    forest_area <- dimSums(FA_LUI[,,c("forestry","primforest","secdforest")],dim=3)
    forest_area <- setNames(forest_area,c("forest_area"))
    combined$forest_area<-forest_area
  }

  if ("urbanization_WDI" %in% datasources) {
    urban <- setNames(readSource("WDI",subtype="SP.URB.TOTL.IN.ZS",convert = FALSE)/100,"urban")
    urban<-urban[,sort(getYears(urban)),]
    getCells(urban) <- countrycode(getCells(urban),"iso2c","iso3c")
    urban <- urban[which(!is.na(dimnames(urban)[[1]])),,]
    combined$urban<-urban
  }

  if ("population_WDI" %in% datasources) {
  
    pop <- setNames(readSource("WDI",subtype="SP.POP.TOTL",convert = TRUE),"pop")
    pop<-pop[,sort(getYears(pop)),]
    pop <- pop[which(!is.na(dimnames(pop)[[1]])),,]
    #pop[,"y2014",] <- setYears(pop[,"y2013",])+ setYears(pop[,"y2013",])-setYears(pop[,"y2012",])
    combined$pop<-pop
  }
    
  if ("population_WDI" %in% datasources) {
    
    pop <- setNames(readSource("WDI",subtype="SP.POP.TOTL",convert = TRUE),"pop")
    pop<-pop[,sort(getYears(pop)),]
    pop <- pop[which(!is.na(dimnames(pop)[[1]])),,]
    #pop[,"y2014",] <- setYears(pop[,"y2013",])+ setYears(pop[,"y2013",])-setYears(pop[,"y2012",])
    combined$pop<-pop
    }  
  
  if ("gdp" %in% datasources) {
    gdp_pc <- readSource("James",convert = F)[,,"IHME_USD05_PPP_pc"]
    gdp_pc <- gdp_pc[,,"IHME_USD05_PPP_pc"]
    combined$gdp_pc<-gdp_pc
  }
    
  if ("bodyheight" %in% datasources) {
    bodyheight_wrongyears <-readSource("NCDrisc",subtype="height",convert=FALSE)
    bodyheight<-new.magpie(cells_and_regions = getRegions(bodyheight_wrongyears),
                           years = (1961+17):(max(getYears(bodyheight_wrongyears,as.integer = TRUE))+17),
                           names=getNames(bodyheight_wrongyears))
    for(years in getYears(bodyheight,as.integer = TRUE)){
      bodyheight[,years,]=setYears(bodyheight_wrongyears[,years-17,],years)
    }
    bodyheight <- setNames(bodyheight,paste0("bodyheight_",getNames(bodyheight)))
    combined$bodyheight<-bodyheight
  }
  
  if ("intake_pc_schofield" %in% datasources) {
    intake <- calcOutput("Intake2",convert=FALSE, modelinput=TRUE, standardize=FALSE, method="schofield", aggregate=FALSE)
    intake <- setNames(collapseNames(intake),"intake_pc_schofield")
    combined$intake<-intake
  }
  
  if ("intake_pc_FAO_WHO_UNU1985" %in% datasources) {
    intake <- calcOutput("Intake2",convert=FALSE, modelinput=TRUE, standardize=FALSE, method="FAO_WHO_UNU1985", aggregate=FALSE)
    intake <- setNames(collapseNames(intake),"intake_pc_FAO_WHO_UNU1985")
    combined$intake<-intake
  }
 
  if ("intake_pc_Froehle" %in% datasources) {
    intake <- calcOutput("Intake2",convert=FALSE, modelinput=TRUE, standardize=FALSE, method="Froehle", aggregate=FALSE)
    intake <- setNames(collapseNames(intake),"intake_pc_Froehle")
    combined$intake<-intake
  }
  
  if ("intake_demography" %in% datasources) {
    intake <- calcOutput("Intake",convert=FALSE, modelinput=FALSE, standardize=FALSE, method="schofield", aggregate=FALSE)
    intake<-collapseNames(intake[,,"SSP2"][,,c("F","M")])
    getNames(intake)<-paste0("intake_",sub(x = getNames(intake),pattern = "\\.",replacement = "_"))
    getSets(intake)<-c("region","year","intake")
    combined$intake_demography<-intake
  }
    
  if ("intake_standardized_demography" %in% datasources) {
    intake <- calcOutput("Intake",convert=FALSE, modelinput=FALSE, standardize="BMI", method="Froehle", aggregate=FALSE)
    intake<-collapseNames(intake[,,"SSP2"][,,c("F","M")])
    getNames(intake)<-paste0("intake_standardized_",sub(x = getNames(intake),pattern = "\\.",replacement = "_"))
    getSets(intake)<-c("region","year","intake_standardized")
    combined$intake_standardized<-intake
  }
    
  if ("intake_pc_standardized_BMI_FAO_WHO_UNU1985" %in% datasources) {
    intake <- calcOutput("Intake",convert=FALSE, modelinput=TRUE, standardize="BMI", method="FAO_WHO_UNU1985", aggregate=FALSE)
    intake <- setNames(collapseNames(intake[,,"SSP2"]),"intake_pc_standardized_BMI_FAO_WHO_UNU1985")
    intake <- time_interpolate(intake,interpolated_year = paste0("y",1965:2010),integrate_interpolated_years = FALSE)
    combined$intake_BMI<-intake
  }    
    
  if ("physical_inactivity" %in% datasources) {
    inactive1=readSource("WHO",subtype = "physical_inactivity_adults",convert=FALSE)
    inactive2=readSource("WHO",subtype = "physical_inactivity_underaged",convert=FALSE)
    getNames(inactive1)<-paste0("inactivity_adults_",getNames(inactive1))
    getNames(inactive2)<-paste0("inactivity_underaged_",getNames(inactive2))
    combined$intake_BMI<-mbind(inactive1,inactive2)
  }

  if ("batten_last_20yrs" %in% datasources) {
    food_supply_crop <- readSource("FAO",subtype="FSCrop",convert = F)
    food_supply_live <- readSource("FAO",subtype="FSLive",convert = F)
    food_supply <- toolFAOcombine(food_supply_crop,food_supply_live, combine="Item")
    relationmatrix <-  toolGetMapping("FAOitems.rda", type = "sectoral", where="moinput")
    relationmatrix <- relationmatrix[,which(names(relationmatrix)%in%c("FoodBalanceItem","k"))]
    relationmatrix <- relationmatrix[-which(duplicated(relationmatrix[,1])==T),]
    
    vcat(2,"removing strange values for montenegro in 2004, 2005 and luxemburg in 1990:1999")
    food_supply["MNE",c("y2004","y2005"),]=0
    food_supply["LUX",1961:1999,]=0
    
    food_supply <- collapseNames(food_supply[,,"food_supply_kcal/cap/day"])
    kcal <- toolAggregate(#x = kcal,
      x = food_supply,
      rel =relationmatrix,
      dim = 3.1,
      from = "FoodBalanceItem",
      to = "k", 
      partrel=TRUE)
    
    kcal<-add_columns(kcal,addnm = c("brans","scp"))
    kcal[,,c("brans","scp")]<-0
    
    kcal <- kcal[,,"remaining",invert=TRUE]
    
    missing <- dimSums(kcal,dim=3,na.rm=TRUE) # missing values
    missing[missing == 0] <- NA
    missing[!is.na(missing)]<-1
    kcal[is.na(kcal)] = 0
    kcal = kcal * missing
    
    batten<-dimSums(kcal[,,c(
      "fish","livst_chick","livst_egg",
      "livst_milk","livst_pig","livst_rum",
      "oils",
      "puls_pro","soybean","groundnut"
      )],dim=3)
    
    kcal<-batten[,(1961+19):max(getYears(batten,as.integer = TRUE)),]*NA
    for(year_x in (1961+19):max(getYears(food_supply_crop,as.integer = TRUE))){
      kcal[,year_x,] <- dimSums(batten[,(year_x-19):(year_x-5),],dim=2)/15
    }
    combined$kcal_last_20yrs<-setNames(collapseNames(kcal),"batten_last_20yrs")
  }

  if("BMI_shr"  %in% datasources){
    x<-readSource("NCDrisc",subtype="BMI_shr",convert=FALSE)
    mapping<-toolMappingFile(type = "sectoral",name = "NCDriscBMIshr2Lutz.csv",readcsv = TRUE)
    Lutz<-calcOutput("Demography",education=FALSE,aggregate = FALSE)
    Lutz<-collapseNames(time_interpolate(Lutz[getRegions(x),,"SSP2"],interpolated_year = getYears(x),integrate_interpolated_years = FALSE))
    Lutz2<-toolAggregate(Lutz[,,c(mapping$lutz)],rel = mapping,from = "lutz",to = "NCDrisc",dim=3.2)
    getSets(Lutz2)<-c("country","year","sex","age")
    Lutz2<-dimOrder(Lutz2,perm = c(2,1))
    
    mapping<-toolMappingFile(type = "sectoral",name = "NCDriscBMIshr2agegroups.csv",readcsv = TRUE)
    twogroups<-toolAggregate(x,rel = mapping,weight = Lutz2,from = "NCDrisc",to="agegroups",dim=3.1)
    
    dimnames(twogroups)[[3]]<-gsub(pattern = "\\.",replacement = "_",dimnames(twogroups)[[3]])
    
    combined$BMI_shr<-twogroups
  }  
    
  if("BMI_shr_underaged"  %in% datasources){
    x<-readSource("NCDrisc",subtype="BMI_shr_underaged",convert=FALSE)
    x<-x[,,c("age5", "age6", "age7", "age8", "age9", "age10", "age11", "age12", "age13", "age14")]
    x<-dimSums(x,dim=3.1)/fulldim(x)[[1]][[3]]
    dimnames(x)[[3]]<-gsub(pattern = "\\.",replacement = "_",dimnames(x)[[3]])
    getSets(x)<-c("iso","year","group") 
    combined$BMI_shr_underaged<-x
  } 
    
  if ("BMI" %in% datasources){
    
    x<-readSource("NCDrisc",subtype="BMI",convert=FALSE)
    mapping<-toolMappingFile(type = "sectoral",name = "NCDrisc2Lutz.csv",readcsv = TRUE)
    BMI<-new.magpie(cells_and_regions = getRegions(x),years = getYears(x),names = c(paste0(unique(mapping$lutz),".M"),paste0(unique(mapping$lutz),".F")))
    for(i in getNames(BMI,dim=1)){
      item<-mapping$NCDrisc[mapping$lutz==i]
      BMI[,,i]=dimSums(x[,,item],dim="age")/length(item)
    }
    getNames(BMI)<-paste0("BMI_",sub(x = getNames(BMI),pattern = "\\.",replacement = "_"))
    getSets(BMI)<-c("region","year","BMI")
    combined$BMI<-BMI
  }
    
  if ("kcal" %in% datasources) {
    food_supply_crop <- readSource("FAO",subtype="FSCrop",convert = F)
    food_supply_live <- readSource("FAO",subtype="FSLive",convert = F)
    food_supply <- toolFAOcombine(food_supply_crop,food_supply_live, combine="Item")
    relationmatrix <-  toolGetMapping("FAOitems.rda", type = "sectoral", where="moinput")
    relationmatrix <- relationmatrix[,which(names(relationmatrix)%in%c("FoodBalanceItem","k"))]
    relationmatrix <- relationmatrix[-which(duplicated(relationmatrix[,1])==T),]
    
    vcat(2,"removing strange values for montenegro in 2004, 2005 and luxemburg in 1990:1999")
    food_supply["MNE",c("y2004","y2005"),]=0
    food_supply["LUX",1961:1999,]=0
    
    food_supply <- collapseNames(food_supply[,,"food_supply_kcal/cap/day"])
    kcal <- toolAggregate(#x = kcal,
      x = food_supply,
      rel =relationmatrix,
      dim = 3.1,
      from = "FoodBalanceItem",
      to = "k", 
      partrel=TRUE)
    
    kcal<-add_columns(kcal,addnm = c("brans","scp"))
    kcal[,,c("brans","scp")]<-0
    
    kcal <- kcal[,,"remaining",invert=TRUE]
    
    
    missing <- dimSums(kcal,dim=3,na.rm=TRUE) # missing values
    missing[missing == 0] <- NA
    missing[!is.na(missing)]<-1
    kcal[is.na(kcal)] = 0
    kcal = kcal * missing
    combined$kcal<-kcal
  }
  
  if ("demographics" %in% datasources){
    Lutz <- readSource("Lutz2014",convert=FALSE)
    LutzSSP2 <- time_interpolate(Lutz[,,"SSP2"],
                                 paste0("y", 1970:2011),
                                 integrate_interpolated_years=F,
                                 extrapolation_type = "linear")
    TotalBoth <- LutzSSP2[,,"Total"][,,"Both"][,,"All"]
    TotalFem <- LutzSSP2[,,"Total"][,,"Female"][,,"All"]
    TotalBothNoKids <- dimSums(LutzSSP2[,,c("Under 15","Total"),invert=TRUE][,,"Both"][,,"All"],dim=3.4)
    FemaleNoKids <- dimSums(LutzSSP2[,,c("Under 15","Total"),invert=TRUE][,,"Both"][,,"All"],dim=3.4)
    
    ### gender ###
    femShare <- mbind(setNames(
      dimSums(TotalFem/TotalBoth, dim=3),
      "femaleShare"))
    
    ### education ###  
    education<-mbind(
      setNames(dimSums(LutzSSP2[,,"Post Secondary"][,,"Both"][,,"All"] 
                       / TotalBothNoKids,
                       dim=3),
               "college"),
      setNames(dimSums(LutzSSP2[,,"Post Secondary"][,,"Female"][,,"All"]
                       / FemaleNoKids,
                       dim=3),
               "femcollege"),
      setNames(dimSums(LutzSSP2[,,c("Incomplete Primary",
                                    "No Education",
                                    "Primary")][,,"Both"][,,"All"] 
                       / TotalBothNoKids,
                       dim=3),
               "low_education")
    )
    #education[,,2] <- education[,,2] - education[,,1]

    if(any(education<0, na.rm=TRUE))
    {
      stop("education share smaller 0")
    }
    if(any(education>1, na.rm=TRUE))
    {
      stop("education share larger 1")
    }
    
    ### age ###
    age<-mbind(
      setNames(dimSums(LutzSSP2[,,"Total"][,,"Both"][,,c(
        "0--4","5--9","10--14")] 
        / TotalBoth,
        dim=3),
        "below15"),
      setNames(dimSums(LutzSSP2[,,"Total"][,,"Both"][,,c(
        "15--19","20--24","25--29","30--34","35--39",
        "40--44","45--49","50--54","55--59","60--64")] 
        / TotalBoth,
        dim=3),
        "15-64"),
      setNames(dimSums(LutzSSP2[,,"Total"][,,"Both"][,,c(
        "65--69","70--74","75--79","80--84",
        "85--89","90--94","95--99","100+")] 
        / TotalBoth,
        dim=3),
        "above64")
    )
    demographics<-mbind(education,age,femShare)
    combined$demographics<-demographics
  }
    
  if ("food_price" %in% datasources){
    price <- collapseNames(calcOutput("PriceAgriculture",datasource = "WBGEM",aggregate = FALSE))
    
    dimnames(price)[[1]] <- "DEU"
    price <- toolCountryFill(price,fill = 0 )
    for(i in getRegions(price)){
      price[i,,] <- as.matrix(price["DEU",,])
    }
    getNames(price) <- paste("price",getNames(price),sep="_")
    combined$food_price <- price
  }
  
  if ("climate"%in% datasources) {
    CZ <- readSource("Koeppen",convert=FALSE) #klimazone
    CZ <- dimSums(CZ[,,c("kg_p_af","kg_p_aw","kg_p_bs","kg_p_cf","kg_p_df","kg_p_e")],dim=3)
    CZ <- setNames(CZ,"climate")
    CZ <- add_columns(setYears(CZ,"y2010"), setdiff(getYears(combined[[1]]),"y2010"),dim = 2.1)
    CZ[,,]<-setYears(CZ[,"y2010",],NULL)
    getYears(combined[[1]])
    combined$climate<-CZ
  }
    
  if (any(grepl("crossvalid",datasources))) {  
    code=datasources[grep("crossvalid",datasources)]
    code2=strsplit(code,"_")
    randomseed = as.integer(substring(code2[[1]][2],5))
    k = as.integer(substring(code2[[1]][3],2))
    # format: crossvalid_seedX_kY
    # X is the random seed,
    # Y is the number of drawings
    
    countries = toolGetMapping("iso_country.csv", where = "moinput")
    years=paste0("y",1961:2020)
    sampleset=new.magpie(cells_and_regions = countries$x,years = years,names = code)
    set.seed(42); sampleset[,,] <- round(runif(length(sampleset))*k+0.5)

    combined$crossvalid<-sampleset
   }    
  

  mbindCommonDimensions <- function(magpielist){
    if(!is.list(magpielist)){magpielist<-list(magpielist)}
    if (length(magpielist)==2){
      a<-magpielist[[1]]
      b<-magpielist[[2]]
      ab<-c(getRegions(a),getRegions(b))
      ab_regions <- ab[duplicated(ab)]
      
      ab<-c(getYears(a),getYears(b))
      ab_time <- ab[duplicated(ab)]
      
      ab<-list(mbind(a[ab_regions,ab_time,],b[ab_regions,ab_time,]))
      mbindCommonDimensions(ab)
    } else if (length(magpielist)>2) {
      ab<-mbindCommonDimensions(list(magpielist[[1]],magpielist[[2]]))
      ab<-append(list(ab),magpielist[3:length(magpielist)])
      mbindCommonDimensions(ab)
    } else if (length(magpielist)==0) {
      stop("empty list")
    } else {return(magpielist[[1]])}
  }
  
  out<-mbindCommonDimensions(combined)
  
  ### aggregation weights
  
  
  pop <- setNames(readSource("WDI",subtype="SP.POP.TOTL",convert = T),"weight")
  #pop[,"y2014",] <- setYears(pop[,"y2013",])+ setYears(pop[,"y2013",])-setYears(pop[,"y2012",])
  
  out<-mbindCommonDimensions(list(out,pop))
  weight<-x<-out
  weight[,,]<-setNames(out[,,"weight"],NULL)
  if("pop"%in%getNames(weight)){  weight[,,"pop"]<-0 }
  

  return(list(x = x,
              weight =  weight,
              unit = "share of population, per-capita income or per-capita consumption",
              description = "Merged dataset containing raw data for regression",
              min = 0,
              na_warning=FALSE,
              isocountries = FALSE)
  )
} 
