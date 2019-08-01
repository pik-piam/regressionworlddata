#' @title toolCollectRegressionVariables
#' @description todo
#' @param indicators todo
#' @return todo
#' @author Benjamin Leon Bodirsky
#' @export

toolCollectRegressionVariables<-function(indicators){
  indicators<-unique(indicators)
  
  datasources=NULL
  if (any(c("roundwood","wood","woodfuel") %in% indicators)) {
    datasources=c(datasources,"wooddemand")
  }
  if ("forest_area" %in% indicators) {
    datasources=c(datasources,"forest_area")
  }
  
  if (any(c("Roundwood","Industrial roundwood","Wood fuel","Other industrial roundwood","Pulpwood",
            "Sawlogs and veneer logs","Fibreboard","Particle board and OSB","Wood pulp","Sawnwood",
            "Plywood","Veneer sheets","Wood-based panels" ,
            "Other sawnwood") %in% indicators)) {
    datasources=c(datasources,"timber_demand")
  }
  
  if ("urban" %in% indicators) {
    datasources=c(datasources,"urbanization_WDI")
  }
  if ("pop" %in% indicators) {
    datasources=c(datasources,"population_WDI")
  }
  if (any(c("ss_wood","ss_woodfuel") %in% indicators)) {
    datasources=c(datasources,"SelfSuff")
  }
  if ("IHME_USD05_PPP_pc" %in% indicators) {
    datasources=c(datasources,"gdp")
  }
  if (any(findset("kfo") %in% indicators)) {
    datasources=c(datasources,"kcal")
  }
  if (any("kcal_last_20yrs" %in% indicators)) {
    datasources=c(datasources,"kcal_last_20yrs")
  }
  if (any("batten_last_20yrs" %in% indicators)) {
    datasources=c(datasources,"batten_last_20yrs")
  }
  if (any(c("inactivity_adults_M","inactivity_adults_F","inactivity_underaged_M","inactivity_underaged_F") %in% indicators)) {
    datasources=c(datasources,"physical_inactivity")
  }
  if (any("overconsumption_last_20yrs" %in% indicators)) {
    datasources=c(datasources,"overconsumption_last_20yrs")
  }
  
  if (any(c("college","femcollege","low_education","below15","15-64","above64","femaleShare") %in% indicators)){
    datasources=c(datasources,"demographics")
  }
  if (any(c("climate") %in% indicators)) {
    datasources=c(datasources,"climate")
  }
  if (any(c("intake_pc_schofield") %in% indicators)) {
    datasources=c(datasources,"intake_pc_schofield")
  }
  if (any(c("intake_pc_FAO_WHO_UNU1985") %in% indicators)) {
    datasources=c(datasources,"intake_pc_FAO_WHO_UNU1985")
  }
  if (any(c("intake_pc_Froehle") %in% indicators)) {
    datasources=c(datasources,"intake_pc_Froehle")
  }
  if (any(c("intake_pc_FAO_WHO_UNU1985_M_old") %in% indicators)) {
    datasources=c(datasources,"intake_pc_FAO_WHO_UNU1985_M_old")
  }
  if (any(c("intake_pc_FAO_WHO_UNU1985_F_old") %in% indicators)) {
    datasources=c(datasources,"intake_pc_FAO_WHO_UNU1985_F_old")
  }
  if (any(c("intake_pc_FAO_WHO_UNU1985_M_underaged") %in% indicators)) {
    datasources=c(datasources,"intake_pc_FAO_WHO_UNU1985_M_underaged")
  }
  if (any(c("intake_pc_FAO_WHO_UNU1985_F_underaged") %in% indicators)) {
    datasources=c(datasources,"intake_pc_FAO_WHO_UNU1985_F_underaged")
  }
  if (any(c("intake_pc_FAO_WHO_UNU1985_M_working") %in% indicators)) {
    datasources=c(datasources,"intake_pc_FAO_WHO_UNU1985_M_working")
  }
  if (any(c("intake_pc_FAO_WHO_UNU1985_F_working") %in% indicators)) {
    datasources=c(datasources,"intake_pc_FAO_WHO_UNU1985_F_working")
  }
  if (any(c("intake_pc_standardized") %in% indicators)) {
    datasources=c(datasources,"intake_pc_standardized")
  }
  if (any(c("intake_pc_standardized_BMI_FAO_WHO_UNU1985") %in% indicators)) {
    datasources=c(datasources,"intake_pc_standardized_BMI_FAO_WHO_UNU1985")
  }
  if (any(c("intake_pc_standardized_BMI_FAO_WHO_UNU1985_M_working") %in% indicators)) {
    datasources=c(datasources,"intake_pc_standardized_BMI_FAO_WHO_UNU1985_M_working")
  }
  if (any(c("intake_pc_standardized_BMI_FAO_WHO_UNU1985_F_working") %in% indicators)) {
    datasources=c(datasources,"intake_pc_standardized_BMI_FAO_WHO_UNU1985_F_working")
  }
  if (any(c("intake_pc_standardized_BMI_FAO_WHO_UNU1985_M_underaged") %in% indicators)) {
    datasources=c(datasources,"intake_pc_standardized_BMI_FAO_WHO_UNU1985_M_underaged")
  }
  if (any(c("intake_pc_standardized_BMI_FAO_WHO_UNU1985_F_underaged") %in% indicators)) {
    datasources=c(datasources,"intake_pc_standardized_BMI_FAO_WHO_UNU1985_F_underaged")
  }
  if (any(c("intake_pc_standardized_BMI_FAO_WHO_UNU1985_M_old") %in% indicators)) {
    datasources=c(datasources,"intake_pc_standardized_BMI_FAO_WHO_UNU1985_M_old")
  }
  if (any(c("intake_pc_standardized_BMI_FAO_WHO_UNU1985_F_old") %in% indicators)) {
    datasources=c(datasources,"intake_pc_standardized_BMI_FAO_WHO_UNU1985_F_old")
  }
  if (any(c("bodyheight_M","bodyheight_F") %in% indicators)) {
    datasources=c(datasources,"bodyheight")
  }

  if (any(c("working_M_BMI_18-5","retired_M_BMI_18-5","working_M_BMI_18-5_20","retired_M_BMI_18-5_20", "working_M_BMI_20_25",  
            "retired_M_BMI_20_25","working_M_BMI_25_30","retired_M_BMI_25_30","working_M_BMI_30_35","retired_M_BMI_30_35",  
            "working_M_BMI_35_40","retired_M_BMI_35_40","working_M_BMI_40","retired_M_BMI_40","working_F_BMI_18-5",   
            "retired_F_BMI_18-5","working_F_BMI_18-5_20","retired_F_BMI_18-5_20","working_F_BMI_20_25","retired_F_BMI_20_25",  
            "working_F_BMI_25_30","retired_F_BMI_25_30","working_F_BMI_30_35","retired_F_BMI_30_35","working_F_BMI_35_40",  
            "retired_F_BMI_35_40","working_F_BMI_40","retired_F_BMI_40"
            )%in% indicators)) {
    datasources=c(datasources,"BMI_shr")
  }
  
  if (any(c(  "M_BMI_2sd","F_BMI_2sd","M_BMI_1sd_2sd",          
              "F_BMI_1sd_2sd","M_BMI_minus1sd_1sd","F_BMI_minus1sd_1sd",     
              "M_BMI_minus1sd_minus2sd","F_BMI_minus1sd_minus2sd","M_BMI_minus2sd",         
              "F_BMI_minus2sd"  
  )%in% indicators)) {
    datasources=c(datasources,"BMI_shr_underaged")
  }
  
  if (any(c(
    "intake_F_0--4","intake_F_10--14","intake_F_100+","intake_F_15--19","intake_F_20--24",
    "intake_F_25--29","intake_F_30--34","intake_F_35--39","intake_F_40--44","intake_F_45--49",
    "intake_F_5--9","intake_F_50--54","intake_F_55--59","intake_F_60--64","intake_F_65--69",
    "intake_F_70--74","intake_F_75--79","intake_F_80--84","intake_F_85--89","intake_F_90--94",
    "intake_F_95--99","intake_F_All","intake_M_0--4","intake_M_10--14","intake_M_100+",  
    "intake_M_15--19","intake_M_20--24","intake_M_25--29","intake_M_30--34","intake_M_35--39",
    "intake_M_40--44","intake_M_45--49","intake_M_5--9","intake_M_50--54","intake_M_55--59",
    "intake_M_60--64","intake_M_65--69","intake_M_70--74","intake_M_75--79","intake_M_80--84",
    "intake_M_85--89","intake_M_90--94","intake_M_95--99","intake_M_All"
  ) %in% indicators)) {
    datasources=c(datasources,"intake_demography")
  }
  
  if (any(c(
    "intake_standardized_F_0--4","intake_standardized_F_10--14","intake_standardized_F_100+","intake_standardized_F_15--19","intake_standardized_F_20--24",
    "intake_standardized_F_25--29","intake_standardized_F_30--34","intake_standardized_F_35--39","intake_standardized_F_40--44","intake_standardized_F_45--49",
    "intake_standardized_F_5--9","intake_standardized_F_50--54","intake_standardized_F_55--59","intake_standardized_F_60--64","intake_standardized_F_65--69",
    "intake_standardized_F_70--74","intake_standardized_F_75--79","intake_standardized_F_80--84","intake_standardized_F_85--89","intake_standardized_F_90--94",
    "intake_standardized_F_95--99","intake_standardized_F_All","intake_standardized_M_0--4","intake_standardized_M_10--14","intake_standardized_M_100+",  
    "intake_standardized_M_15--19","intake_standardized_M_20--24","intake_standardized_M_25--29","intake_standardized_M_30--34","intake_standardized_M_35--39",
    "intake_standardized_M_40--44","intake_standardized_M_45--49","intake_standardized_M_5--9","intake_standardized_M_50--54","intake_standardized_M_55--59",
    "intake_standardized_M_60--64","intake_standardized_M_65--69","intake_standardized_M_70--74","intake_standardized_M_75--79","intake_standardized_M_80--84",
    "intake_standardized_M_85--89","intake_standardized_M_90--94","intake_standardized_M_95--99","intake_standardized_M_All"
  ) %in% indicators)) {
    datasources=c(datasources,"intake_standardized_demography")
  }
  
  
  data<-calcOutput("CollectRegressionData",datasources=datasources,aggregate = FALSE)[,,indicators]
  #data<-calcOutput("CollectRegressionData",aggregate = FALSE)
  
  if(!all(indicators%in%getNames(data))){
    missing<-indicators[which(!indicators%in%getNames(data))]
    missing<-findset(missing)
    indicators<-c(missing,indicators[which(indicators%in%getNames(data))])
  } 
  
  data<-data[,,indicators]
  return(data)
}
