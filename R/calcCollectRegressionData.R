#' @title calcCollectRegressionData
#' @description collects regression data using un-converted raw data sources, and
#' crops the data that only joint years and countries are selected.
#'
#' @param datasources All datasources that shall be returned. Due to the cropping
#' of data which is not present in all datasources, reducing the number of datasources
#' will increase the number of observations.
#' @return List of magpie objects with results on country level, weight on country level,
#' unit and description.
#' @author Benjamin Leon Bodirsky, Eleonora Martinelli, Abhijeet Mishra, Xiaoxi Wang
#' @examples
#' \dontrun{
#' calcOutput("CollectRegressionData", aggregate = F)
#' }
#'

#' @importFrom countrycode countrycode
#' @importFrom magpiesets findset
#' @importFrom mrfaocore toolFAOcombine
#' @importFrom stats quantile
#' @importFrom stats runif
#' @import magclass
#' @import madrat
#' @export

calcCollectRegressionData <- function(datasources) { # nolint: cyclocomp_linter

  combined <- list()

  if ("wooddemand" %in% datasources) {
    wooddemand <- collapseNames(calcOutput("TimberDemand", aggregate = FALSE)[, , "domestic_supply"])
    combined$wooddemand <- wooddemand
  }

  if ("SelfSuff" %in% datasources) {
    selfSuffW <- calcOutput("TradeSelfSuff", convert = FALSE)[, , "wood"]
    selfSuffWf <- calcOutput("TradeSelfSuff", convert = FALSE)[, , "woodfuel"]
    selfSuff <- mbind(selfSuffW, selfSuffWf)
    selfSuff <- selfSuff[, sort(getYears(selfSuff)), ]
    selfSuff <- selfSuff[which(!is.na(dimnames(selfSuff)[[1]])), , ]
    getNames(selfSuff) <- paste0("ss_", getNames(selfSuff))
    combined$SelfSuff <- selfSuff
  }

  if ("timber_demand" %in% datasources) {
    timberDemand <- collapseNames(calcOutput("TimberDemand", aggregate = FALSE)[, , "domestic_supply"])
    combined$forestry <- timberDemand
  }

  if ("forest_area" %in% datasources) {
    faLui <- calcOutput("LanduseInitialisation", aggregate = FALSE)

    forestArea <- dimSums(faLui[, , c("forestry", "primforest", "secdforest")], dim = 3)
    forestArea <- setNames(forestArea, c("forest_area"))
    combined$forest_area <- forestArea
  }

  if ("urbanization_WDI" %in% datasources) {
    urban <- setNames(readSource("WDI", subtype = "SP.URB.TOTL.IN.ZS", convert = FALSE) / 100, "urban")
    urban <- urban[, sort(getYears(urban)), ]
    getCells(urban) <- countrycode(getCells(urban), "iso2c", "iso3c")
    urban <- urban[which(!is.na(dimnames(urban)[[1]])), , ]
    combined$urban <- urban
  }

  if ("population_WDI" %in% datasources) {

    pop <- setNames(readSource("WDI", subtype = "SP.POP.TOTL", convert = TRUE), "pop")
    pop <- pop[, sort(getYears(pop)), ]
    pop <- pop[which(!is.na(dimnames(pop)[[1]])), , ]
    combined$pop <- pop
  }

  if ("population_WDI" %in% datasources) {

    pop <- setNames(readSource("WDI", subtype = "SP.POP.TOTL", convert = TRUE), "pop")
    pop <- pop[, sort(getYears(pop)), ]
    pop <- pop[which(!is.na(dimnames(pop)[[1]])), , ]
    combined$pop <- pop
  }

  if ("gdp" %in% datasources) {
    gdpPC <- readSource("James", convert = FALSE)[, , "IHME_USD05_PPP_pc"]
    gdpPC <- gdpPC[, , "IHME_USD05_PPP_pc"]
    combined$gdp_pc <- gdpPC
  }

  if ("bodyheight" %in% datasources) {
    bodyheightWrongyears <- readSource("NCDrisc", subtype = "height", convert = FALSE)
    bodyheight <- new.magpie(cells_and_regions = getItems(bodyheightWrongyears, dim = 1.1),
                             years = (1961 + 17):(max(getYears(bodyheightWrongyears, as.integer = TRUE)) + 17),
                             names = getNames(bodyheightWrongyears))
    for (years in getYears(bodyheight, as.integer = TRUE)) {
      bodyheight[, years, ] <- setYears(bodyheightWrongyears[, years - 17, ], years)
    }
    bodyheight <- setNames(bodyheight, paste0("bodyheight_", getNames(bodyheight)))
    combined$bodyheight <- bodyheight
  }

  if ("intake_pc_schofield" %in% datasources) {
    intake <- calcOutput("Intake2", convert = FALSE, modelinput = TRUE, standardize = FALSE,
                         method = "schofield", aggregate = FALSE)
    intake <- setNames(collapseNames(intake), "intake_pc_schofield")
    combined$intake <- intake
  }

  if ("intake_pc_FAO_WHO_UNU1985" %in% datasources) {
    intake <- calcOutput("Intake2", convert = FALSE, modelinput = TRUE, standardize = FALSE,
                         method = "FAO_WHO_UNU1985", aggregate = FALSE)
    intake <- setNames(collapseNames(intake), "intake_pc_FAO_WHO_UNU1985")
    combined$intake <- intake
  }

  if ("intake_pc_Froehle" %in% datasources) {
    intake <- calcOutput("Intake2", convert = FALSE, modelinput = TRUE, standardize = FALSE,
                         method = "Froehle", aggregate = FALSE)
    intake <- setNames(collapseNames(intake), "intake_pc_Froehle")
    combined$intake <- intake
  }

  if ("intake_demography" %in% datasources) {
    intake <- calcOutput("Intake", convert = FALSE, modelinput = FALSE, standardize = FALSE,
                         method = "schofield", aggregate = FALSE)
    intake <- collapseNames(intake[, , "SSP2"][, , c("F", "M")])
    getNames(intake) <- paste0("intake_", sub(x = getNames(intake), pattern = "\\.", replacement = "_"))
    getSets(intake) <- c("region", "year", "intake")
    combined$intake_demography <- intake
  }

  if ("intake_standardized_demography" %in% datasources) {
    intake <- calcOutput("Intake", convert = FALSE, modelinput = FALSE, standardize = "BMI",
                         method = "Froehle", aggregate = FALSE)
    intake <- collapseNames(intake[, , "SSP2"][, , c("F", "M")])
    getNames(intake) <- paste0("intake_standardized_", sub(x = getNames(intake), pattern = "\\.", replacement = "_"))
    getSets(intake) <- c("region", "year", "intake_standardized")
    combined$intake_standardized <- intake
  }

  if ("intake_pc_standardized_BMI_FAO_WHO_UNU1985" %in% datasources) {
    intake <- calcOutput("Intake", convert = FALSE, modelinput = TRUE, standardize = "BMI",
                         method = "FAO_WHO_UNU1985", aggregate = FALSE)
    intake <- setNames(collapseNames(intake[, , "SSP2"]), "intake_pc_standardized_BMI_FAO_WHO_UNU1985")
    intake <- time_interpolate(intake, interpolated_year = paste0("y", 1965:2010),
                               integrate_interpolated_years = FALSE)
    combined$intake_BMI <- intake
  }

  if ("physical_inactivity" %in% datasources) {
    inactive1 <- readSource("WHO", subtype = "physical_inactivity_adults", convert = FALSE)
    inactive2 <- readSource("WHO", subtype = "physical_inactivity_underaged", convert = FALSE)
    getNames(inactive1) <- paste0("inactivity_adults_", getNames(inactive1))
    getNames(inactive2) <- paste0("inactivity_underaged_", getNames(inactive2))
    combined$intake_BMI <- mbind(inactive1, inactive2)
  }

  if ("batten_last_20yrs" %in% datasources) {
    foodSupplyCrop <- readSource("FAO", subtype = "FSCrop", convert = FALSE)
    foodSupplyLive <- readSource("FAO", subtype = "FSLive", convert = FALSE)
    foodSupply <- toolFAOcombine(foodSupplyCrop, foodSupplyLive, combine = "Item")
    relationmatrix <-  toolGetMapping("FAOitems.rda", type = "sectoral", where = "moinput")
    relationmatrix <- relationmatrix[, which(names(relationmatrix) %in% c("FoodBalanceItem", "k"))]
    relationmatrix <- relationmatrix[-which(duplicated(relationmatrix[, 1]) == TRUE), ]

    vcat(2, "removing strange values for montenegro in 2004, 2005 and luxemburg in 1990:1999")
    foodSupply["MNE", c("y2004", "y2005"), ] <- 0
    foodSupply["LUX", 1961:1999, ] <- 0

    foodSupply <- collapseNames(foodSupply[, , "food_supply_kcal/cap/day"])
    kcal <- toolAggregate(x = foodSupply,
                          rel = relationmatrix,
                          dim = 3.1,
                          from = "FoodBalanceItem",
                          to = "k",
                          partrel = TRUE)

    kcal <- add_columns(kcal, addnm = c("brans", "scp"))
    kcal[, , c("brans", "scp")] <- 0

    kcal <- kcal[, , "remaining", invert = TRUE]

    missing <- dimSums(kcal, dim = 3, na.rm = TRUE) # missing values
    missing[missing == 0] <- NA
    missing[!is.na(missing)] <- 1
    kcal[is.na(kcal)] <- 0
    kcal <- kcal * missing

    batten <- dimSums(kcal[, , c(
      "fish", "livst_chick", "livst_egg",
      "livst_milk", "livst_pig", "livst_rum",
      "oils",
      "puls_pro", "soybean", "groundnut"
    )], dim = 3)

    kcal <- batten[, (1961 + 19):max(getYears(batten, as.integer = TRUE)), ] * NA
    for (year_x in (1961 + 19):max(getYears(foodSupplyCrop, as.integer = TRUE))) {
      kcal[, year_x, ] <- dimSums(batten[, (year_x - 19):(year_x - 5), ], dim = 2) / 15
    }
    combined$kcal_last_20yrs <- setNames(collapseNames(kcal), "batten_last_20yrs")
  }

  if ("BMI_shr"  %in% datasources) {
    x <- readSource("NCDrisc", subtype = "BMI_shr", convert = FALSE)
    mapping <- toolGetMapping(type = "sectoral", name = "NCDriscBMIshr2Lutz.csv")
    lutz <- calcOutput("Demography", education = FALSE, aggregate = FALSE)
    lutz <- collapseNames(time_interpolate(lutz[getItems(x, dim = 1.1), , "SSP2"], interpolated_year = getYears(x),
                                           integrate_interpolated_years = FALSE))
    lutz2 <- toolAggregate(lutz[, , c(mapping$lutz)], rel = mapping, from = "lutz", to = "NCDrisc", dim = 3.2)
    getSets(lutz2) <- c("country", "year", "sex", "age")
    lutz2 <- dimOrder(lutz2, perm = c(2, 1))

    mapping <- toolGetMapping(type = "sectoral", name = "NCDriscBMIshr2agegroups.csv")
    twogroups <- toolAggregate(x, rel = mapping, weight = lutz2, from = "NCDrisc", to = "agegroups", dim = 3.1)

    dimnames(twogroups)[[3]] <- gsub(pattern = "\\.", replacement = "_", dimnames(twogroups)[[3]])

    combined$BMI_shr <- twogroups
  }

  if ("BMI_shr_underaged"  %in% datasources) {
    x <- readSource("NCDrisc", subtype = "BMI_shr_underaged", convert = FALSE)
    x <- x[, , c("age5", "age6", "age7", "age8", "age9", "age10", "age11", "age12", "age13", "age14")]
    x <- dimSums(x, dim = 3.1) / fulldim(x)[[1]][[3]] # nolint: undesirable_function_linter
    dimnames(x)[[3]] <- gsub(pattern = "\\.", replacement = "_", dimnames(x)[[3]])
    getSets(x) <- c("iso", "year", "group")
    combined$BMI_shr_underaged <- x
  }

  if ("BMI" %in% datasources) {

    x <- readSource("NCDrisc", subtype = "BMI", convert = FALSE)
    mapping <- toolGetMapping(type = "sectoral", name = "NCDrisc2Lutz.csv")
    bmi <- new.magpie(cells_and_regions = getItems(x, dim = 1.1), years = getYears(x),
                      names = c(paste0(unique(mapping$lutz), ".M"), paste0(unique(mapping$lutz), ".F")))
    for (i in getNames(bmi, dim = 1)) {
      item <- mapping$NCDrisc[mapping$lutz == i]
      bmi[, , i] <- dimSums(x[, , item], dim = "age") / length(item)
    }
    getNames(bmi) <- paste0("BMI_", sub(x = getNames(bmi), pattern = "\\.", replacement = "_"))
    getSets(bmi) <- c("region", "year", "BMI")
    combined$BMI <- bmi
  }

  if ("kcal" %in% datasources) {
    foodSupplyCrop <- readSource("FAO", subtype = "FSCrop", convert = FALSE)
    foodSupplyLive <- readSource("FAO", subtype = "FSLive", convert = FALSE)
    foodSupply <- toolFAOcombine(foodSupplyCrop, foodSupplyLive, combine = "Item")
    relationmatrix <-  toolGetMapping("FAOitems.rda", type = "sectoral", where = "moinput")
    relationmatrix <- relationmatrix[, which(names(relationmatrix) %in% c("FoodBalanceItem", "k"))]
    relationmatrix <- relationmatrix[-which(duplicated(relationmatrix[, 1]) == TRUE), ]

    vcat(2, "removing strange values for montenegro in 2004, 2005 and luxemburg in 1990:1999")
    foodSupply["MNE", c("y2004", "y2005"), ] <- 0
    foodSupply["LUX", 1961:1999, ] <- 0

    foodSupply <- collapseNames(foodSupply[, , "food_supply_kcal/cap/day"])
    kcal <- toolAggregate(x = foodSupply,
                          rel = relationmatrix,
                          dim = 3.1,
                          from = "FoodBalanceItem",
                          to = "k",
                          partrel = TRUE)

    kcal <- add_columns(kcal, addnm = c("brans", "scp"))
    kcal[, , c("brans", "scp")] <- 0

    kcal <- kcal[, , "remaining", invert = TRUE]


    missing <- dimSums(kcal, dim = 3, na.rm = TRUE) # missing values
    missing[missing == 0] <- NA
    missing[!is.na(missing)] <- 1
    kcal[is.na(kcal)] <- 0
    kcal <- kcal * missing
    combined$kcal <- kcal
  }

  if ("demographics" %in% datasources) {
    lutz <- readSource("Lutz2014", convert = FALSE)
    lutzSSP2 <- time_interpolate(lutz[, , "SSP2"],
                                 paste0("y", 1970:2011),
                                 integrate_interpolated_years = FALSE,
                                 extrapolation_type = "linear")
    totalBoth <- lutzSSP2[, , "Total"][, , "Both"][, , "All"]
    totalFem <- lutzSSP2[, , "Total"][, , "Female"][, , "All"]
    totalBothNoKids <- dimSums(lutzSSP2[, , c("Under 15", "Total"), invert = TRUE][, , "Both"][, , "All"], dim = 3.4)
    femaleNoKids <- dimSums(lutzSSP2[, , c("Under 15", "Total"), invert = TRUE][, , "Both"][, , "All"], dim = 3.4)

    ### gender ###
    femShare <- mbind(setNames(dimSums(totalFem / totalBoth, dim = 3), "femaleShare"))

    ### education ###
    education <- mbind(
      setNames(dimSums(lutzSSP2[, , "Post Secondary"][, , "Both"][, , "All"]
                       / totalBothNoKids,
                       dim = 3),
               "college"),
      setNames(dimSums(lutzSSP2[, , "Post Secondary"][, , "Female"][, , "All"]
                       / femaleNoKids,
                       dim = 3),
               "femcollege"),
      setNames(dimSums(lutzSSP2[, , c("Incomplete Primary",
                                      "No Education",
                                      "Primary")][, , "Both"][, , "All"]
                       / totalBothNoKids,
                       dim = 3),
               "low_education")
    )

    if (any(education < 0, na.rm = TRUE)) {
      stop("education share smaller 0")
    }
    if (any(education > 1, na.rm = TRUE)) {
      stop("education share larger 1")
    }

    ### age ###
    age <- mbind(
      setNames(dimSums(lutzSSP2[, , "Total"][, , "Both"][, , c("0--4", "5--9", "10--14")]
                       / totalBoth, dim = 3), "below15"),
      setNames(dimSums(lutzSSP2[, , "Total"][, , "Both"][, , c("15--19", "20--24", "25--29", "30--34", "35--39",
                                                               "40--44", "45--49", "50--54", "55--59", "60--64")]
                       / totalBoth, dim = 3), "15-64"),
      setNames(dimSums(lutzSSP2[, , "Total"][, , "Both"][, , c("65--69", "70--74", "75--79", "80--84",
                                                               "85--89", "90--94", "95--99", "100+")]
                       / totalBoth, dim = 3), "above64")
    )
    demographics <- mbind(education, age, femShare)
    combined$demographics <- demographics
  }

  if ("food_price" %in% datasources) {
    price <- collapseNames(calcOutput("PriceAgriculture", datasource = "WBGEM", aggregate = FALSE))

    dimnames(price)[[1]] <- "DEU"
    price <- toolCountryFill(price, fill = 0)
    for (i in getItems(price, dim = 1.1)) {
      price[i, , ] <- as.matrix(price["DEU", , ])
    }
    getNames(price) <- paste("price", getNames(price), sep = "_")
    combined$food_price <- price
  }

  if ("climate" %in% datasources) {
    cz <- readSource("Koeppen", convert = FALSE) # klimazone
    cz <- dimSums(cz[, , c("kg_p_af", "kg_p_aw", "kg_p_bs", "kg_p_cf", "kg_p_df", "kg_p_e")], dim = 3)
    cz <- setNames(cz, "climate")
    cz <- add_columns(setYears(cz, "y2010"), setdiff(getYears(combined[[1]]), "y2010"), dim = 2.1)
    cz[, , ] <- setYears(cz[, "y2010", ], NULL)
    getYears(combined[[1]])
    combined$climate <- cz
  }

  if (any(grepl("crossvalid", datasources))) {
    code <- datasources[grep("crossvalid", datasources)]
    code2 <- strsplit(code, "_")
    k <- as.integer(substring(code2[[1]][3], 2))
    # format: crossvalid_seedX_kY
    # X is the random seed,
    # Y is the number of drawings

    countries <- toolGetMapping("iso_country.csv", where = "moinput")
    years <- paste0("y", 1961:2020)
    sampleset <- new.magpie(cells_and_regions = countries$x, years = years, names = code)
    set.seed(42)
    sampleset[, , ] <- round(runif(length(sampleset)) * k + 0.5)

    combined$crossvalid <- sampleset
  }


  mbindCommonDimensions <- function(magpielist) {
    if (!is.list(magpielist)) {
      magpielist <- list(magpielist)
    }
    if (length(magpielist) == 2) {
      a <- magpielist[[1]]
      b <- magpielist[[2]]
      ab <- c(getItems(a, dim = 1.1), getItems(b, dim = 1.1))
      abRegions <- ab[duplicated(ab)]

      ab <- c(getYears(a), getYears(b))
      abTime <- ab[duplicated(ab)]

      ab <- list(mbind(a[abRegions, abTime, ], b[abRegions, abTime, ]))
      mbindCommonDimensions(ab)
    } else if (length(magpielist) > 2) {
      ab <- mbindCommonDimensions(list(magpielist[[1]], magpielist[[2]]))
      ab <- append(list(ab), magpielist[3:length(magpielist)])
      mbindCommonDimensions(ab)
    } else if (length(magpielist) == 0) {
      stop("empty list")
    } else {
      return(magpielist[[1]])
    }
  }

  out <- mbindCommonDimensions(combined)

  ### aggregation weights


  pop <- setNames(readSource("WDI", subtype = "SP.POP.TOTL", convert = TRUE), "weight")

  out <- mbindCommonDimensions(list(out, pop))
  weight <- x <- out
  weight[, , ] <- setNames(out[, , "weight"], NULL)
  if ("pop" %in% getNames(weight)) {
    weight[, , "pop"] <- 0
  }


  return(list(x = x,
              weight =  weight,
              unit = "share of population, per-capita income or per-capita consumption",
              description = "Merged dataset containing raw data for regression",
              min = 0,
              na_warning = FALSE,
              isocountries = FALSE)
  )
}
