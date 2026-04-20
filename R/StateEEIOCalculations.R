# StateEEIOCalculations.R
library(reshape2)
library(stringr)
library(useeior)

## Match calculation results to either import, export or instate
purpose <- list()
purpose$export <- c("Gsw", "Gsrf", "Gsri")
purpose$import <- c("Gri", "Grf", "Gmi", "Gmf")
purpose$instate <- c("Gs", "Gh")

calculateStateEmissions <- function(model) {
  # Define regions SoI = s; RoUS = r
  s <- model$specs$ModelRegionAcronyms[1]
  r <- model$specs$ModelRegionAcronyms[2]

  statecoms <- model$Commodities$Code_Loc[1:73]
  rouscoms <- model$Commodities$Code_Loc[74:146]



  # Get demand vectors including SoI consumption of SoI and RoUS coms, use it as starting place for regionalized final demand vectors
  y_d_c <- useeior:::prepareDemandVectorForStandardResults(model,
                                              demand = "Consumption",
                                              location = s,
                                              use_domestic_requirements = TRUE)
  y_rous_c <- useeior:::prepareDemandVectorForStandardResults(model,
                                              demand = "Consumption",
                                              location = r,
                                              use_domestic_requirements = TRUE)
  y_m <- useeior:::prepareImportConsumptionDemand(model, location = s)
  y <- y_d_c + y_m
  # Soi final use of RoUS coms
  y_rs <- y_d_c
  y_rs[grepl(s, rownames(y_rs))] <- 0
  # Soi final use of SoI coms
  y_ss <- y_d_c
  y_ss[grepl(r, rownames(y_ss))] <- 0
  # Soi exports to RoW
  y_e <- getStateUsebyType(model, type = "Export",
                           domestic = TRUE, RoUS = FALSE)
  # RoUS final use of SoI
  y_sr <- y_rous_c
  y_sr[grepl(r, rownames(y_sr))] <- 0
  # RoUS use of RoUS coms
  y_rr <- y_rous_c
  y_rr[grepl(s, rownames(y_rr))] <- 0

  ### Prepare all needed A and L matrices
  ## Get direct requirements for RoUS goods to make SoI goods
  A_rs <- model$A_d
  # Set inputs from SoI to SoI to zero in A matrix
  A_rs[grepl(s, rownames(A_rs)), grepl(s, colnames(A_rs))] <- 0
  # Set all inputs to RoUS to zero in A matrix
  A_rs[, grepl(r, colnames(A_rs))] <- 0
  # L_rs <- calculateLeontiefInverse(A_rs)

  # Get direct requirements for SoI goods to make SoI goods
  A_ss <- model$A_d
  # Set inputs from SoI to SoI to zero in A matrix
  A_ss[grepl(r, rownames(A_ss)), grepl(s, colnames(A_ss))] <- 0
  # Set all inputs to RoUS to zero in A matrix
  A_ss[, grepl(r, colnames(A_ss))] <- 0
  L_ss <- useeior:::calculateLeontiefInverse(A_ss)

  ## Requirement for SoI goods by RouS
  A_sr <- model$A_d
  # Set inputs from RoUS to RoUS to zero in A matrix
  A_sr[grepl(r, rownames(A_sr)), grepl(r, colnames(A_sr))] <- 0
  # Set all inputs to SoI to zero in A matrix
  A_sr[, grepl(s, colnames(A_sr))] <- 0

  ## Requirements for RoUS goods by RouS
  A_rr <- model$A_d
  # Set inputs from SoI to RoUS to zero in A matrix
  A_rr[grepl(s, rownames(A_rr)), grepl(r, colnames(A_rr))] <- 0
  # Set all inputs to SoI to zero in A matrix
  A_rr[, grepl(s, colnames(A_rr))] <- 0
  L_rr <- useeior:::calculateLeontiefInverse(A_rr)

  ## Regionalize B Matrices
  B_s <- model$B
  B_s[, grepl(r, colnames(B_s))] <- 0

  B_r <- model$B
  B_r[, grepl(s, colnames(B_r))] <- 0

  # Get model C
  C <- model$C

  ## Get A M matrices
  A_m <- model$A_m
  M_m <- model$M_m
  colnames(M_m) <- gsub("/.*", "/RoW", colnames(M_m))

  ## Define helper function to rename columns
  renamecols <- function(df, ref_df) {
    colnames(df) <- colnames(ref_df)
    return(df)
  }

  ## Define emissions calculation functions (have access to parent scope variables)
  ## Calculate in-state emissions from SoI final consumption
  calculateGs <- function() {
    return(B_s %*% L_ss %*% diag(as.vector(y_ss)))
  }

  ## Emissions from use of RoUS as intermediate inputs to SoI final demand
  calculateGri <- function() {
    i <- A_rs %*% L_ss %*% y_ss
    return((B_r %*% L_rr) %*% diag(as.vector(i), nrow(i)))
  }

  ## Emissions from use of RoUS as final products to SoI
  calculateGrf <- function() {
    return(B_r %*% L_rr %*% diag(as.vector(y_rs)))
  }

  ## International import emissions from final consumption of international
  calculateGmf <- function() {
    G <- M_m %*% diag(as.vector(y_m))
    ncoms <- ncol(G) / 2
    G <- G[, 1:ncoms]
    return(G)
  }

  calculateGmi <- function() {
    i <- A_m %*% L_ss %*% y_ss
    G <- M_m %*% diag(as.vector(i), nrow(i))
    ncoms <- ncol(G) / 2
    G <- G[, 1:ncoms]
    return(G)
  }

  ## Calculate SoI emissions in exports to RoW
  calculateGsw <- function() {
    return(B_s %*% L_ss %*% diag(as.vector(y_e)))
  }

  ## Calculate use of SoI as intermediate inputs to RoUS final products
  calculateGsri <- function() {
    i <- A_sr %*% L_rr %*% y_rr
    return((B_s %*% L_ss) %*% diag(as.vector(i), nrow(i)))
  }

  ## RoUS final use of SoI products
  calculateGsrf <- function() {
    return(B_s %*% L_ss %*% diag(as.vector(y_sr)))
  }

## Calculate SoI emissions including for instate, export and import 
  g <- list()
  g$Gs <- calculateGs()
  g$Gsw <- calculateGsw()
  g$Gsri <- calculateGsri()
  g$Gsrf <- calculateGsrf()
  #Remove unused RoUS sectors
  g <- lapply(g, function(m) {
    return(m[,1:73])
  } )
  g <- lapply(g, function(m) {
    colnames(m) <- statecoms
    return(m)
  })
  
  #Imports from RoUS
  gr <- list()
  gr$Gri <- calculateGri()
  gr$Grf <- calculateGrf()
  #Remove unused SoI sectors
  gr <- lapply(gr, function(m) {
    return(m[,74:146])
  } )
  gr <- lapply(gr, function(m) {
    colnames(m) <- rouscoms
    return(m)
  })
  #Merge back into g
  g <- c(g,gr)
  rm(gr)

  i <- list()
  i$Gmi <- calculateGmi()
  i$Gmf <- calculateGmf()
  i <- lapply(i, function(m) {
      colnames(m) <- colnames(M_m[,1:73])
      return(m)})
  g <- c(g,i)
  rm(i)
  h <- lapply(names(g), function(name) {
      mat <- C %*% g[[name]]
      col_names <- colnames(g[[name]])  # Get column names from original matrix before C multiplication
    
      # For Gmi and Gmf (import matrices), use full commodity name as Sector
      if (name %in% c("Gmi", "Gmf")) {
          Sector <- sub("/RoW", "", col_names)  # Remove the /RoW suffix
          Region <- "RoW"
      } else {
          # For other matrices, extract sector (before "/") and region (after "/")
          Sector <- sub("/.*", "", col_names)
          Region <- sub(".*/", "", col_names)
      }
      
      # Determine purpose based on which purpose list the name belongs to
      purpose_val <- NA
      if (name %in% purpose$export) {
          purpose_val <- "export"
      } else if (name %in% purpose$import) {
          purpose_val <- "import"
      } else if (name %in% purpose$instate) {
          purpose_val <- "instate"
      }

      # Create data frame with proper rows
      df <- data.frame(
          Sector = Sector,
          Region = Region,
          use_type = ifelse(grepl("i$", name), "intermediate", "final"),
          source = name,
          purpose = purpose_val,
          emissions = as.numeric(t(mat))
      )
      
      return(df)
      }
  )

  ## Add household emissions
  h$Gh <- data.frame((h[[1]][0,]))
  h$Gh[1,"emissions"] <- useeior:::calculateHouseholdEmissions(model, y, s, characterized=TRUE)[1]
  h$Gh[1,"use_type"] <- "final"
  h$Gh[1,"purpose"] <- "household"
  h$Gh[1,"Sector"] <- "F010"
  h$Gh[1,"Region"] <- s
  h$Gh[1,"source"] <- "Gh"

  H <- do.call(rbind, h)
  return(H)
}




## Primary State CBE function.
## Returns by default a vector with GHG in CO2e totals by sector (rows)
calculateStateCBE <- function(model, CO2e = TRUE, perspective = "FINAL",
                              demand = "Consumption", domestic = FALSE, RoUS = FALSE,
                              household_emissions = TRUE, show_RoW = TRUE) {
  loc <- getLocation(RoUS, model)
  r <- useeior::calculateEEIOModel(model,
    perspective = perspective,
    demand = demand,
    location = loc,
    use_domestic_requirements = domestic,
    household_emissions = household_emissions,
    show_RoW = show_RoW
  )
  # Note this function requires a model with only a single indicator
  if (CO2e) {
    if (perspective == "DIRECT") {
      r <- r$H_r
    } else {
      r <- r$H_l
    }
  } else {
    if (perspective == "DIRECT") {
      r <- r$G_r
    } else {
      r <- r$G_l
    }
  }
  return(r)
}

## Returns a vector of demand in dollars by type with sectors as rows
#' @param type, str, "Household", "Federal Government", "State Government", "Investment", "final", or "intermediate"
getStateUsebyType <- function(model, type = "final", domestic = FALSE, RoUS = FALSE) {
  opt <- c("Household", "Federal Government", "State Government", "Government", "Investment", "Export", "Import", "ChangeInventories", "final", "intermediate")
  if (!type %in% opt) {
    stop(paste0("'type' options are ", paste(opt, collapse = ", ")))
  }
  loc <- getLocation(RoUS, model)
  if (type == "final") {
    code_loc <- model$FinalDemandMeta[endsWith(model$FinalDemandMeta$Code_Loc, loc), ][["Code_Loc"]]
  } else if (type == "intermediate") {
    code_loc <- model$Industries$Code_Loc[endsWith(model$Industries$Code_Loc, loc)]
  } else if (type == "State Government") {
    code_loc <- model$FinalDemandMeta[model$FinalDemandMeta$Group == "Government" &
      endsWith(model$FinalDemandMeta$Code_Loc, loc) &
      startsWith(model$FinalDemandMeta$Code, "F10"), ][["Code_Loc"]]
  } else if (type == "Federal Government") {
    code_loc <- model$FinalDemandMeta[model$FinalDemandMeta$Group == "Government" &
      endsWith(model$FinalDemandMeta$Code_Loc, loc) &
      !startsWith(model$FinalDemandMeta$Code, "F10"), ][["Code_Loc"]]
  } else {
    code_loc <- model$FinalDemandMeta[model$FinalDemandMeta$Group == type &
      endsWith(model$FinalDemandMeta$Code_Loc, loc), ][["Code_Loc"]]
  }
  if (domestic) {
    U <- model$U_d
  } else {
    U <- model$U
  }
  name <- type
  # Sum across demand columns, drop the Value Add rows
  usebytype <- as.matrix(rowSums(U[-which(startsWith(rownames(U), "V00")), code_loc, drop = FALSE]))
  colnames(usebytype) <- name

  return(usebytype)
}

# Adjusts a matrix of dollar values in a given IO year to the target price year
adjustDollarMatrixPriceYear <- function(model, matrix, io_year, price_year) {
  rho <- model$Rho[, toString(io_year)] / model$Rho[, toString(price_year)]
  matrix <- t(t(matrix) %*% diag(rho))
  return(matrix)
}


# Calculate demand by sector by type
calculateDemandByType <- function(model, price_year, RoUS = FALSE) {
  demand_by_type <- data.frame(sapply(c("Household", "Investment", "Federal Government", "State Government"),
    getStateUsebyType,
    model = model, RoUS = RoUS,
    simplify = FALSE, USE.NAMES = FALSE
  ))
  demand_by_type <- cbind(demand_by_type, Total = rowSums(demand_by_type))

  demand_by_type <- adjustDollarMatrixPriceYear(model, demand_by_type,
    io_year = model$specs$IOYear,
    price_year = price_year
  )
  total_demand_by_type <- as.matrix(colSums(demand_by_type))
  colnames(total_demand_by_type) <- "Demand"
  return(total_demand_by_type)
}

getDemandbyRegion <- function(model, region = "SoI") {
  state <- model$specs$ModelRegionAcronyms[[1]]
  # Get consumption amounts by region
  # The 3rd demand vector is Consumption complete
  # The 4th is Consumption domestic.
  # The difference should represent total consumption by each

  soi_comms <- grep(state, names(model$DemandVectors$vectors[[4]]))
  rous_comms <- grep("RoUS", names(model$DemandVectors$vectors[[4]]))

  soi_soi_finalconsumption <- model$DemandVectors$vectors[[4]][soi_comms]
  soi_rous_finalconsumption <- model$DemandVectors$vectors[[4]][rous_comms]

  soi_import_consumption <- model$DemandVectors$vectors[[3]] - model$DemandVectors$vectors[[4]]

  soi_import_consumption <- soi_import_consumption[soi_comms]
  if (region == "SoI") {
    d <- soi_soi_finalconsumption
  } else if (region == "RoUS") {
    d <- soi_rous_finalconsumption
  } else if (region == "ROW") {
    d <- soi_import_consumption
  }
  d <- as.matrix(d)
  return(d)
}

# Calculate demand by sector by region
calculateDemandByRegion <- function(model, price_year = NULL) {
  demand_by_region <- data.frame(sapply(c("SoI", "RoUS", "ROW"),
    getDemandbyRegion,
    model = model,
    simplify = FALSE, USE.NAMES = TRUE
  ))
  demand_by_region <- cbind(demand_by_region, Total = rowSums(demand_by_region))

  # if desired, adjust price type before summing
  if (is.null(price_year)) {
    price_year <- model$specs$IOYear
  }
  rho <- model$Rho[, toString(model$specs$IOYear)] / model$Rho[, toString(price_year)]
  demand_by_region <- demand_by_region * rho

  total_demand_by_region <- as.matrix(colSums(demand_by_region))
  colnames(total_demand_by_region) <- "Demand"
  return(total_demand_by_region)
}


reformatStatebyYearLongtoWide <- function(df, value.var) {
  colnames(df) <- c(value.var, "State", "Year")
  df_wide <- reshape(df,
    v.names = value.var,
    idvar = "State",
    timevar = "Year",
    direction = "wide"
  )
  row.names(df_wide) <- df_wide$State # Make row names the states
  df_wide <- df_wide[, -1] # Remove the column with state names
  colnames(df_wide) <- years
  df_wide <- df_wide[order(rownames(df_wide)), order(colnames(df_wide))]
  return(df_wide)
}

reformatWidetoLong <- function(df) {
  df <- melt(df, varnames = c("variable", "ID"))
  x <- do.call("rbind", (strsplit(as.character(df$ID), "-", fixed = TRUE)))
  if (ncol(x) == 2) {
    colnames(x) <- c("State", "Year")
  } else if (ncol(x) == 1) {
    colnames(x) <- c("Year")
  } else {
    stop("Error in reformatting")
  }
  df <- cbind(x, df)
  return(df)
}


convertStateResultFormatToStatebyYear <- function(df, value.var) {
  df_names <- t(data.frame(strsplit(row.names(df), "-")))
  df <- cbind(df, df_names)
  df <- reformatStatebyYearLongtoWide(df, value.var = value.var)
  return(df)
}


getLocation <- function(RoUS, model) {
  if (RoUS) {
    loc <- "RoUS"
  } else {
    loc <- model$specs$ModelRegionAcronyms[1]
  }
  return(loc)
}

aggregateStateResultMatrix <- function(model, matrix, region) {
  name <- colnames(matrix)
  matrix <- subset(matrix, endsWith(rownames(matrix), region))
  matrix <- useeior:::aggregateResultMatrixbyRow(matrix, "Sector", model$crosswalk)
  # reorder matrix rows
  rows <- subset(unique(model$crosswalk$BEA_Sector), unique(model$crosswalk$BEA_Sector) %in% rownames(matrix))
  matrix <- matrix[rows, , drop = FALSE]
  return(matrix)
}


subsetColumnsByString <- function(matrix, s) {
  m <- matrix[, stringr::str_detect(colnames(matrix), s), drop = FALSE]
  return(m)
}

# Returns the territorial inventory in Result format
# constructed from the model's Total by Sector amounts and indicator GWPs
getStateGHGI <- function(model, RoUS = FALSE) {
  loc <- getLocation(RoUS, model)
  fields <- c("Sector", "Flowable", "FlowAmount", "Location")
  GHGI <- useeior:::collapseTBS(model$TbS, model)[, fields]
  # filter out other regions (RoUS)
  GHGI <- GHGI[GHGI$Location == loc, ]
  GWPs <- data.frame("Flowable" = row.names(t(model$C)), t(model$C))
  GWPs$Flowable <- gsub("/.*", "", GWPs$Flowable)
  colnames(GWPs) <- c("Flowable", "Amount")
  GHGI <- merge(GHGI, GWPs, all.x = TRUE, )

  GHGI$`Greenhouse Gases` <- GHGI$FlowAmount * GHGI$Amount
  GHGI <- aggregate(`Greenhouse Gases` ~ Sector, GHGI, sum)
  # Merge in sectors in case some are missing
  comms_in_m <- list(Sector = unique(model$Commodities$Code))
  GHGI <- merge(GHGI, comms_in_m, all = TRUE)
  # Replace NA values with 0 for sectors that are in comms_in_m but not in GHGI
  GHGI$`Greenhouse Gases`[is.na(GHGI$`Greenhouse Gases`)] <- 0
  row.names(GHGI) <- apply(cbind(GHGI["Sector"], loc), 1, FUN = useeior:::joinStringswithSlashes)
  GHGI <- matrix(GHGI[, c("Greenhouse Gases")],
    dimnames = list(rownames(GHGI), c("Greenhouse Gases"))
  )
  ## TODO update order of sectors before returning

  return(GHGI)
}

# Combine two or more results vectors passed in a named vector; sets the ID equal
# to the name used in the named vector
# Returns a dataframe
combineResults <- function(dfNames) {
  df <- do.call(rbind, lapply(dfNames, function(x) {
    data.frame(ID = x, Sector = rownames(get(x)), get(x))
  }))
  df <- setNames(df, c("ID", "Sector", "Value"))
  y <- setNames(names(dfNames), dfNames)
  df$ID <- stringr::str_replace_all(df$ID, y)
  rownames(df) <- NULL

  return(df)
}

# Calculate CBE in exports to RoUS, exports to RoW, imports from RoUS, imports from ROW
# Add trade balance as exports - imports
calculateCBETradeBalance <- function(model) {
  # Get exports to RoW
  export_RoW <- getStateUsebyType(model, type = "Export")
  # Must be named vector to be used as model demand
  export_RoW <- setNames(export_RoW[, 1], row.names(export_RoW))
  SoI <- model$specs$ModelRegionAcronyms[[1]]

  # Emissions exported from SoI to RoUS
  E_x_RoUS <- calculateEEIOModel(model,
    perspective = "DIRECT", demand = "Consumption", location = "RoUS",
    use_domestic_requirements = TRUE, show_RoW = TRUE
  )[["H_r"]]
  E_x_RoUS <- E_x_RoUS[grepl(paste0("/*", SoI), row.names(E_x_RoUS)), , drop = FALSE]

  # Emissions exported from SoI to RoW (uses SoI export demand vector)
  E_x_RoW <- calculateEEIOModel(model,
    perspective = "DIRECT", demand = export_RoW, location = SoI,
    use_domestic_requirements = TRUE, show_RoW = TRUE
  )[["H_r"]]
  E_x_RoW_RoUS <- E_x_RoW[grepl("/RoUS", row.names(E_x_RoW)), , drop = FALSE] ## Add this in E_m_RoUS below
  E_x_RoW <- E_x_RoW[grepl(paste0("/*", SoI), row.names(E_x_RoW)), , drop = FALSE]

  # Emissions imported to SoI from RoUS
  E_m_RoUS <- calculateEEIOModel(model,
    perspective = "DIRECT", demand = "Consumption", location = SoI,
    use_domestic_requirements = TRUE, show_RoW = TRUE
  )[["H_r"]]
  E_m_RoUS <- E_m_RoUS[grepl("/RoUS", row.names(E_m_RoUS)), , drop = FALSE]
  E_m_RoUS <- E_m_RoUS + E_x_RoW_RoUS ## Add in SoI export emissions occurring in RoUS

  # Emissions imported to SoI from RoW
  # To get at 2nd and 3rd term, subtract domestic from Total
  E_m_RoW <- (calculateEEIOModel(model,
    perspective = "DIRECT", demand = "Consumption", location = SoI,
    use_domestic_requirements = FALSE, show_RoW = TRUE
  )[["H_r"]] -
    calculateEEIOModel(model,
      perspective = "DIRECT", demand = "Consumption", location = SoI,
      use_domestic_requirements = TRUE, show_RoW = TRUE
    )[["H_r"]])
  E_m_RoW <- E_m_RoW[grepl("/RoW", row.names(E_m_RoW)), , drop = FALSE]

  CBE_trade <- data.frame(cbind(-E_x_RoUS, -E_x_RoW, E_m_RoUS, E_m_RoW))
  colnames(CBE_trade) <- c("export_RoUS", "export_RoW", "import_RoUS", "import_RoW")
  rownames(CBE_trade) <- gsub("/.*", "", rownames(CBE_trade))
  CBE_trade$Balance <- rowSums(CBE_trade)
  return(CBE_trade)
}

# Calculate the share of household emissions for mobile and stationary applications
# Returns a matrix with 1 column and 2 rows (sum to 1)
calculateHouseholdShares <- function(model, indicator) {
  # extract the satellite spec based on the indicator name
  for (s in model$specs$SatelliteTable) {
    if (s$FullName == indicator) {
      sat_spec <- s
    }
  }
  code_loc <- model$specs$ModelRegionAcronyms[[1]]
  ### Regenerate tbs for households to obtain MetaSources
  tbs <- useeior:::generateTbSfromSatSpec(sat_spec, model)
  tbs <- useeior:::conformTbStoStandardSatTable(tbs)
  tbs <- useeior:::conformTbStoIOSchema(tbs, sat_spec, model, agg_metasources = FALSE)
  tbs$Flow <- apply(tbs[, c("Flowable", "Context", "Unit")], 1, FUN = useeior:::joinStringswithSlashes)

  df <- subset(tbs, (startsWith(tbs$Sector, "F010") &
    tbs$Location == code_loc))
  # unique(df$MetaSources)
  df <- df %>%
    mutate(
      Sector = case_when(
        grepl("transport", MetaSources) ~ "F010-Mobile",
        grepl("mobile", MetaSources) ~ "F010-Mobile",
        grepl("EPA_GHGI_T_A_97", MetaSources) ~ "F010-Mobile", # HFCs from Transportation
        grepl("EPA_GHGI_T_3_1", MetaSources) ~ "F010-Mobile", # 3-13, 3-14, and 3-15 for mobile emissions
        .default = "F010-Stationary"
      )
    )
  # reshape as matrix and convert to LCIA
  matrix <- reshape2::dcast(df, Flow ~ Sector, fun.aggregate = sum, value.var = "FlowAmount")
  rownames(matrix) <- matrix$Flow
  matrix$Flow <- NULL
  matrix[setdiff(rownames(model$B), rownames(matrix)), ] <- 0
  matrix <- matrix[rownames(model$B), ]
  lcia <- t(model$C %*% as.matrix(matrix))
  lcia <- sweep(lcia, 2, colSums(lcia), `/`)
  return(lcia)
}

# make into a matrix and transpose
matricizeandflip <- function(StateResult) {
  m <- t(as.matrix(colSums(StateResult, na.rm = TRUE)))
  return(m)
}

## Splits results in StateResult for household emissions into stationary and mobile emissions
applyHouseholdSharestoResult <- function(StateResult, shares) {
  # Get existing household result
  h_results <- StateResult[grep("F010", row.names(StateResult)), ]
  # Drop existing household result from StateResult
  StateResult <- StateResult[-grep("F010", row.names(StateResult)), ]
  # Create rows for stationary emissions by multiplying old result by stationary shares
  h1 <- h_results * shares["F010-Stationary", ]
  rownames(h1) <- stringr::str_replace_all(rownames(h1), pattern = "F010", replacement = "F010-Stationary")
  # Create rows for mobile emissions by multiplying old result by stationary shares
  h2 <- h_results * shares["F010-Mobile", ]
  rownames(h2) <- stringr::str_replace_all(rownames(h2), pattern = "F010", replacement = "F010-Mobile")
  # Add new stationary and mobile shares to the result
  StateResult <- rbind(StateResult, h1, h2)
  return(StateResult)
}
