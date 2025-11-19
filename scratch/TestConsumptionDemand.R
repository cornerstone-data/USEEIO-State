##Script to test if a modified consumption demand vector does not lead to overestimation of total commodity output q
## Gets totals uses from Domestic Use Transactions with Trade table both intermediate and with consumption demand
## Adds in International Trade Adjustment (ITA) to consumption demand and Export Residual
## Compares estimated q from these uses to the model q
## Runs for all states for a given year and logs any failures where estimated q exceeds model q by more than 1 million dollars

library(devtools)
load_all("../useeior")
library(logging)
basicConfig() 
addHandler(writeToFile, logger="test_q", file="test_consumption_against_q.log")

source("R/StateEEIOCalculations.R")

states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL",
            "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT",
            "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
            "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC")

year <- 2022

spec <- "v1.3-pecan"
modelstub <- paste0('EEIO',spec,"-")
modelspec <- paste0("State",modelstub,substr(year,3,4))  
specpath <- file.path("model_specs","all_states//")
configpaths <- file.path(paste0(specpath, modelspec, '.yml'))


c_all_states <- list()

manual <- TRUE  #Set to TRUE to do manual demand vector estimation

#Vars for manual loop
change_inventory <- TRUE
change_inventory_code <- "F030"
export <- TRUE
export_code <- "F040"


for (st in states) {

    m <- initializeModel(modelspec, configpaths)
    iolevel <- m$specs$BaseIOLevel
    modelname <- paste0(st, modelstub,substr(year,3,4)) 
    m$specs$ModelRegionAcronyms[1] <- paste0("US-",st)
    m <- loadIOData(m, configpaths)
    m$specs$Model <- modelname

## Manual model initialization and data loading to ensure clean model for each state
if (manual) {
  
  

    #Get Domestic Use Transactions with Trade table
    use_table <- m$DomesticUseTransactionswithTrade
    #names(use_table)


    ita_column <- "F051"


    #This is current code in useeior for consumption demand
    FD_columns <- unlist(sapply(list("HouseholdDemand", "InvestmentDemand", "GovernmentDemand"),
                                    getVectorOfCodes, ioschema = m$specs$BaseIOSchema, iolevel = iolevel))

    #FD <- use_table[["SoI2SoI"]][, c(FD_columns)] 


    #include RoUS uses

    #tables can be added together but first remove the Export Residual column from SoI2SoI

    use_soi2soi <- use_table[["SoI2SoI"]]

    # column not to add
    er_col <- c("ExportResidual")
    er <- use_soi2soi[,er_col]
    use_soi2soi <- use_soi2soi[ , !(names(use_soi2soi) %in% er_col)]
    use_all_soi_uses <- use_soi2soi + use_table[["SoI2RoUS"]]
    #now add export residual back in
    use_all_soi_uses <- cbind(use_all_soi_uses, er)

    FD <- use_all_soi_uses[, c(FD_columns)] 


    #FD_columns_w_ita <- c(FD_columns, ita_column)
    #FD_w_ita_ <- use_table[["SoI2SoI"]][, c(FD_columns_w_ita)] 


    #Following from procedure with production demand, for domestic model add in both ITA and Export residual
    FD_columns_w_er <- c(FD_columns, ita_column, "ExportResidual")
    if (change_inventory) {
      FD_columns_w_er <- c(FD_columns_w_er, change_inventory_code)
    }
    if (export) {
      FD_columns_w_er <- c(FD_columns_w_er, export_code)
    }

    FD_w_er_ <- use_table[["SoI2SoI"]][, c(FD_columns_w_er)] 


    y_c <- rowSums(FD)

    y_c_w_er <- rowSums(FD_w_er_)

     


    names(y_c) <- paste0(names(y_c), "/US-", st)
    names(y_c_w_er) <- paste0(names(y_c_w_er), "/US-", st)

    # q <- m$CommodityOutput[1:73]

    interm <- rowSums(use_all_soi_uses[, c(m$Industries$Code[1:71])])

    q_est_w_e_r <- interm + y_c_w_er
    
    c_all_states[[st]] <- q_est_w_e_r
    
    # q_est <- interm + y_c

    # # q <- matrix(model$q,dimnames = c(list(names(model$q), "q")))
    # # q_soi <-  subset(q, endsWith(rownames(q), soi), drop=FALSE)

    # test_q <- as.data.frame(cbind(q, q_est, q_est_w_e_r))

    # test_q$`q_est_>_q` <- test_q$q_est - test_q$q > 1E6

    # test_q$`q_est_w_e_r>_q` <- test_q$q_est_w_e_r - test_q$q > 1E6

    # failures_q_est <- subset(test_q, test_q$`q_est_>_q` == TRUE)

    # loginfo(paste0("State: ", st, " Year: ", year), logger = "test_q")
    # if (nrow(failures_q_est) > 0) {
    #   logwarn("Failures found in q estimation with current consumption demand:", logger = "test_q")
    #   logwarn(rownames(failures_q_est), logger = "test_q")
    # } else {
    #   print("No failures found in q estimation with current consumption demand.")
    # }

    # failures_q_est_w_e_r <- subset(test_q, test_q$`q_est_w_e_r>_q` == TRUE)
    # if (nrow(failures_q_est_w_e_r) > 0) {
    #   logwarn("Failures found in q estimation with consumption demand including ITA and Export Residual:", logger = "test_q")
    #   logwarn(rownames(failures_q_est_w_e_r), logger = "test_q")
    # } else {
    #   loginfo("No failures found in q estimation with consumption demand including ITA and Export Residual.", logger = "test_q")
    # }


} else {
    m <- useeior:::loadandbuildSatelliteTables(m)
    m <- useeior:::loadandbuildIndicators(m)
    m <- useeior:::loadDemandVectors(m)
    m <- useeior:::constructEEIOMatrices(m)
    Y <- m$DemandVectors$vectors
    FDm <- m$DemandVectors$meta
    y_c_soi_name <- FDm[FDm$Name == "DomesticConsumption" & FDm$Location != "RoUS", "ID"]
    y_c_rous_name <- FDm[FDm$Name == "DomesticConsumption" & FDm$Location == "RoUS", "ID"]
    y_c <- Y[[y_c_soi_name]][1:73] + Y[[y_c_rous_name]][1:73]
    interm_soi <- getStateUsebyType(m, type="intermediate", domestic=TRUE, RoUS=FALSE) 
    interm_rous <- getStateUsebyType(m, type="intermediate", domestic=TRUE, RoUS=TRUE) 
    interm <- interm_soi[1:73] + interm_rous[1:73]
    #Estimate q with current consumption demand, q_est and with the additions of ITA and Export Residual, q_est_w_e_r
    

    q_est <- interm + y_c

    q <- m$q[1:73]

    #q <- matrix(model$q,dimnames = c(list(names(model$q), "q")))
    #q_soi <-  subset(q, endsWith(rownames(q), soi), drop=FALSE)

    test_q <- as.data.frame(cbind(q, q_est))

    test_q$`q_est_>_q` <- test_q$q_est-test_q$q > 1E6


    failures_q_est <- subset(test_q, test_q$`q_est_>_q` == TRUE)

    loginfo(paste0("State: ",st, " Year: ",year),logger="test_q")
    if (nrow(failures_q_est) > 0) {
    logwarn("Failures found in q estimation with current consumption demand:",logger="test_q")
    logwarn(rownames(failures_q_est),logger="test_q")
    } else {
    print("No failures found in q estimation with current consumption demand.")
    }           

  }
}


#Compare y_c_all_states to US consumption


#c_states <- unlist(y_c_all_states, use.names=FALSE)

c_states <- sapply(c_all_states,cbind) #simplify = TRUE, USE.NAMES = TRUE)

rownames(c_states) <- names(c_all_states[[1]])

c_states <- rowSums(c_states)

## Get equivalent national model


modelspec <- "USEEIOv2.6-22"
configpaths <- "model_specs/national/USEEIOv2.6-22.yml"
m <- initializeModel(modelspec, configpaths)
m <- loadIOData(m, configpaths)

use_table <- m$DomesticUseTransactions
colnames(use_table)

FD_columns <- unlist(sapply(list("HouseholdDemand", "InvestmentDemand", "GovernmentDemand"),
                                    getVectorOfCodes, ioschema = m$specs$BaseIOSchema, iolevel = m$specs$BaseIOLevel))
FD_columns <- paste0(FD_columns,"/US")

interm <- rowSums(use_table[,c(m$Industries$Code_Loc)])

y_c_us <- rowSums(m$DomesticFinalDemand[,FD_columns])

c_us <- interm + y_c_us


compare_us_state_c <- data.frame(cbind(c_states,c_us))

compare_us_state_c[,"rel_dff"] <- (compare_us_state_c[,"c_states"] - compare_us_state_c[,"c_us"])/compare_us_state_c[,"c_us"]





#Just initialize model and load IO data
#model <- initializeModel(modelname, configpaths)


#alias <- ifelse(!is.na(model$specs$Alias), model$specs$Alias, NULL)
#dataname <- "DomesticUseTransactionswithTrade"
#filename <- paste(lapply(c("TwoRegion", model$specs$BaseIOLevel, dataname,
##                              alias, model$specs$IOYear,
 #                             model$specs$IODataVersion),  
#                  function(x) x[!is.na(x)]), collapse = "_")
#filename <- gsub(dataname, "DomesticUsewithTrade", filename)
#f <- loadDataFile(paste0("stateio/",filename,".rds"), "DataCommons", "stateior")

#TwoRegionIOData <- readRDS(f)
#names(TwoRegionIOData)

#Just initialize model and load IO data
#model <- initializeModel(modelname, configpaths)



#write.csv(test_q, paste0("scratch/",st,"EEIOv1.1_20_TestConsumptionDemand_q.csv"))
