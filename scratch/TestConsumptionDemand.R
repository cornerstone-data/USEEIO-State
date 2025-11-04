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


states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL",
            "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT",
            "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
            "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

year <- 2020

spec <- "v1.1-GHG"
modelstub <- paste0('EEIO',spec,"-")
modelspec <- paste0("State",modelstub,substr(year,3,4))  
specpath <- file.path("model_specs","all_states//")
configpaths <- file.path(paste0(specpath, modelspec, '.yml'))


for (st in states) {

    m <- useeior:::initializeModel(modelspec, configpaths)
    iolevel <- m$specs$BaseIOLevel
    m$specs$ModelRegionAcronyms[1] <- paste0("US-",st)
    m <- loadIOData(m, configpaths)

    

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

    FD_w_er_ <- use_table[["SoI2SoI"]][, c(FD_columns_w_er)] 


    y_c <- rowSums(FD)

    y_c_w_er <- rowSums(FD_w_er_)

    names(y_c) <- paste0(names(y_c),"/US-",st)
    names(y_c_w_er) <- paste0(names(y_c_w_er),"/US-",st)

    q <- m$CommodityOutput[1:73] 

    interm <- rowSums(use_all_soi_uses[, c(m$Industries$Code[1:71])])

    #Estimate q with current consumption demand, q_est and with the additions of ITA and Export Residual, q_est_w_e_r
    q_est_w_e_r <- interm + y_c_w_er

    q_est <- interm + y_c

    #q <- matrix(model$q,dimnames = c(list(names(model$q), "q")))
    #q_soi <-  subset(q, endsWith(rownames(q), soi), drop=FALSE)

    test_q <- as.data.frame(cbind(q, q_est, q_est_w_e_r))

    test_q$`q_est_>_q` <- test_q$q_est-test_q$q > 1E6

    test_q$`q_est_w_e_r>_q` <- test_q$q_est_w_e_r-test_q$q > 1E6

    failures_q_est <- subset(test_q, test_q$`q_est_>_q` == TRUE)

    loginfo(paste0("State: ",st, " Year: ",year),logger="test_q")
    if (nrow(failures_q_est) > 0) {
    logwarn("Failures found in q estimation with current consumption demand:",logger="test_q")
    logwarn(rownames(failures_q_est),logger="test_q")
    } else {
    print("No failures found in q estimation with current consumption demand.")
    }           

    failures_q_est_w_e_r <- subset(test_q, test_q$`q_est_w_e_r>_q` == TRUE)
    if (nrow(failures_q_est_w_e_r) > 0) {
    logwarn("Failures found in q estimation with consumption demand including ITA and Export Residual:",logger="test_q")
    logwarn(rownames(failures_q_est_w_e_r),logger="test_q")
    } else {
    loginfo("No failures found in q estimation with consumption demand including ITA and Export Residual.",logger="test_q")
    }


}









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
