##Script to test if a modified consumption demand vector does not lead to overestimation of total commodity output q

library(devtools)
load_all("../useeior")

#Select model with a model spec
#modelname <- "NYEEIOv1.1-GHGc-20"
modelname <- "CAEEIOv1.1-GHG-20"
#modelname <- "MEEEIOv1.1-GHGc-20"

st <- substr(modelname, 1,2)
configpaths <- file.path("model_specs/",st,paste0(modelname,'.yml')) 

#Just initialize model and load IO data
model <- initializeModel(modelname, configpaths)
model <- loadIOData(model, configpaths)

iolevel <- model$specs$BaseIOLevel

#Get Domestic Use Transactions with Trade table
use_table <- model$DomesticUseTransactionswithTrade

ita_column <- ifelse(iolevel == "Detail", "F05100", "F051")


#This is current code in useeior for consumption demand
FD_columns <- unlist(sapply(list("HouseholdDemand", "InvestmentDemand", "GovernmentDemand"),
                                getVectorOfCodes, ioschema = model$specs$BaseIOSchema, iolevel = iolevel))

FD <- use_table[["SoI2SoI"]][, c(FD_columns)] 

#FD_columns_w_ita <- c(FD_columns, ita_column)
#FD_w_ita_ <- use_table[["SoI2SoI"]][, c(FD_columns_w_ita)] 


#Following from procedure with production demand, for domestic model add in both ITA and Export residual
FD_columns_w_er <- c(FD_columns, ita_column, "ExportResidual")

FD_w_er_ <- use_table[["SoI2SoI"]][, c(FD_columns_w_er)] 


y_c <- rowSums(FD)

y_c_w_er <- rowSums(FD_w_er_)

names(y_c) <- paste0(names(y_c),"/US-",st)
names(y_c_w_er) <- paste0(names(y_c_w_er),"/US-",st)

q <- model$CommodityOutput[1:73] 

interm <- rowSums((use_table[["SoI2SoI"]][, c(model$Industries$Code[1:71])]))

#Estimate q with current consumption demand, q_est and with the additions of ITA and Export Residual, q_est_w_e_r
q_est_w_e_r <- interm + y_c_w_er

q_est <- interm + y_c

#q <- matrix(model$q,dimnames = c(list(names(model$q), "q")))
#q_soi <-  subset(q, endsWith(rownames(q), soi), drop=FALSE)

test_q <- as.data.frame(cbind(q, q_est, q_est_w_e_r))

test_q$`q_est_>_q` <- test_q$q_est-test_q$q > 1E6

test_q$`q_est_w_e_r>_q` <- test_q$q_est_w_e_r-test_q$q > 1E6

write.csv(test_q, paste0("scratch/",st,"EEIOv1.1_20_TestConsumptionDemand_q.csv"))
