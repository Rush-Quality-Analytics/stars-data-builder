#######################################################
# FIRST STAGE - 											    
# DERIVING GROUP SCORE FOR EACH MEASURE TYPE GROUP 			    
#														        
# YALE/YNHH CORE 								                    
# 															    
# For questions, please submit an inquiry to 		
# the QualityNet Question & Answer Tool at:			
# https://cmsqualitysupport.servicenowservices.com/qnet_qa 
#######################################################


###This program is used to calculate the 5 groups(domains) scores for each hospital. 
#The 5 groups(domains) are:
#(1)Mortality  
#(2)Safety of Care
#(3)Readmission 
#(4)Patient Experience 
#(5)Timely and Effective Care
#The 5 groups (domains) scores of each hospitals will be used in program 2 to derive the hospital star ratings. 



################################################
###Group Score Calculation
################################################

fn_grp_score <- function(varlist, indt0) {
  
  ID_measure<-c("PROVIDER_ID", varlist)
  indt<-indt0[,ID_measure]

  # Count non-missing values measures
  total_cnt <- rowSums(!is.na(indt[varlist]))
  indt$total_cnt<-total_cnt
  indt$measure_wt <- ifelse(total_cnt > 0, 1 / total_cnt, NA)
  # Calculate row-wise average
  indt$score_before_std <- rowMeans(indt[, varlist], na.rm = TRUE)
  # Standardize the data
  indt$grp_score<-scale(indt$score_before_std)
  # Remove the '[, 1]' at the end of the variable name
  indt$grp_score <- gsub("\\[, 1\\]$", "", indt$grp_score)

  # Calculate summary statistics for score_before_std
  summary_stats <- describe(indt$score_before_std)
  summary_stats_df <- as.data.frame(summary_stats)
  summary_stats_df2 <- summary_stats_df[, c("mean", "sd")]
  # Replicate the single-row data frame to match the number of rows in indt
  df2_replicated <- summary_stats_df2[rep(1, nrow(indt)), ]
  
  indt2<-cbind(indt,df2_replicated)
  return(indt2)
  
}

################################################
###Outcomes - Mortality 
################################################
Outcome_mortality<-fn_grp_score(measure_OM, Std_data_analysis)
# Output the dataframe to a CSV file
output_file <- file.path(R, paste0("Outcome_mortality", ".csv"))
write.csv(Outcome_mortality, output_file, row.names = FALSE) 


################################################
###Outcomes - Safety 
################################################
Outcome_safety<-fn_grp_score(measure_OS, Std_data_analysis)
output_file <- file.path(R, paste0("Outcome_safety", ".csv"))
write.csv(Outcome_safety, output_file, row.names = FALSE) 

################################################
###Outcomes - Readmission 
################################################
Outcome_readmission <-fn_grp_score(measure_OR, Std_data_analysis)
output_file <- file.path(R, paste0("Outcome_readmission", ".csv"))
write.csv(Outcome_readmission, output_file, row.names = FALSE) 

################################################
###Patient Experience
################################################
PtExp <-fn_grp_score(measure_PtExp, Std_data_analysis)
output_file <- file.path(R, paste0("PtExp", ".csv"))
write.csv(PtExp, output_file, row.names = FALSE) 


################################################
###Timely and Effective Care
################################################
Process <-fn_grp_score(measure_Process, Std_data_analysis)
output_file <- file.path(R, paste0("Process", ".csv"))
write.csv(Process, output_file, row.names = FALSE) 

#Continue to "2 - Second Stage_Weighted Average and Categorize Star_2026Apr" R progam