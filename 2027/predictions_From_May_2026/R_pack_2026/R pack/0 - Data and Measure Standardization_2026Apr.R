#######################################################
# Create Star analysis data file 			  	
#										  		
# YALE/YNHH CORE 	  		                      		
#											  		
# For questions, please submit an inquiry to 		
# the QualityNet Question & Answer Tool at:			
# https://cmsqualitysupport.servicenowservices.com/qnet_qa 
#######################################################

##This file is used to derive analysis data 'Std_data_2026Apr_analysis', which will be used in programs 1 and 2

# Lets clean the unnecessary items
gc()
rm(list = ls(all = TRUE))

# The below code is a function, which can help us to install any package if you don’t have it, later it load the package
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}


# Read data
# Load necessary library
packages(readr)  # For reading data files

# Define year and quarter of the data
year <- 2026
quarter <- "may"

#define input and output file paths
HC <- "/Users/kenlocey/GitHub/stars-data-builder/2027/predictions_From_May_2026"
R <- "/Users/kenlocey/GitHub/stars-data-builder/2027/predictions_From_May_2026/R_CSV_output"


# Load data 
HC_all_data <- file.path(HC, paste0("data_for_2027_predictions_from_May2026.csv"))

# Read the data into R
all_data <- read_csv(HC_all_data)
#str(all_data)


subset_df0 <- all_data[, !grepl("_DEN$|_PRED$|_VOL$|_RATE_P$|_NUMB_COMP$|PROVIDER_ID$", names(all_data))] # Subset data to include only variables not ending with '_DEN', '_PRED', '_VOL', 'RATE_P', or 'PROVIDER_ID'
measure_all<-names(subset_df0)  #measures on Hospital Compare
#print(measure_all)
ID_measure<-c('PROVIDER_ID', measure_all)  #add PROVIDER_ID

################################################
###Exclusion Criteria: MEASURE VOLUME <=100
################################################
# Set the threshold for the number of observations
threshold <- 100

# Calculate the frequency of each variable
variable_freq <- sapply(all_data[,measure_all], function(x) sum(!is.na(x)))

# Select variables with fewer observations than the threshold
measure_name <- names(variable_freq[variable_freq <= threshold])

# Print the selected variables
print(measure_name)
Less100_measure<-as.data.frame(measure_name)
# Output the dataframe to a CSV file
output_file <- file.path(R, paste0("Less100_measure_", year, quarter, ".csv"))
write.csv(Less100_measure, output_file, row.names = FALSE)  


################################################
###REMOVE HOSPITALS WHICH DO NOT HAVE ANY FINAL INCLUDED MEASURES
################################################
#create data with measure volume >100
measure_in <- names(variable_freq[variable_freq > threshold])
ID_measure_in<-c('PROVIDER_ID',measure_in)  #add PRIVIDER_ID
Initial_data_0<-all_data[ID_measure_in]

# Count non-missing variables for each row
Initial_data_0$non_missing_count <- rowSums(!is.na(Initial_data_0))

# REMOVE HOSPITALS WHICH DO NOT HAVE ANY FINAL INCLUDED MEASURES
Initial_data <- Initial_data_0[Initial_data_0$non_missing_count >1, ] # PROVIDER_ID is also counted, so it is >1
#chk<-Initial_data[Initial_data_0$non_missing_count ==0, ]


################################################
###Add an output file for mean and standard deviation of measure scores
################################################

packages("psych")   
# Calculate summary statistics for each variable in the list
summary_stats <- describe(Initial_data[,measure_in])
summary_stats <- data.frame(variable = rownames(summary_stats), summary_stats)

# Keep only the variables 'n', 'min', 'max', 'mean', 'sd'
Measure_average_stddev <- summary_stats[, c("variable", "n", "min", "max", "mean", "sd")]
# Output the data frame to a CSV file
output_file <- file.path(R, paste0("Measure_average_stddev_", year, quarter, ".csv"))
write.csv(Measure_average_stddev, output_file, row.names = FALSE) 

################################################
###Standardize Measure Scores
################################################
# Standardize the data
Std_data0 <- scale(Initial_data[,measure_in])

# Convert the standardized data to a data frame
Std_data<- as.data.frame(Std_data0)


################################################
###RE-DIRECT MEASURES
################################################
Std_data$MORT_30_AMI <- -Std_data$MORT_30_AMI
Std_data$MORT_30_CABG <- -Std_data$MORT_30_CABG
Std_data$MORT_30_COPD <- -Std_data$MORT_30_COPD
Std_data$MORT_30_HF <- -Std_data$MORT_30_HF
Std_data$MORT_30_PN <- -Std_data$MORT_30_PN
Std_data$MORT_30_STK <- -Std_data$MORT_30_STK
Std_data$PSI_4_SURG_COMP <- -Std_data$PSI_4_SURG_COMP
Std_data$Hybrid_HWM <- -Std_data$Hybrid_HWM  #Hybrid_HWM is added in Year 2026


Std_data$COMP_HIP_KNEE <- -Std_data$COMP_HIP_KNEE
Std_data$HAI_1 <- -Std_data$HAI_1
Std_data$HAI_2 <- -Std_data$HAI_2
Std_data$HAI_3 <- -Std_data$HAI_3
Std_data$HAI_4 <- -Std_data$HAI_4
Std_data$HAI_5 <- -Std_data$HAI_5
Std_data$HAI_6 <- -Std_data$HAI_6
Std_data$PSI_90_SAFETY <- -Std_data$PSI_90_SAFETY

Std_data$EDAC_30_AMI <- -Std_data$EDAC_30_AMI
Std_data$EDAC_30_HF <- -Std_data$EDAC_30_HF
Std_data$EDAC_30_PN <- -Std_data$EDAC_30_PN
Std_data$OP_32 <- -Std_data$OP_32
Std_data$READM_30_CABG <- -Std_data$READM_30_CABG
Std_data$READM_30_COPD <- -Std_data$READM_30_COPD
Std_data$READM_30_HIP_KNEE <- -Std_data$READM_30_HIP_KNEE
Std_data$Hybrid_HWR  <- -Std_data$Hybrid_HWR #Hybrid_HWR to replace READM_30_HOSP_WIDE in Year 2026
Std_data$OP_35_ADM <- -Std_data$OP_35_ADM
Std_data$OP_35_ED <- -Std_data$OP_35_ED
Std_data$OP_36 <- -Std_data$OP_36

Std_data$OP_22 <- -Std_data$OP_22
#Std_data$PC_01 <- -Std_data$PC_01 ##PC_01 is removed in year 2026

#Std_data$OP_3B <- -Std_data$OP_3B  ##OP_3B is removed in year 2025
Std_data$OP_18B <- -Std_data$OP_18B

Std_data$OP_8 <- -Std_data$OP_8
Std_data$OP_10 <- -Std_data$OP_10
Std_data$OP_13 <- -Std_data$OP_13
Std_data$SAFE_USE_OF_OPIOIDS <- -Std_data$SAFE_USE_OF_OPIOIDS  #SAFE_USE_OF_OPIOIDS is added in Year 2025


###OUTPUTS OF 'std_data_2026Apr_analysis' ARE GENERATED
# Rename columns by adding "std_" before each variable name
names(Std_data) <- paste0("std_", names(Std_data))

Std_data_analysis <- cbind(Initial_data["PROVIDER_ID"], Std_data)
# Output the data to a CSV file
output_file <- file.path(R, paste0("Std_data_analysis_", year, quarter, ".csv"))
write.csv(Std_data_analysis, output_file, row.names = FALSE) 


################################################
###CREATE THE FINAL LISTS OF EACH MEASURE GROUP AND REMOVE THOSE MEASURES WITH <= 100 HOSPITALS
################################################
std_measure_in<-names(Std_data)
#MORTALITY
measure_OM0<-c('std_MORT_30_AMI', 'std_MORT_30_CABG', 'std_MORT_30_COPD', 'std_MORT_30_HF', 'std_MORT_30_PN', 'std_MORT_30_STK', 'std_PSI_4_SURG_COMP', 'std_Hybrid_HWM')
measure_OM <- intersect(std_measure_in, measure_OM0)
print(measure_OM)

#SAFETY
measure_OS0<-c('std_COMP_HIP_KNEE',  'std_HAI_1', 'std_HAI_2', 'std_HAI_3', 'std_HAI_4', 'std_HAI_5', 'std_HAI_6', 'std_PSI_90_SAFETY')
measure_OS <- intersect(std_measure_in, measure_OS0)
print(measure_OS)

#READMISSION
measure_OR0<-c('std_EDAC_30_AMI', 'std_EDAC_30_HF', 'std_EDAC_30_PN', 'std_OP_32','std_READM_30_CABG', 'std_READM_30_COPD', 
               'std_READM_30_HIP_KNEE', 'std_Hybrid_HWR', 'std_OP_35_ADM', 'std_OP_35_ED', 'std_OP_36')
measure_OR <- intersect(std_measure_in, measure_OR0)
print(measure_OR)

#PATIENT EXPERIENCE 
measure_PtExp0<-c('std_H_COMP_1_LINEAR_SCORE', 'std_H_COMP_2_LINEAR_SCORE', 'std_H_COMP_3_LINEAR_SCORE', 'std_H_COMP_5_LINEAR_SCORE', 'std_H_COMP_6_LINEAR_SCORE',
                  'std_H_COMP_7_LINEAR_SCORE', 'std_H_CLEAN_LINEAR_SCORE', 'std_H_QUIET_LINEAR_SCORE','std_H_HSP_RATING_LINEAR_SCORE','std_H_RECMND_LINEAR_SCORE',
                  'std_O-COMP-1', 'std_O-COMP-2', 'std_O-COMP-3', 'std_O-PATIENT-RATE', 'std_O-PATIENT-REC')
measure_PtExp <- intersect(std_measure_in, measure_PtExp0)
print(measure_PtExp)

#TIMELY AND EFFECTIVE CARE
measure_Process0<-c('std_IMM_3', 'std_OP_10', 'std_OP_13', 'std_OP_18B', 'std_OP_22', 'std_OP_23', 
                    'std_OP_29',  'std_OP_8', 'std_SAFE_USE_OF_OPIOIDS', 'std_SEP_1')  
#OP-2 and OP-3B are removed in Year 2025 #SAFE_USE_OF_OPIOIDS is added in Year 2025
#HCP_COVID_19  and PC_01 are removed in Year 2026
measure_Process <- intersect(std_measure_in, measure_Process0)
print(measure_Process)


#Continue to "1 - First stage_Simple Average of Measure Scores_2026Apr" R progam