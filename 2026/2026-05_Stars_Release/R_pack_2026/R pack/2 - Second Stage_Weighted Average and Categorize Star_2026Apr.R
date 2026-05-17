#######################################################
# SECOND STAGE - 											    
# DERIVING SUMMARY SCORE AND STAR RATING                        
#														        
# YALE/YNHH CORE 								                    
# 															    
# For questions, please submit an inquiry to 		
# the QualityNet Question & Answer Tool at:			
# https://cmsqualitysupport.servicenowservices.com/qnet_qa 
#######################################################


#This program is used to derive the hospital stars. The summary
#  score of each hospital is calculated based on the hospital's
#  group(domain) scores which are output from program 1. Then
#  the K-means approach is used to derive the hospital star ratings.

packages(dplyr)  #if "dplyr" package is not installed, you need to run this row

#Put all group scores into one file
Outcome_mortality2 <- Outcome_mortality %>%
  rename(Std_Outcomes_Mortality_score = grp_score) %>% rename(Outcomes_Mortality_cnt=total_cnt)

Outcome_safety2 <- Outcome_safety %>%
  rename(Std_Outcomes_Safety_score = grp_score) %>% rename(Outcomes_Safety_cnt=total_cnt)

Outcome_readmission2 <- Outcome_readmission %>%
  rename(Std_Outcomes_Readmission_score = grp_score) %>% rename(Outcomes_Readmission_cnt=total_cnt)

PtExp2 <- PtExp %>%
  rename(Std_PatientExp_score = grp_score) %>% rename(Patient_Experience_cnt=total_cnt)

Process2 <- Process %>%
  rename(Std_Process_score = grp_score) %>% rename(Process_cnt=total_cnt)


all <- inner_join(Outcome_mortality2 %>% select(PROVIDER_ID, Std_Outcomes_Mortality_score, Outcomes_Mortality_cnt),
                  Outcome_safety2 %>% select(PROVIDER_ID, Std_Outcomes_Safety_score, Outcomes_Safety_cnt),
                     by = "PROVIDER_ID") %>%
  inner_join(Outcome_readmission2 %>% select(PROVIDER_ID, Std_Outcomes_Readmission_score, Outcomes_Readmission_cnt),
             by = "PROVIDER_ID") %>%
  inner_join(PtExp2 %>% select(PROVIDER_ID, Std_PatientExp_score, Patient_Experience_cnt),
             by = "PROVIDER_ID") %>%
  inner_join(Process2 %>% select(PROVIDER_ID, Std_Process_score, Process_cnt),
             by = "PROVIDER_ID") 


################################################
#CALCULATING SUMMARY SCORES BASED ON WEIGHTED AVERAGE 
#1) fixed standard weights from CMS				 
#2) redistribute weights when there is missing group   
################################################
# Define fixed standard weights from CMS
all$std_weight_Mortality <- 0.22
all$std_weight_Safety <- 0.22
all$std_weight_Readmission <- 0.22
all$std_weight_PatientExperience <- 0.22
all$std_weight_Process <- 0.12

#Redistribute weights when there is missing group. For details, please refer to technical report. 
#For example, the group of Safety is missing, the weight for Mortality groups is changed 
#from 22/100 to 22/78, the weight for Timely and Effective Care is changed from 12/100 to 12/78.
wt_Mortality <- ifelse(all$Std_Outcomes_Mortality_score == "NaN", 0, all$std_weight_Mortality)
wt_Safety <- ifelse(all$Std_Outcomes_Safety_score == "NaN", 0, all$std_weight_Safety)
wt_Readmission <- ifelse(all$Std_Outcomes_Readmission_score == "NaN", 0, all$std_weight_Readmission)
wt_PtExp <- ifelse(all$Std_PatientExp_score == "NaN", 0, all$std_weight_PatientExperience)
wt_Process <- ifelse(all$Std_Process_score == "NaN", 0, all$std_weight_Process)
sum <- wt_Mortality + wt_Safety + wt_Readmission + wt_PtExp + wt_Process

all$weight_Outcomes_Mortality <- as.numeric(ifelse(wt_Mortality == 0, NA, wt_Mortality / sum))
all$weight_Outcomes_Safety <- as.numeric(ifelse(wt_Safety == 0, NA, wt_Safety / sum))
all$weight_Outcomes_Readmission <- as.numeric(ifelse(wt_Readmission == 0, NA, wt_Readmission / sum))
all$weight_PatientExperience <- as.numeric(ifelse(wt_PtExp == 0, NA, wt_PtExp / sum))
all$weight_Process <- as.numeric(ifelse(wt_Process == 0, NA, wt_Process / sum))

#change to numeric for summary_score calculation
all$Std_Outcomes_Mortality_score <- as.numeric(all$Std_Outcomes_Mortality_score)
all$Std_Outcomes_Safety_score <- as.numeric(all$Std_Outcomes_Safety_score)
all$Std_Outcomes_Readmission_score <- as.numeric(all$Std_Outcomes_Readmission_score)
all$Std_PatientExp_score <- as.numeric(all$Std_PatientExp_score)
all$Std_Process_score <- as.numeric(all$Std_Process_score)

mort<-all$weight_Outcomes_Mortality * all$Std_Outcomes_Mortality_score
safe<-all$weight_Outcomes_Safety * all$Std_Outcomes_Safety_score
readm<-all$weight_Outcomes_Readmission * all$Std_Outcomes_Readmission_score
ptexp<-all$weight_PatientExperience * all$Std_PatientExp_score
process<-all$weight_Process * all$Std_Process_score

df <- data.frame(mort, safe, readm, ptexp, process)
# Replace NA values with 0
df[is.na(df)] <- 0
# Calculate summary_score
df$summary_score <- df$mort + df$safe + df$readm + df$ptexp + df$process

all$summary_score <- df$summary_score


################################################
###reporting criteria - minimum 3 measures/per group and 3 groups with one of which must be Safety or mortality to receive a Star
################################################
D_mort<-ifelse(all$Outcomes_Mortality_cnt>=3, 1, 0)
D_safe<-ifelse(all$Outcomes_Safety_cnt>=3, 1, 0)
D_readm<-ifelse(all$Outcomes_Readmission_cnt>=3, 1, 0)
D_ptexp<-ifelse(all$Patient_Experience_cnt>=3, 1, 0)
D_process<-ifelse(all$Process_cnt>=3, 1, 0)

all$Total_measure_group_cnt <- D_mort + D_safe + D_readm + D_ptexp + D_process
all$MortSafe_Group_cnt <- (all$Outcomes_Mortality_cnt >= 3) + (all$Outcomes_Safety_cnt >= 3)
report_indicator <- (all$MortSafe_Group_cnt >= 1) & (all$Total_measure_group_cnt >= 3)
all$report_indicator <- as.numeric(report_indicator)

###Define peer grouping
total_cnt<- all$Total_measure_group_cnt

cnt_grp <- character(length(total_cnt))
cnt_grp[total_cnt == 3 & report_indicator ==1] <- "1) # of groups=3"
cnt_grp[total_cnt == 4 & report_indicator ==1] <- "2) # of groups=4"
cnt_grp[total_cnt == 5 & report_indicator ==1] <- "3) # of groups=5"

all$cnt_grp <- cnt_grp


################################################
###Generate Star Rating by K-Means for each peer grouping
################################################

#packages(tidyverse) # data manipulation
#packages(corrplot)
#packages(gridExtra)
#packages(GGally)
#packages(cluster) # clustering algorithms 
#packages(factoextra) # clustering algorithms & visualization


#Function for k-means clustering
fn_kmeans <- function(indt0) {
  
  # step1: k-means clustering with median quintile medians as initial centroids;
  s1 <- indt0 %>%
    summarise(P20 = quantile(summary_score, 0.2),
              P40 = quantile(summary_score, 0.4),
              P60 = quantile(summary_score, 0.6),
              P80 = quantile(summary_score, 0.8))
  
  s2 <- indt0 %>%
    mutate(grp = case_when(
      is.na(summary_score) ~ NA_integer_,
      summary_score <= s1$P20 ~ 1L,
      summary_score <= s1$P40 ~ 2L,
      summary_score <= s1$P60 ~ 3L,
      summary_score <= s1$P80 ~ 4L,
      TRUE ~ 5L
    ))
  table(s2$grp)
  
  s3 <- s2 %>%
    group_by(grp) %>%
    summarise(summary_score_median = median(summary_score, na.rm = TRUE)) # Calculate median of summary scores by group
  
  #set.seed(123) #no need set.seed() because we use the medians of quintiles as the initial centeroids
  km.out1<- kmeans(indt0$summary_score, s3$summary_score_median, iter.max = 1000) #algorithm of Hartigan and Wong (1979) is used by default
  
  # step2: using results from Step 1 (km.out1) as initial centeroids
  km.out<- kmeans(indt0$summary_score, km.out1$centers, iter.max = 1000) #algorithm of Hartigan and Wong (1979) is used by default
  print(km.out)
  
  # Merge with the initial dataset
  dd <- cbind(indt0, cluster = km.out$cluster)
  # head(dd)
  
  # Order clusters based on the mean of clusters (summary_score)
  cluster_sort <- data.frame(cluster = order(km.out$centers))
  cluster_sort$star <- seq.int(nrow(cluster_sort)) #add _n_ as star (highest is with star=5)
  
  #merge data sets
  merged1 <- merge(dd,cluster_sort,by="cluster")
  return(merged1)
  
}
cnt_s10 <- all[all$cnt_grp == "1) # of groups=3" & all$report_indicator==1, ]
merged3<-fn_kmeans(cnt_s10)
star1 <- inner_join(all, merged3 %>% select(PROVIDER_ID, star),
                   by = "PROVIDER_ID") 

cnt_s20 <- all[all$cnt_grp == "2) # of groups=4" & all$report_indicator==1, ]
merged4<-fn_kmeans(cnt_s20)
star2 <- inner_join(all, merged4 %>% select(PROVIDER_ID, star),
                   by = "PROVIDER_ID")

cnt_s30 <- all[all$cnt_grp == "3) # of groups=5" & all$report_indicator==1, ]
merged5<-fn_kmeans(cnt_s30)
star3 <- inner_join(all, merged5 %>% select(PROVIDER_ID, star),
                   by = "PROVIDER_ID")


nstar <- all[all$report_indicator==0, ]
all <- bind_rows(star1, star2, star3, nstar)
#Output file
Star_ <- all %>%
   arrange(PROVIDER_ID)
#output_file <- file.path(R, paste0("Star_", year, quarter, ".csv"))
#write.csv(Star_, output_file, row.names = FALSE) 

################################################
###National Average of Summary Score and Group Scores
################################################
#freq_table <- table(all$star)
#print(freq_table)

#summary score, and summary score by peer grouping
temp <- subset(Star_, report_indicator==1)
Summary_Score_Nat <- mean(temp$summary_score, na.rm = TRUE)
#print(Summary_Score_Nat)

temp <- subset(Star_, report_indicator==1 & cnt_grp == "1) # of groups=3")
Summary_Score_Nat_peer3 <- mean(temp$summary_score, na.rm = TRUE)
#print(Summary_Score_Nat_peer3)

temp <- subset(Star_, report_indicator==1 & cnt_grp == "2) # of groups=4")
Summary_Score_Nat_peer4 <- mean(temp$summary_score, na.rm = TRUE)
#print(Summary_Score_Nat_peer4)

temp <- subset(Star_, report_indicator==1 & cnt_grp == "3) # of groups=5")
Summary_Score_Nat_peer5 <- mean(temp$summary_score, na.rm = TRUE)
#print(Summary_Score_Nat_peer5)

#group scores
temp <- subset(Star_, Outcomes_Mortality_cnt >= 3)
Out_Mrt_Grp_Score_Nat <- mean(temp$Std_Outcomes_Mortality_score, na.rm = TRUE)
#print(Out_Mrt_Grp_Score_Nat)

temp <- subset(Star_, Outcomes_Safety_cnt >= 3)
Out_Sft_Grp_Score_Nat <- mean(temp$Std_Outcomes_Safety_score, na.rm = TRUE)
#print(Out_Sft_Grp_Score_Nat)

temp <- subset(Star_, Outcomes_Readmission_cnt >= 3)
Out_Readm_grp_Score_Nat <- mean(temp$Std_Outcomes_Readmission_score, na.rm = TRUE)
#print(Out_Readm_grp_Score_Nat)

temp <- subset(Star_, Patient_Experience_cnt >= 3)
Pt_Exp_Grp_Score_Nat <- mean(temp$Std_PatientExp_score, na.rm = TRUE)
#print(Pt_Exp_Grp_Score_Nat)

temp <- subset(Star_, Process_cnt >= 3)
Prc_of_Care_Grp_Score_Nat <- mean(temp$Std_Process_score, na.rm = TRUE)
#print(Prc_of_Care_Grp_Score_Nat)

#Output file
National_average_<- data.frame(Summary_Score_Nat, Summary_Score_Nat_peer3, Summary_Score_Nat_peer4, Summary_Score_Nat_peer5,
                                      Out_Mrt_Grp_Score_Nat, Out_Sft_Grp_Score_Nat, Out_Readm_grp_Score_Nat, Pt_Exp_Grp_Score_Nat, Prc_of_Care_Grp_Score_Nat)
output_file <- file.path(R, paste0("National_average_", year, quarter, ".csv"))
write.csv(National_average_, output_file, row.names = FALSE) 

################################################
###Add Cap for Safety of Care
################################################
qq <- Star_

# Rank into 4 quantile groups
qq <- qq %>%
  mutate(Safe_q = ntile(Std_Outcomes_Safety_score, 4))

attr(qq$Safe_q, "label") <- "Quartiles of Safety Group Score"

# cap: 4-star maximum for hospitals with Q1 Safety & 3+ Safety measures
qq <- qq %>%
  mutate(
    star_p3 = star,
    star_p3 = ifelse(Outcomes_Safety_cnt >= 3 & Safe_q == 1 & star > 4, 4, star_p3) 
  )

# Replace original 'star' with modified value
RESULTS <- qq %>%
  mutate(star = star_p3) %>%
  select(-Safe_q, -star_p3)
#table(RESULTS$star)
#table(Star_$star)

output_file <- file.path(R, paste0("Star_", year, quarter, ".csv"))
write.csv(RESULTS, output_file, row.names = FALSE) 

