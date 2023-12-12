# Importing and Preparing Data for Figures and Tables in 'Belief Updating with Misinformation'
# Code by Lars Wittrock
# Date: 12/12/2023

######################################################
# TO ADJUST
######################################################

# Path - raw data input and clean data output
path <- "C:/Users/larwi/OneDrive/AAA/Uni/Projects/Removing Information/Analysis/InfoRetract/Data"



######################################################
# SETUP
######################################################

# Packages
library(tidyr)
library(dplyr)
library(plotrix)
library(stringr)

library(anytime)


######################################################
# DATA
######################################################

# Input two treatments
######################################################

# Treat 1:
# Loading data file
raw_data <- read.csv(paste0(path, "/raw_data.csv"))
df_wide <- raw_data

# Loading data file with times per page
raw_times <- read.csv(paste0(path, "/raw_times.csv"))

# Treat 2:
# Loading data file additional treatment
raw_data_extra <- read.csv(paste0(path, "/raw_data_extra.csv"))
df_wide_extra <- raw_data_extra

# Adjusting names for compatibility
names(df_wide_extra) <-  sub("_aggregate", "", names(df_wide_extra), perl = TRUE)

# Loading data file with times per page
raw_times_extra <- read.csv(paste0(path, "/raw_times_extra.csv"))


# Preparation for merge
######################################################

# Participant id
names(df_wide)[names(df_wide) == "ï..participant.id_in_session"] <- "id"
names(df_wide_extra)[names(df_wide_extra) == "ï..participant.id_in_session"] <- "id"
df_wide_extra$id <- df_wide_extra$id + 1000

# Adding session variable
df_wide$treat_aggregate_signal <- 0
df_wide_extra$treat_aggregate_signal <- 1

# Merging data sets
df_wide_combined <- bind_rows(df_wide, df_wide_extra)
df_wide <- df_wide_combined


# Cleaning data
######################################################

# Reformatting variable names
names(df_wide)[names(df_wide) == "participant.label"] <- "prolific_id"
names(df_wide)[names(df_wide) == "participant._index_in_pages"] <- "final_page"
names(df_wide)[names(df_wide) == "participant.payoff"] <- "payoff"
names(df_wide)[names(df_wide) == "Intro.1.player.is_mobile"] <- "is_mobile"

names(df_wide)[names(df_wide) == "belief_survey.1.player.urn"] <- "urn"
names(df_wide)[names(df_wide) == "belief_survey.1.player.sr_button_clicks"] <- "sr_button_clicks"
names(df_wide)[names(df_wide) == "belief_survey.1.player.test1"] <- "test1"
names(df_wide)[names(df_wide) == "belief_survey.1.player.test2"] <- "test2"
names(df_wide)[names(df_wide) == "belief_survey.1.player.test3"] <- "test3"
names(df_wide)[names(df_wide) == "belief_survey.1.player.test4"] <- "test4"
names(df_wide)[names(df_wide) == "belief_survey.1.player.test5"] <- "test5"
names(df_wide)[names(df_wide) == "belief_survey.1.player.test6"] <- "test6"
names(df_wide)[names(df_wide) == "belief_survey.1.player.test_correct"] <- "test_correct"

names(df_wide)[names(df_wide) == "questionnaire.1.player.age"] <- "age"
names(df_wide)[names(df_wide) == "questionnaire.1.player.country"] <- "country"
names(df_wide)[names(df_wide) == "questionnaire.1.player.belief_fake_blue"] <- "belief_fake_blue"
names(df_wide)[names(df_wide) == "questionnaire.1.player.belief_fake_red"] <- "belief_fake_red"
names(df_wide)[names(df_wide) == "questionnaire.1.player.open_feedback"] <- "text_choice"

# Treatment variable
df_wide$treat <- 999
df_wide <- mutate(df_wide, treat = ifelse(belief_survey.1.player.treat==3, 1, treat)) # show history and previous belief
df_wide <- mutate(df_wide, treat = ifelse(belief_survey.1.player.treat==1, 2, treat)) # show history but not previous belief
df_wide <- mutate(df_wide, treat = ifelse(belief_survey.1.player.treat==2, 3, treat)) # show no history but previous belief
df_wide$treat_no_anchor <- ifelse(df_wide$treat == 2, 1, 0)
df_wide$treat_no_history <- ifelse(df_wide$treat == 3, 1, 0)

# Completion check
df_wide$finished <- ifelse(df_wide$participant._current_page_name == "FinalPage", 1, 0)

# CRT questions
df_wide <- mutate(df_wide, crt1_correct = ifelse(questionnaire.1.player.crt1 == 2, 1, 0))
df_wide <- mutate(df_wide, crt2_correct = ifelse(questionnaire.1.player.crt2 == 2, 1, 0))
df_wide <- mutate(df_wide, crt3_correct = ifelse(questionnaire.1.player.crt3 == 3, 1, 0))
df_wide <- mutate(df_wide, crt4_correct = ifelse(questionnaire.1.player.crt4 == 1, 1, 0))
df_wide$crt_score <- df_wide$crt1_correct + df_wide$crt2_correct + df_wide$crt3_correct + df_wide$crt4_correct

# Formatting factor variables
df_wide$gender <- factor(df_wide$questionnaire.1.player.gender, levels = 1:3, labels = c("Female", "Male", "Other"))
df_wide$edu <- factor(df_wide$questionnaire.1.player.edu, levels = 1:6, labels = c("No completed education", "High school", "Bachelor", "Master", "PhD", "Other"))
df_wide$occ <- factor(df_wide$questionnaire.1.player.occ, levels = 1:5, labels = c("Student", "Unemployed", "Working full time", "Working part time", "Other"))
df_wide$prob_fam <- factor(df_wide$questionnaire.1.player.prob_fam, levels = 1:4, labels = c("I have no idea what it is about.", "I have heard of it, but I do not know much about it.", "I know a bit about it.", "I understand it well."))
df_wide$belief_strategy <- factor(df_wide$questionnaire.1.player.belief_strategy, levels = 1:5, labels = c("I tried to calculate precise probabilities.", "I used some rule of thumb for calculations.", "I mainly relied on my intuition.", "I had no idea and entered random numbers.", "Other"))
df_wide$belief_optimal <- factor(df_wide$questionnaire.1.player.belief_optimal, levels = 1:8, labels = c("0%", "1-5%", "6-10%", "11-20%", "21-30%", "31-40%", "41-50%", "More than 50%"))

# Memory questions
names(df_wide)[names(df_wide) == "questionnaire.1.player.belief_memory1"] <- "memory_1"
names(df_wide)[names(df_wide) == "questionnaire.1.player.belief_memory2"] <- "memory_2"

# Removing unnecessary variables
df_wide = df_wide[, !grepl("participant.", names(df_wide))]
df_wide = df_wide[, !grepl("session.", names(df_wide))]
df_wide = df_wide[, !grepl("Intro.", names(df_wide))]
df_wide = df_wide[, !grepl("player.urn", names(df_wide))]
df_wide = df_wide[, !grepl("player.treat", names(df_wide))]
df_wide = df_wide[, !grepl("pay_round", names(df_wide))]
df_wide = df_wide[, !grepl("id_in_group", names(df_wide))]
df_wide = df_wide[, !grepl("role", names(df_wide))]
df_wide = df_wide[, !grepl("player.test", names(df_wide))]
df_wide = df_wide[, !grepl("player.payoff", names(df_wide))]
df_wide = df_wide[, !grepl("player.sr_button_clicks", names(df_wide))]
df_wide = df_wide[, !grepl("id_in_subsession", names(df_wide))]
df_wide = df_wide[, !grepl("questionnaire", names(df_wide))]

# Cleaning data of empty and test observations
df_wide <- df_wide[df_wide$final_page != 0, ]
df_wide <- df_wide[df_wide$id != 1, ]  # own test before running


# Modifying time data file
######################################################

# Merging two time files
df_time <- raw_times
df_time$id <- df_time$participant_id_in_session

df_time_extra <- raw_times_extra
df_time_extra$id <- df_time_extra$participant_id_in_session
df_time_extra$id <- df_time_extra$id + 1000

df_time_combined <- rbind(df_time, df_time_extra)

df_time <- df_time_combined

# Some cleaning
df_time <- df_time[df_time$id != 1, ]  # own test before running

df_time <- df_time[order(df_time$id, df_time$page_index),]

df_time$time <- anytime(df_time$epoch_time_completed)
df_time$page_seconds <- ifelse(df_time$page_index!=0, (df_time$time - lag(df_time$time, 1)), NA)

# total duration
df_time <- df_time %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    time_max = max(epoch_time_completed, na.rm = TRUE),
    time_min = min(epoch_time_completed, na.rm = TRUE))

df_time$duration <- anytime(df_time$time_max) - anytime(df_time$time_min)
df_time$duration <- as.numeric(gsub(" secs", "", df_time$duration))

df_time <- subset(df_time,select=-c(time_max, time_min))

df_time_temp <- df_time %>% select(id, duration)
df_time_temp <- unique(df_time_temp)

df_time_temp$duration_min <- df_time_temp$duration/60

# Pivot wide for merging later  
df_time_wide <- df_time %>%
  pivot_wider(id_cols = "id",
              names_from = c("page_name", "page_index"),
              values_from = "page_seconds")

# Replacing NULL values with NA
df_time_wide[] <- lapply(df_time_wide, function(x) as.numeric(as.character(x)))

# Merging with total duration
df_time_wide <- merge(df_time_wide, df_time_temp)

# Merging fields from different sessions
df_time_wide$CRT <- ifelse(is.na(df_time_wide$CRT_43), df_time_wide$CRT_54, df_time_wide$CRT_43)
df_time_wide$Demographics <- ifelse(is.na(df_time_wide$Demographics_44), df_time_wide$Demographics_55, df_time_wide$Demographics_44)
df_time_wide$Transition <- ifelse(is.na(df_time_wide$Transition_41), df_time_wide$Transition_52, df_time_wide$Transition_41)
df_time_wide$BeliefQuestions <- ifelse(is.na(df_time_wide$BeliefQuestions_42), df_time_wide$BeliefQuestions_53, df_time_wide$BeliefQuestions_42)

df_time_wide <- subset(df_time_wide,select=-c(CRT_43, CRT_54, Demographics_44, Demographics_55, Transition_41, Transition_52, BeliefQuestions_42, BeliefQuestions_53))


# Renaming variables
df_time_wide <- subset(df_time_wide,select=-c(InitializeParticipant_0))

names(df_time_wide)[names(df_time_wide) == "Welcome_1"] <- "seconds_welcome"
names(df_time_wide)[names(df_time_wide) == "Consent_3"] <- "seconds_consent"
names(df_time_wide)[names(df_time_wide) == "Instructions_4"] <- "seconds_instructions"
names(df_time_wide)[names(df_time_wide) == "InstructionsFeedback_5"] <- "seconds_instructions_feedback"
names(df_time_wide)[names(df_time_wide) == "UrnDraw_6"] <- "seconds_urn_draw"
names(df_time_wide)[names(df_time_wide) == "Transition"] <- "seconds_transition"
names(df_time_wide)[names(df_time_wide) == "BeliefQuestions"] <- "seconds_belief_questions"
names(df_time_wide)[names(df_time_wide) == "CRT"] <- "seconds_crt_questions"
names(df_time_wide)[names(df_time_wide) == "Demographics"] <- "seconds_demographics_questions"
names(df_time_wide)[names(df_time_wide) == "MemoryQuestions_40"] <- "seconds_memory_questions"
names(df_time_wide)[names(df_time_wide) == "FinalPage_56"] <- "seconds_final_page"

# Merging datasets
df_wide_all <- merge(df_wide, df_time_wide, by = "id", all = TRUE)


# Creating long dataset for belief analysis
######################################################

# Wide -> long by round per subject
df_long <- df_wide_all %>% 
  pivot_longer(cols = starts_with(c("belief_survey")), 
                names_to = c("round", ".value"),
                names_pattern = "belief_survey.([0-9]+).player.(ball|verification|belief|verify_round|info_button_clicks|minus_button_clicks|plus_button_clicks|aggregate_round|check)")


# Including page seconds with long pivot
df_longer <- df_long %>%
  pivot_longer(cols = starts_with("BeliefInput"),
               names_to = "page",
               values_to = "seconds_belief")

df_longer <- df_longer[!(df_longer$round == 1 & !df_longer$page == "BeliefInput_7"), ]
df_longer <- df_longer[!(df_longer$round == 2 & !df_longer$page == "BeliefInput_11"), ]
df_longer <- df_longer[!(df_longer$round == 3 & !df_longer$page == "BeliefInput_15"), ]
df_longer <- df_longer[!(df_longer$round == 4 & !df_longer$page == "BeliefInput_19"), ]
df_longer <- df_longer[!(df_longer$round == 5 & !df_longer$page == "BeliefInput_23"), ]
df_longer <- df_longer[!(df_longer$round == 6 & !df_longer$page == "BeliefInput_27"), ]
df_longer <- df_longer[!(df_longer$round == 7 & !df_longer$page == "BeliefInput_31"), ]
df_longer <- df_longer[!(df_longer$round == 8 & !df_longer$page == "BeliefInput_35"), ]
df_longer <- df_longer[!(df_longer$round == 9 & !df_longer$page == "BeliefInput_39"), ]
df_longer <- df_longer[!(df_longer$round == 10 & !df_longer$page == "BeliefInput_43"), ]
df_longer <- df_longer[!(df_longer$round == 11 & !df_longer$page == "BeliefInput_47"), ]
df_longer <- df_longer[!(df_longer$round == 12 & !df_longer$page == "BeliefInput_51"), ]

df_long <- df_longer
df_long <- subset(df_long,select=-c(page))

# keeping only finished responses
df_long <- df_long[df_long$finished == 1 & !is.na(df_long$belief), ]
df_long <- subset(df_long,select=-c(final_page, finished, is_mobile))



# Cleaning and additional variables
######################################################

# Formatting variables
df_long$round <- as.numeric(df_long$round)

df_long <- df_long %>% mutate(verify_round = coalesce(verify_round, 0))
df_long <- df_long %>% mutate(aggregate_round = coalesce(aggregate_round, 0))

df_long$belief <- as.numeric(df_long$belief)
df_long$belief <- df_long$belief/100

df_long$ball_red <- ifelse(df_long$ball=="red", 1, 0)
df_long <- mutate(df_long, ball_red = ifelse(verify_round == 1, NA, ball_red))

df_long$ball_blue <- ifelse(df_long$ball=="blue", 1, 0)
df_long <- mutate(df_long, ball_blue = ifelse(verify_round == 1, NA, ball_blue))

df_long$ver_retract <- ifelse(df_long$verification=="fake", 1, 0)
df_long <- mutate(df_long, ver_retract = ifelse(verify_round == 0, NA, ver_retract))

df_long$aggregate_informative <- ifelse(df_long$check=="informative", 1, 0)
df_long <- mutate(df_long, aggregate_informative = ifelse(aggregate_round == 0, NA, aggregate_informative))

df_long$ball_red_lag1 <- ifelse(df_long$verify_round == 1, lag(df_long$ball_red, n = 1), NA)


df_long$red_retract <- NA
df_long$blue_retract <- NA
df_long$red_confirm <- NA
df_long$blue_confirm <- NA

df_long <- mutate(df_long, red_retract = ifelse(verify_round ==1 | aggregate_round ==1, 0, red_retract))
df_long <- mutate(df_long, red_retract = ifelse(verify_round ==1 & ball_red_lag1==1 & ver_retract==1, 1, red_retract))
df_long <- mutate(df_long, red_retract = ifelse(aggregate_round ==1 & ball_red==1 & aggregate_informative==0, 1, red_retract))

df_long <- mutate(df_long, blue_retract = ifelse(verify_round ==1 | aggregate_round ==1, 0, blue_retract))
df_long <- mutate(df_long, blue_retract = ifelse(verify_round ==1 & ball_red_lag1==0 & ver_retract==1, 1, blue_retract))
df_long <- mutate(df_long, blue_retract = ifelse(aggregate_round ==1 & ball_red==0 & aggregate_informative==0, 1, blue_retract))

df_long <- mutate(df_long, red_confirm = ifelse(verify_round ==1 | aggregate_round ==1, 0, red_confirm))
df_long <- mutate(df_long, red_confirm = ifelse(verify_round ==1 & ball_red_lag1==1 & ver_retract==0, 1, red_confirm))
df_long <- mutate(df_long, red_confirm = ifelse(aggregate_round ==1 & ball_red==1 & aggregate_informative==1, 1, red_confirm))

df_long <- mutate(df_long, blue_confirm = ifelse(verify_round ==1 | aggregate_round ==1, 0, blue_confirm))
df_long <- mutate(df_long, blue_confirm = ifelse(verify_round ==1 & ball_red_lag1==0 & ver_retract==0, 1, blue_confirm))
df_long <- mutate(df_long, blue_confirm = ifelse(aggregate_round ==1 & ball_red==0 & aggregate_informative==1, 1, blue_confirm))


# Cleaning 
df_long <- df_long[!is.na(df_long$belief),]
df_long <- subset(df_long,select=-c(ball, verification, check))



# Preparing analysis by generating posteriors
######################################################

# Adding parameters
gamma <- 0.75 # urn distribution, 3 same and 1 opposite ball
prob_fake <- 0.6
prior_R <- 0.5

prob_r_R <- (1-prob_fake)*gamma + prob_fake*0.5
prob_b_R <- (1-prob_fake)*(1-gamma) + prob_fake*0.5

prob_r_B <- (1-prob_fake)*(1-gamma) + prob_fake*0.5
prob_b_B <- (1-prob_fake)*gamma + prob_fake*0.5

# Calculating posteriors
post <- function(prior, num_red, num_blue, num_red_ret, num_blue_ret, num_red_conf, num_blue_conf){
  
  num_blue_nonretracted <- num_blue - num_blue_ret
  num_red_nonretracted <- num_red - num_red_ret
  posterior_temp <- (prior*(prob_b_R^num_blue_nonretracted)*(prob_r_R^num_red_nonretracted)) / ((1-prior)*(prob_b_B^num_blue_nonretracted)*(prob_r_B^num_red_nonretracted) + prior*(prob_b_R^num_blue_nonretracted)*(prob_r_R^num_red_nonretracted))
  posterior <- (posterior_temp*((1-gamma)^num_blue_conf)*(gamma^num_red_conf)) / ((1-posterior_temp)*(gamma^num_blue_conf)*((1-gamma)^num_red_conf) + posterior_temp*((1-gamma)^num_blue_conf)*(gamma^num_red_conf))
  
  return(posterior)
}

# Counting previous verifications
df_long$prev_verified <- sapply(1:nrow(df_long), function(r) sum(df_long$verify_round & df_long$id==df_long$id[r] & df_long$round < df_long$round[r], na.rm = TRUE))

# Counting total ball numbers at each point in time
df_long$prev_balls_red <- sapply(1:nrow(df_long), function(r) sum(df_long$ball_red & df_long$id==df_long$id[r] & df_long$round < df_long$round[r], na.rm = TRUE))
df_long$prev_balls_blue <- sapply(1:nrow(df_long), function(r) sum(df_long$ball_blue & df_long$id==df_long$id[r] & df_long$round < df_long$round[r], na.rm = TRUE))

df_long <- mutate(df_long, red_balls = colSums(rbind(ball_red, prev_balls_red), na.rm = TRUE))
df_long <- mutate(df_long, blue_balls = colSums(rbind(ball_blue, prev_balls_blue), na.rm = TRUE))

df_long$prev_balls_red_ret <- sapply(1:nrow(df_long), function(r) sum(df_long$red_retract & df_long$id==df_long$id[r] & df_long$round < df_long$round[r], na.rm = TRUE))
df_long$prev_balls_blue_ret <- sapply(1:nrow(df_long), function(r) sum(df_long$blue_retract & df_long$id==df_long$id[r] & df_long$round < df_long$round[r], na.rm = TRUE))
df_long$prev_balls_red_conf <- sapply(1:nrow(df_long), function(r) sum(df_long$red_confirm & df_long$id==df_long$id[r] & df_long$round < df_long$round[r], na.rm = TRUE))
df_long$prev_balls_blue_conf <- sapply(1:nrow(df_long), function(r) sum(df_long$blue_confirm & df_long$id==df_long$id[r] & df_long$round < df_long$round[r], na.rm = TRUE))

df_long <- mutate(df_long, red_balls_ret = colSums(rbind(red_retract, prev_balls_red_ret), na.rm = TRUE))
df_long <- mutate(df_long, blue_balls_ret = colSums(rbind(blue_retract, prev_balls_blue_ret), na.rm = TRUE))
df_long <- mutate(df_long, red_balls_conf = colSums(rbind(red_confirm, prev_balls_red_conf), na.rm = TRUE))
df_long <- mutate(df_long, blue_balls_conf = colSums(rbind(blue_confirm, prev_balls_blue_conf), na.rm = TRUE))

df_long$prev_informative <- sapply(1:nrow(df_long), function(r) sum(df_long$aggregate_informative & df_long$id==df_long$id[r] & df_long$round < df_long$round[r], na.rm = TRUE))

df_long$temp_agg_uninf <- ifelse(df_long$aggregate_informative==0,1,0)
df_long$prev_uninformative <- sapply(1:nrow(df_long), function(r) sum(df_long$temp_agg_uninf & df_long$id==df_long$id[r] & df_long$round < df_long$round[r], na.rm = TRUE))


# cleaning
df_long = df_long[, !grepl("prev_balls", names(df_long))]
df_long <- subset(df_long,select=-c(ball_blue, temp_agg_uninf))


# History variable - compressed and sign (see Goncalvez et al)
#############################################

# Creating string variable with signal
df_long$sig = ""

df_long <- df_long %>% mutate(df_long, sig = ifelse(ball_red == 1 & verify_round == 0 & aggregate_round == 0, "r", sig))
df_long <- df_long %>% mutate(df_long, sig = ifelse(ball_red == 0 & verify_round == 0 & aggregate_round == 0, "b", sig))

df_long <- df_long %>% mutate(df_long, sig = ifelse(ball_red_lag1 == 1 & ver_retract == 1 & verify_round == 1, "r_ret", sig))
df_long <- df_long %>% mutate(df_long, sig = ifelse(ball_red_lag1 == 0 & ver_retract == 1 & verify_round == 1, "b_ret", sig))

df_long <- df_long %>% mutate(df_long, sig = ifelse(ball_red_lag1 == 1 & ver_retract == 0 & verify_round == 1, "r_conf", sig))
df_long <- df_long %>% mutate(df_long, sig = ifelse(ball_red_lag1 == 0 & ver_retract == 0 & verify_round == 1, "b_conf", sig))

df_long <- df_long %>% mutate(df_long, sig = ifelse(ball_red == 1 & aggregate_informative == 1 & aggregate_round == 1, "r_inf", sig))
df_long <- df_long %>% mutate(df_long, sig = ifelse(ball_red == 0 & aggregate_informative == 1 & aggregate_round == 1, "b_inf", sig))

df_long <- df_long %>% mutate(df_long, sig = ifelse(ball_red == 1 & aggregate_informative == 0 & aggregate_round == 1, "r_uninf", sig))
df_long <- df_long %>% mutate(df_long, sig = ifelse(ball_red == 0 & aggregate_informative == 0 & aggregate_round == 1, "b_uninf", sig))


# Pivot wider for merging later  
df_comp_hist <- df_long %>%
  pivot_wider(id_cols = "id",
              names_from = c("round"),
              names_glue = "sig_{round}",
              values_from = "sig")

df_long <- merge(df_long, df_comp_hist, by = "id")

# Complete history
df_long$hist <- NA
df_long <- mutate(df_long, hist = ifelse(round==1, paste(sig_1), hist))
df_long <- mutate(df_long, hist = ifelse(round==2, paste(sig_1, sig_2), hist))
df_long <- mutate(df_long, hist = ifelse(round==3, paste(sig_1, sig_2, sig_3), hist))
df_long <- mutate(df_long, hist = ifelse(round==4, paste(sig_1, sig_2, sig_3, sig_4), hist))
df_long <- mutate(df_long, hist = ifelse(round==5, paste(sig_1, sig_2, sig_3, sig_4, sig_5), hist))
df_long <- mutate(df_long, hist = ifelse(round==6, paste(sig_1, sig_2, sig_3, sig_4, sig_5, sig_6), hist))
df_long <- mutate(df_long, hist = ifelse(round==7, paste(sig_1, sig_2, sig_3, sig_4, sig_5, sig_6, sig_7), hist))
df_long <- mutate(df_long, hist = ifelse(round==8, paste(sig_1, sig_2, sig_3, sig_4, sig_5, sig_6, sig_7, sig_8), hist))
df_long <- mutate(df_long, hist = ifelse(round==9, paste(sig_1, sig_2, sig_3, sig_4, sig_5, sig_6, sig_7, sig_8, sig_9), hist))
df_long <- mutate(df_long, hist = ifelse(round==10, paste(sig_1, sig_2, sig_3, sig_4, sig_5, sig_6, sig_7, sig_8, sig_9, sig_10), hist))
df_long <- mutate(df_long, hist = ifelse(round==11, paste(sig_1, sig_2, sig_3, sig_4, sig_5, sig_6, sig_7, sig_8, sig_9, sig_10, sig_11), hist))
df_long <- mutate(df_long, hist = ifelse(round==12, paste(sig_1, sig_2, sig_3, sig_4, sig_5, sig_6, sig_7, sig_8, sig_9, sig_10, sig_11, sig_12), hist))

# Cleaning
df_long = df_long[, !grepl("sig_", names(df_long))]

# Compressed history
df_long$comp_hist <- gsub(" [a-z] [a-z]_ret", "", df_long$hist)

# Comp hist includes confirmations/retraction/informative/uninformative
df_long$hist_conf <- ifelse(grepl("_conf", df_long$hist), 1, 0)
df_long$hist_ret <- ifelse(grepl("_ret", df_long$hist), 1, 0)

df_long$hist_inf <- ifelse(grepl("_inf", df_long$hist), 1, 0)
df_long$hist_uninf <- ifelse(grepl("_uninf", df_long$hist), 1, 0)

# sign history - example: red retraction is equivalent to blue new signal
df_long$sign_hist <- df_long$hist
df_long <- mutate(df_long, sign_hist = gsub(" b_ret", " r", df_long$sign_hist))
df_long <- mutate(df_long, sign_hist = gsub(" r_ret", " b", df_long$sign_hist))

# History comparing (un)informative signals to retractions/confirmations.
df_long$agg_hist <- df_long$hist
df_long <- mutate(df_long, agg_hist = gsub(" b b_ret", " b_uninf", df_long$agg_hist))
df_long <- mutate(df_long, agg_hist = gsub(" r r_ret", " r_uninf", df_long$agg_hist))
df_long <- mutate(df_long, agg_hist = gsub(" b b_conf", " b_inf", df_long$agg_hist))
df_long <- mutate(df_long, agg_hist = gsub(" r r_conf", " r_inf", df_long$agg_hist))

# Posterior Calculations
######################################

# Aggregate Bayesian posteriors each round
df_long <- mutate(df_long, posterior_agg = post(prior_R, red_balls, blue_balls, red_balls_ret, blue_balls_ret, red_balls_conf, blue_balls_conf))

# Bayesian posteriors given last round's belief
df_long$belief_lag1 <- ifelse(df_long$round > 1, lag(df_long$belief, 1), 0.5)
df_long$belief_lag2 <- ifelse(df_long$round > 2, lag(df_long$belief, 2), 0.5)
df_long$belief_lag3 <- ifelse(df_long$round > 3, lag(df_long$belief, 2), 0.5)

df_long <- mutate(df_long, posterior_subj = ifelse(verify_round == 0 & ball_red == 1, post(belief_lag1, 1, 0, 0, 0, 0, 0), post(belief_lag1, 0, 1, 0, 0, 0, 0)))
df_long <- mutate(df_long, posterior_subj = ifelse(verify_round == 1 & ver_retract==1, belief_lag2, posterior_subj))
df_long <- mutate(df_long, posterior_subj = ifelse(verify_round == 1 & ver_retract==0 & ball_red_lag1==1, post(belief_lag2, 0, 0, 0, 0, 1, 0), posterior_subj))
df_long <- mutate(df_long, posterior_subj = ifelse(verify_round == 1 & ver_retract==0 & ball_red_lag1==0, post(belief_lag2, 0, 0, 0, 0, 0, 1), posterior_subj))
df_long <- mutate(df_long, posterior_subj = ifelse(aggregate_round == 1 & aggregate_informative==1 & ball_red==0, post(belief_lag1, 0, 0, 0, 0, 0, 1), posterior_subj))
df_long <- mutate(df_long, posterior_subj = ifelse(aggregate_round == 1 & aggregate_informative==1 & ball_red==1, post(belief_lag1, 0, 0, 0, 0, 1, 0), posterior_subj))
df_long <- mutate(df_long, posterior_subj = ifelse(aggregate_round == 1 & aggregate_informative==0, belief_lag1, posterior_subj))

# Adjusting reported beliefs and posteriors for log calculations - similar to Benjamin (2019)
df_long$belief_adj <- df_long$belief
df_long$belief_adj[df_long$belief_adj == 0] <- 0.001
df_long$belief_adj[df_long$belief_adj == 1] <- 0.999

df_long$belief_lag_adj <- df_long$belief_lag1
df_long$belief_lag_adj[df_long$belief_lag_adj == 0] <- 0.001
df_long$belief_lag_adj[df_long$belief_lag_adj == 1] <- 0.999

df_long$post_subj_adj <- df_long$posterior_subj
df_long$post_subj_adj[df_long$post_subj_adj == 0] <- 0.001
df_long$post_subj_adj[df_long$post_subj_adj == 1] <- 0.999

# Calculating added info signal
added_info <- function(prior, post){
  x = (post-prior*post)/(post+prior-2*post*prior)
  return(x)
}

df_long$signal_subj <- added_info(df_long$belief_lag_adj, df_long$post_subj_adj)

df_long$signal_memory <- 999
df_long <- mutate(df_long, signal_memory = ifelse(!is.na(ver_retract) & ver_retract==1 & ball_red_lag1==1, 0.4, signal_memory))
df_long <- mutate(df_long, signal_memory = ifelse(!is.na(ver_retract) & ver_retract==1 & ball_red_lag1==0, 0.6, signal_memory))

df_long <- mutate(df_long, signal_memory = ifelse(!is.na(ver_retract) & ver_retract==0 & ball_red_lag1==1, 2/3, signal_memory))
df_long <- mutate(df_long, signal_memory = ifelse(!is.na(ver_retract) & ver_retract==0 & ball_red_lag1==0, 1/3, signal_memory))

df_long <- mutate(df_long, signal_memory = ifelse(verify_round==0 & ball_red==0, 0.4, signal_memory))
df_long <- mutate(df_long, signal_memory = ifelse(verify_round==0 & ball_red==1, 0.6, signal_memory))


# Calculate likelihood ratios - based on subjective posteriors
df_long$truelnpost <- log(df_long$post_subj_adj/(1-df_long$post_subj_adj))
df_long$obslnpost <- log(df_long$belief_adj/(1-df_long$belief_adj))

df_long$prior_ratio <- log(df_long$belief_lag_adj/(1-df_long$belief_lag_adj))
df_long$signal_ratio <- log(df_long$signal_subj/(1-df_long$signal_subj))

df_long$signal_mem_ratio <- log(df_long$signal_memory/(1-df_long$signal_memory))


# Creating additional variables for analysis
######################################################

# Creating type of updating variable
df_long$type <- 999
df_long <- mutate(df_long, type = ifelse(verify_round == 0 & aggregate_round == 0, 1, type))

df_long <- mutate(df_long, type = ifelse(verify_round == 1 & ver_retract==1, 2, type))
df_long <- mutate(df_long, type = ifelse(verify_round == 1 & ver_retract==0, 3, type))

df_long <- mutate(df_long, type = ifelse(aggregate_round == 1 & aggregate_informative==0, 4, type))
df_long <- mutate(df_long, type = ifelse(aggregate_round == 1 & aggregate_informative==1, 5, type))

df_long$type <- factor(df_long$type, levels=1:5, labels=c("Regular signal", "Retraction", "Confirmation", "Uninformative", "Informative"))

# Number of same retractions/confirmations for regular updating
df_long <- mutate(df_long, ret_same = ifelse(ball_red==1, red_balls_ret, blue_balls_ret))
df_long <- mutate(df_long, conf_same = ifelse(ball_red==1, red_balls_conf, blue_balls_conf))

# Number of same ret/conf for verifications
df_long <- mutate(df_long, conf_same = ifelse(verify_round==1 & ball_red_lag1==1, red_balls_conf, conf_same))
df_long <- mutate(df_long, conf_same = ifelse(verify_round==1 & ball_red_lag1==0, blue_balls_conf, conf_same))

df_long <- mutate(df_long, ret_same = ifelse(verify_round==1 & ball_red_lag1==1, red_balls_ret, ret_same))
df_long <- mutate(df_long, ret_same = ifelse(verify_round==1 & ball_red_lag1==0, blue_balls_ret, ret_same))

df_long$ret_total <- df_long$red_balls_ret + df_long$blue_balls_ret
df_long$conf_total <- df_long$red_balls_conf + df_long$blue_balls_conf

df_long$diff_ret_conf <- df_long$ret_same - df_long$conf_same

# Belief difference to Bayesian posterior
df_long$belief_diff <- df_long$belief - df_long$posterior_subj
df_long$belief_diff_abs <- abs(df_long$belief_diff)
df_long$belief_diff_abs_lag <- lag(df_long$belief_diff_abs,1)

# Over reported beliefs
df_long$over_report <- NA
df_long <- mutate(df_long, over_report = ifelse(verify_round==0 & ball_red==1, belief - posterior_subj, over_report))
df_long <- mutate(df_long, over_report = ifelse(verify_round==0 & ball_red==0, posterior_subj - belief, over_report))

df_long <- mutate(df_long, over_report = ifelse(verify_round==1 & ver_retract==1 & ball_red_lag1==1, posterior_subj - belief, over_report))
df_long <- mutate(df_long, over_report = ifelse(verify_round==1 & ver_retract==1 & ball_red_lag1==0, belief - posterior_subj, over_report))

df_long <- mutate(df_long, over_report = ifelse(verify_round==1 & ver_retract==0 & ball_red_lag1==1, belief - posterior_subj, over_report))
df_long <- mutate(df_long, over_report = ifelse(verify_round==1 & ver_retract==0 & ball_red_lag1==0, posterior_subj - belief, over_report))


# Change in belief
df_long$belief_change <- df_long$belief - df_long$belief_lag1

df_long$belief_change_adj <- NA

df_long <- mutate(df_long, belief_change_adj = ifelse(verify_round!=1 & ball_red == 0, -belief_change, belief_change_adj))
df_long <- mutate(df_long, belief_change_adj = ifelse(verify_round!=1 & ball_red == 1, belief_change, belief_change_adj))

df_long <- mutate(df_long, belief_change_adj = ifelse(verify_round==1 & ver_retract==1 & ball_red_lag1 == 0, belief_change, belief_change_adj))
df_long <- mutate(df_long, belief_change_adj = ifelse(verify_round==1 & ver_retract==1 & ball_red_lag1 == 1, -belief_change, belief_change_adj))
df_long <- mutate(df_long, belief_change_adj = ifelse(verify_round==1 & ver_retract==0 & ball_red_lag1 == 0, -belief_change, belief_change_adj))
df_long <- mutate(df_long, belief_change_adj = ifelse(verify_round==1 & ver_retract==0 & ball_red_lag1 == 1, belief_change, belief_change_adj))


df_long$belief_change_reg <- ifelse(df_long$type=="Regular signal", df_long$belief_change_adj, NA)


# Dummy for correct/over/under/no change/wrong reported beliefs - cutoff is +- 0.01 
df_long$correct <- ifelse(df_long$over_report <= 0.01 & df_long$over_report>= -0.01, 1, 0)
df_long$over <- ifelse(df_long$over_report > 0.01, 1, 0)
df_long$under <- ifelse(df_long$over_report < -0.01 & df_long$belief_change_adj>0, 1, 0)
df_long$no_change <- ifelse(df_long$belief_change==0, 1, 0)
df_long$wrong <- ifelse(df_long$belief_change_adj<0, 1, 0)

# Dummy for belief report 0.5
df_long$belief50 <- ifelse(df_long$belief == 0.5, 1, 0)

# lagged variables
df_long$post_lag1 <- lag(df_long$posterior_subj, 1)
df_long$post_lag2 <- lag(df_long$posterior_subj, 2)

df_long$belief_change_lag1 <- lag(df_long$belief_change, 1)
df_long$belief_change_adj_lag1 <- lag(df_long$belief_change_adj, 1)

df_long$obslnpost_lag1 <- lag(df_long$obslnpost, 1)
df_long$truelnpost_lag1 <- lag(df_long$truelnpost, 1)

df_long$signal_subj_lag1 <- lag(df_long$signal_subj, 1)

df_long$prior_ratio_lag <- lag(df_long$prior_ratio, 1)
df_long$signal_ratio_lag <- NA
df_long <- mutate(df_long, signal_ratio_lag = ifelse(verify_round==1 & ball_red_lag1 == 1, log(0.6/0.4), signal_ratio_lag))
df_long <- mutate(df_long, signal_ratio_lag = ifelse(verify_round==1 & ball_red_lag1 == 0, log(0.4/0.6), signal_ratio_lag))

df_long$correct_lag <- lag(df_long$correct, 1)
df_long$under_lag <- lag(df_long$under, 1)
df_long$over_lag <- lag(df_long$over, 1)
df_long$no_change_lag <- lag(df_long$no_change, 1)
df_long$wrong_lag <- lag(df_long$wrong, 1)

df_long$over_report_lag1 <- lag(df_long$over_report, 1)
df_long$over_report_lag1 <- ifelse(df_long$round==1, NA, df_long$over_report_lag1)

df_long$over_report_lag2 <- lag(df_long$over_report, 2)
df_long$over_report_lag2 <- ifelse(df_long$round==1 | df_long$round==2, NA, df_long$over_report_lag2)

df_long$belief_diff_lag1 <- lag(df_long$belief_diff, 1)
df_long$belief_diff_lag1 <- ifelse(df_long$round==1, NA, df_long$belief_diff_lag1)

df_long$belief_diff_lag2 <- lag(df_long$belief_diff, 2)
df_long$belief_diff_lag2 <- ifelse(df_long$round==1 | df_long$round==2, NA, df_long$belief_diff_lag2)

# prior and signal adjusted
df_long <- mutate(df_long, prior_adj = ifelse(ball_red_lag1 == 1, belief_lag1, 1-belief_lag1))
df_long <- mutate(df_long, signal_adj = ifelse(ball_red_lag1 == 1, 1-signal_subj, signal_subj))


# Additional variables for retraction analysis
######################################################

# Re-code over-reaction
df_long$over_report_ret <- -df_long$over_report

# Initial reaction
df_long$initial_reaction <- NA
df_long <- mutate(df_long, initial_reaction = ifelse(correct_lag ==1, "correct", initial_reaction))
df_long <- mutate(df_long, initial_reaction = ifelse(over_lag ==1, "over", initial_reaction))

df_long <- mutate(df_long, initial_reaction = ifelse(under_lag ==1, "under", initial_reaction))
df_long <- mutate(df_long, initial_reaction = ifelse(no_change_lag ==1, "no change", initial_reaction))
df_long <- mutate(df_long, initial_reaction = ifelse(wrong_lag ==1, "wrong", initial_reaction))


df_long$initial_reaction_alt <- df_long$initial_reaction
df_long$initial_reaction_alt <- ifelse(df_long$initial_reaction=="no change", "under", df_long$initial_reaction_alt)

# Reaction now
df_long$reaction <- NA
df_long <- mutate(df_long, reaction = ifelse(correct ==1, "correct", reaction))
df_long <- mutate(df_long, reaction = ifelse(over ==1, "over", reaction))

df_long <- mutate(df_long, reaction = ifelse(under ==1, "under", reaction))
df_long <- mutate(df_long, reaction = ifelse(no_change ==1, "no change", reaction))
df_long <- mutate(df_long, reaction = ifelse(wrong ==1, "wrong", reaction))

df_long$reaction_alt <- df_long$reaction
df_long$reaction_alt <- ifelse(df_long$reaction=="no change", "under", df_long$reaction_alt)

# belief change in LLR
df_long$obslnpost_change <- df_long$obslnpost - df_long$obslnpost_lag1
df_long$obslnpost_change_lag1 <- lag(df_long$obslnpost_change, 1)

# Calculate induced priors ## only for verification rounds
df_long$prior_induced <- NA
df_long <- mutate(df_long, prior_induced = ifelse(ball_red_lag1==1, (2*belief_lag1)/(3 - belief_lag1), prior_induced))
df_long <- mutate(df_long, prior_induced = ifelse(ball_red_lag1==0, (3*belief_lag1)/(2 + belief_lag1), prior_induced))

df_long$belief_diff_priorinduced <- df_long$belief - df_long$prior_induced

df_long <- mutate(df_long, belief_diff_priorinduced_adj = ifelse(ball_red_lag1 == 0, -belief_diff_priorinduced, belief_diff_priorinduced))

# Creating induced urn distribution that would rationalize beliefs t-1 and t. 
df_long$signal_perceived <- NA
df_long <- mutate(df_long, signal_perceived = ifelse(verify_round==1, 
                                                     (belief_lag_adj*(4*belief_adj+3)-7*belief_adj)/
                                                       (8*belief_lag_adj*belief_adj-4*belief_lag_adj-4*belief_adj), signal_perceived))

df_long <- mutate(df_long, signal_perceived = ifelse(ball_red_lag1==0 & verify_round==1, 1-signal_perceived, signal_perceived))

# Additional variable to compare beliefs after 3 retractions in a row
df_long$belief_lag6 <- lag(df_long$belief,6)
df_long$belief_lag4 <- lag(df_long$belief,4)


# Additional variables for confirmation analysis
######################################################

# Creating induced urn distribution that would rationalize beliefs t-2 and t-1. 
df_long$urn_dist_induced <- NA
df_long <- mutate(df_long, urn_dist_induced = ifelse(verify_round==1, 
                    (belief_lag2*(4*belief_lag1+3)-7*belief_lag1)/
                    (8*belief_lag2*belief_lag1-4*belief_lag2-4*belief_lag1), urn_dist_induced))

df_long <- mutate(df_long, urn_dist_induced = ifelse(ball_red_lag1==0 & verify_round==1, 1-urn_dist_induced, urn_dist_induced))

df_long <- mutate(df_long, confirm_irrational = ifelse(urn_dist_induced > 1 | urn_dist_induced < 0, 1, 0))

# Creating post for after confirmation
df_long$post_induced <- NA
df_long <- mutate(df_long, post_induced = ifelse(!is.na(urn_dist_induced) & ball_red_lag1==1,
                                                 (belief_lag2*urn_dist_induced)/
                                                   (belief_lag2*urn_dist_induced+(1-belief_lag2)*(1-urn_dist_induced)), post_induced))

df_long <- mutate(df_long, post_induced = ifelse(!is.na(urn_dist_induced) & ball_red_lag1==0,
                                                 1 - ((1-belief_lag2)*urn_dist_induced)/
                                                   ((1-belief_lag2)*urn_dist_induced+belief_lag2*(1-urn_dist_induced)), post_induced))

df_long <- mutate(df_long, post_induced = ifelse(post_induced > 1, 1, post_induced))
df_long <- mutate(df_long, post_induced = ifelse(post_induced < -1, -1, post_induced))


df_long$over_report_conf_alt <- df_long$belief - df_long$post_induced
df_long$over_report_conf_alt <- ifelse(df_long$ball_red_lag1==1,df_long$over_report_conf_alt,-df_long$over_report_conf_alt)

# Adjustment of zeros and ones.
df_long$post_induced_adj <- df_long$post_induced
df_long$post_induced_adj[df_long$post_induced_adj == 0] <- 0.001
df_long$post_induced_adj[df_long$post_induced_adj == 1] <- 0.999

# Creating signal that would lead to post after confirm signal
df_long <- mutate(df_long, signal_confirm = added_info(df_long$belief_lag_adj, df_long$post_induced_adj))

# Signal confirm ratio
df_long$signal_confirm_ratio <- log(df_long$signal_confirm/(1-df_long$signal_confirm))

# leads to many NaN for signal ration smaller 0 or bigger 1.

# Induced signal
df_long$signal_update <- added_info(df_long$belief_lag1, df_long$belief)
df_long$signal_update_lag1 <- lag(df_long$signal_update,1)


# Average rational belief change
df_long$belief_change_rational_lag1 <- df_long$belief_lag2 - df_long$post_lag1
df_long$belief_change_rational_lag1 <- ifelse(df_long$ball_red_lag1==1, -df_long$belief_change_rational_lag1, df_long$belief_change_rational_lag1)

df_long$belief_change_rational <- df_long$belief_lag2 - df_long$posterior_subj
df_long$belief_change_rational <- ifelse(df_long$ball_red_lag1==1, -df_long$belief_change_rational, df_long$belief_change_rational)

# Initial reaction for confirmation
df_long$initial_reaction_conf <- df_long$initial_reaction
df_long$initial_reaction_conf <- ifelse((df_long$urn_dist_induced<0 | df_long$urn_dist_induced>1) & df_long$initial_reaction=="over", "over much", df_long$initial_reaction_conf)


# Additional variables for analysis of sequences
######################################################

df_long$prev_ball <- lag(df_long$ball_red, 1)
df_long <- mutate(df_long, prev_ball = ifelse(round==1, NA, prev_ball))

df_long$two_balls <- NA
df_long <- mutate(df_long, two_balls = ifelse(prev_ball==1 & ball_red==1, "RR", two_balls))
df_long <- mutate(df_long, two_balls = ifelse(prev_ball==1 & ball_red==0, "RB", two_balls))
df_long <- mutate(df_long, two_balls = ifelse(prev_ball==0 & ball_red==1, "BR", two_balls))
df_long <- mutate(df_long, two_balls = ifelse(prev_ball==0 & ball_red==0, "BB", two_balls))

df_long <- mutate(df_long, two_balls = ifelse(verify_round==1 & ver_retract==1, "Retraction", two_balls))
df_long <- mutate(df_long, two_balls = ifelse(verify_round==1 & ver_retract==0, "Confirmation", two_balls))

# Posterior after 2 signals
df_long$post2 <- NA
df_long <- mutate(df_long, post2 = ifelse(two_balls=="RR", post(belief_lag2, 2, 0, 0, 0, 0, 0), post2))
df_long <- mutate(df_long, post2 = ifelse(two_balls=="RB", post(belief_lag2, 1, 1, 0, 0, 0, 0), post2))
df_long <- mutate(df_long, post2 = ifelse(two_balls=="BR", post(belief_lag2, 1, 1, 0, 0, 0, 0), post2))
df_long <- mutate(df_long, post2 = ifelse(two_balls=="BB", post(belief_lag2, 0, 2, 0, 0, 0, 0), post2))

df_long$over_report2 <- NA
df_long <- mutate(df_long, over_report2 = ifelse(verify_round==0 & ball_red==1, belief - post2, over_report2))
df_long <- mutate(df_long, over_report2 = ifelse(verify_round==0 & ball_red==0, post2 - belief, over_report2))
df_long <- mutate(df_long, over_report2 = ifelse(verify_round==1, over_report, over_report2))


# Final clean
######################################################

# Reorder variables
df_long <- df_long %>% select(id, treat_aggregate_signal, treat, round, verify_round, aggregate_round, urn, ball_red, ball_red_lag1, ver_retract, aggregate_informative, belief, belief_lag1, signal_subj, signal_memory, posterior_subj, posterior_agg, everything())
df_wide_all <- df_wide_all %>% select(id, prolific_id, treat, treat_aggregate_signal, finished, everything())


# Creating subject level data sets
######################################################

# Creating subject data set - only completed observations
df_subject <- df_long %>%
  group_by(id) %>%
  dplyr::summarise(correct_num = sum(correct), belief50_num = sum(belief50), wrong_num = sum(wrong), no_change_num = sum(no_change),
            treat, treat_aggregate_signal, test_correct, crt1_correct, crt2_correct, crt3_correct, crt4_correct, crt_score, age, occ, edu, prob_fam, gender, 
            country, test1, test2, test3, test4, test5, test6, payoff, sr_button_clicks,
            belief_strategy, belief_optimal, belief_fake_blue, belief_fake_red, text_choice, duration, duration_min,
            prolific_id)

df_subject <- df_subject[!duplicated(df_subject$id), ]


# Categorizing outliers
######################################################

df_subject$outlier <- 0
df_subject <- mutate(df_subject, outlier = ifelse(treat_aggregate_signal == 0 & belief50_num >= 8, 1, outlier))
df_subject <- mutate(df_subject, outlier = ifelse(treat_aggregate_signal == 0 & wrong_num> 6, 1, outlier))
df_subject <- mutate(df_subject, outlier = ifelse(treat_aggregate_signal == 0 & no_change_num>= 8, 1, outlier))

df_subject <- mutate(df_subject, outlier = ifelse(treat_aggregate_signal == 1 & belief50_num > 6, 1, outlier))
df_subject <- mutate(df_subject, outlier = ifelse(treat_aggregate_signal == 1 & wrong_num>= 5, 1, outlier))
df_subject <- mutate(df_subject, outlier = ifelse(treat_aggregate_signal == 1 & no_change_num> 6, 1, outlier))

df_subject <- mutate(df_subject, outlier = ifelse(duration < 360 & !is.na(duration), 1, outlier))

df_subject %>%
  group_by(treat_aggregate_signal) %>%
  summarise(outlier_num = sum(outlier))
  

# Removing outliers from main analysis data
df_outliers <- df_subject %>% select(id, outlier)
df_long <- merge(df_long, df_outliers)
df_main <- df_long[df_long$outlier == 0, ]

# Remove prolific id
df_main <- subset(df_main,select=-c(prolific_id))


# Creating additional datasets
######################################################

# Creating data set - regular updating
df_regular <- df_main[df_main$verify_round == 0, ]

# Creating data set - retraction updating
df_retract <- df_main[df_main$ver_retract == 1, ]
df_retract <- df_retract[!is.na(df_retract$id), ]

# Creating data set - confirmation updating
df_confirm <- df_main[df_main$ver_retract == 0, ]
df_confirm <- df_confirm[!is.na(df_confirm$id), ]

df_confirm$signal_adj <- 1 - df_confirm$signal_adj

# Creating data set - informative updating
df_informative <- df_main[df_main$aggregate_informative == 1, ]
df_informative <- df_informative[!is.na(df_informative$id), ]

# Creating data set - uninformative updating
df_uninformative <- df_main[df_main$aggregate_informative == 0, ]
df_uninformative <- df_uninformative[!is.na(df_uninformative$id), ]

# Creating data set - uninformative updating
df_treat_aggregate <- df_main[df_main$treat_aggregate_signal == 1, ]
df_treat_aggregate <- df_treat_aggregate[!is.na(df_treat_aggregate$id), ]



# Finalizing datasets
######################################################

# Saving main file
save(df_main, file = paste0(path, "\\data_main.rda"))
write.csv(df_main, file = paste0(path, "\\data_main.csv"), row.names = FALSE)

# Saving subject data file
save(df_subject, file = paste0(path, "\\data_subject.rda"))
write.csv(df_subject, file = paste0(path, "\\data_subject.csv"), row.names = FALSE)

# Saving time data file
save(df_time, file = paste0(path, "\\data_time.rda"))
write.csv(df_time, file = paste0(path, "\\data_time.csv"), row.names = FALSE)

# Saving data file for regular updating
save(df_regular, file = paste0(path, "\\data_regular.rda"))
write.csv(df_regular, file = paste0(path, "\\data_regular.csv"), row.names = FALSE)

# Saving data file for info retraction
save(df_retract, file = paste0(path, "\\data_retract.rda"))
write.csv(df_retract, file = paste0(path, "\\data_retract.csv"), row.names = FALSE)

# Saving data file for info confirmation
save(df_confirm, file = paste0(path, "\\data_confirm.rda"))
write.csv(df_confirm, file = paste0(path, "\\data_confirm.csv"), row.names = FALSE)

# Saving data file for informative signals
save(df_informative, file = paste0(path, "\\data_informative.rda"))
write.csv(df_informative, file = paste0(path, "\\data_informative.csv"), row.names = FALSE)

# Saving data file for uninformative signals
save(df_uninformative, file = paste0(path, "\\data_uninformative.rda"))
write.csv(df_uninformative, file = paste0(path, "\\data_uninformative.csv"), row.names = FALSE)

# Saving complete data file - wide
save(df_wide_all, file = paste0(path, "\\data_complete_wide.rda"))
write.csv(df_wide_all, file = paste0(path, "\\data_complete_wide.csv"), row.names = FALSE)

# Saving data file for all aggregate signal treatment values
save(df_treat_aggregate, file = paste0(path, "\\data_treat_agg_signal.rda"))
write.csv(df_treat_aggregate, file = paste0(path, "\\data_treat_agg_signal.rda"), row.names = FALSE)


