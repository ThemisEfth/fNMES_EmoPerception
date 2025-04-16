# Libraries
pacman::p_load(tidyverse)

# Clear all
rm(list=ls())

# import data from Qualtrics
qual <- read_csv('./data/clean_qual.csv') %>% 
  mutate(qual_ID = as.factor(qual_ID))

# import session notes 
sess <- read_csv('./data/session.csv') %>% 
  filter(completed > 0) %>% 
  mutate(qual_ID = as.factor(qual_ID))

# import and concatenate psychopy files
myfiles = list.files(path = "./data/dat_behav", pattern="*.csv", full.names=TRUE)
dat_csv = plyr::ldply(myfiles, read_csv)

# condense the dataframe and remove the practice trials
df <- dat_csv %>% 
  filter(trial_no != "practice" & 
           grouping != 'NA' & 
           real_trials.thisN != 'del' &  real_trials.thisN != '1000') %>% 
  select(participant, 
         gender, 
         age, 
         avatar, 
         emo, 
         emotion, 
         nmes = NMES, 
         response.rt, 
         response = participant_Resp, 
         trial = real_trials.thisN, 
         LEFT, 
         RIGHT) %>% 
  arrange(participant) 

check_trials <- 
  df %>% 
  group_by(participant) %>% 
  summarise(n())

# We will be merging our EEG data but the trials from their start from 1 - 650, whereas the Psychopy files start from 0. Lets fix this by adding 1 to each trial.
df <- mutate(df,
             trial = as.numeric(trial),
             trial = 1 + trial)

# add blocks (every 130 trials there was a break)
df <- mutate(df,
             block = case_when(
               (trial <= 130) ~ 1,
               (trial > 130 & trial <= 260) ~ 2,
               (trial > 260 & trial <= 390) ~ 3,
               (trial > 390 & trial <= 520) ~ 4,
               (trial > 520 & trial <= 650) ~ 5) )

# create correct answer
df <- mutate(df, 
             cor = case_when(
               (emo == 'happy' & response == 'happy') ~ 'correct',
               (emo == 'sad' & response == 'sad') ~ 'correct', 
               (emo == 'sad' & response == 'happy') ~ 'incorrect',
               (emo == 'happy' & response == 'sad') ~ 'incorrect',
               (response == 'miss') ~ 'no_resp') )

df <- df %>%
  mutate(
    intensity = case_when(
      emotion %in% c(10, -10) ~ 1,
      emotion %in% c(20, -20) ~ 2,
      emotion %in% c(30, -30) ~ 3,
      TRUE ~ NA_real_),
    intensity = as.numeric(intensity) )

# add a variable telling the ambiguity of the task-relevant dimension
df <- df %>% 
  mutate(
    ambiguity = case_when(
      intensity == 1 ~ 1,
      intensity == 2 ~ 0.5,
      intensity == 3 ~ 0))

# Fix Dataftame-----------------------------------------------------------------

## fix subjects gender
df <- mutate(df, 
             gender = case_when(
               (gender == '0') ~ 'female',
               (gender == '1') ~ 'male',
               (gender == 'male') ~ 'male',
               (gender == 'female') ~ 'female') )

## Create a new variable with the avatar gender
df <- mutate(df,
             avatar = as.numeric(avatar),
             av_gender = if_else((avatar %% 2) == 0, 'm', 'f'),
             av_gender = as.factor(av_gender) )

## Make adjustments - off meant 0 mV
df$LEFT[df$nmes == "off"] <- 0     
df$RIGHT[df$nmes == "off"] <- 0 

## Set voltages to mA
df <- mutate(df, 
             left_ma = as.numeric(LEFT) * 0.05,
             right_ma = as.numeric(df$RIGHT) * 0.05,
             nmes_int = (left_ma + right_ma) / 2) %>% 
  select(-LEFT, -RIGHT)

# Merge dfs
qual_n_sess <- merge(qual, sess, by =  'qual_ID')
df <- merge(qual_n_sess, df, by = 'participant')

## Create PANAS-----------------------------------------------------------------
PANAS_1 <- 
  mutate(dat_csv, form_2.response = as.numeric(form_2.response)) %>% 
  select(participant, 
         age, 
         gender,
         word = form_2.itemText,
         panas = form_2.response) %>%
  na.exclude

PANAS_2 <- 
  mutate(dat_csv, form.response = as.numeric(form.response)) %>% 
  select(participant, 
         age, gender,
         word = form.itemText,
         panas = form.response) %>%
  na.exclude

PANAS_1 <- reshape(PANAS_1, 
                   idvar = c("participant", "age", "gender"),
                   timevar = c("word"), direction = "wide")

PANAS_2 <- reshape(PANAS_2, 
                   idvar = c("participant", "age", "gender"),
                   timevar = c("word"), direction = "wide")

PANAS_1 %>%
  select(panas.Interested, 
         panas.Excited,
         panas.Strong,
         panas.Enthusiastic,
         panas.Proud, 
         panas.Alert,
         panas.Inspired,
         panas.Determined,
         panas.Active,
         panas.Attentive) %>%
  psych::alpha(title = "Pos_aff_1")
# 0.87 - time 1
# 0.9 - time 2

PANAS_2 %>%
  select(panas.Distressed,
         panas.Upset,
         panas.Guilty,
         panas.Scared,
         panas.Hostile,
         panas.Irritable,
         panas.Ashamed,
         panas.Nervous,
         panas.Jittery,
         panas.Afraid) %>%
  psych::alpha(title = "Neg_aff_1")
# 0.68 - time 1
# 0.61 - time 2 

###  Positive Affect Score: Add the scores on items 1, 3, 5, 9, 10, 12, 14, 16, 17, and 19
PANAS_1$Pos_Aff <- rowSums(cbind(PANAS_1$panas.Interested, 
                                 PANAS_1$panas.Excited,
                                 PANAS_1$panas.Strong,
                                 PANAS_1$panas.Enthusiastic,
                                 PANAS_1$panas.Proud, 
                                 PANAS_1$panas.Alert,
                                 PANAS_1$panas.Inspired,
                                 PANAS_1$panas.Determined,
                                 PANAS_1$panas.Active,
                                 PANAS_1$panas.Attentive, na.rm = T))

PANAS_2$Pos_Aff <- rowSums(cbind(PANAS_2$panas.Interested, 
                                 PANAS_2$panas.Excited,
                                 PANAS_2$panas.Strong,
                                 PANAS_2$panas.Enthusiastic,
                                 PANAS_2$panas.Proud, 
                                 PANAS_2$panas.Alert,
                                 PANAS_2$panas.Inspired,
                                 PANAS_2$panas.Determined,
                                 PANAS_2$panas.Active,
                                 PANAS_2$panas.Attentive, na.rm = T))

### Negative Affect Score: Add the scores on items 2, 4, 6, 7, 8, 11, 13, 15, 18, and 20.
PANAS_1$Neg_Aff <- rowSums(cbind(PANAS_1$panas.Distressed,
                                 PANAS_1$panas.Upset,
                                 PANAS_1$panas.Guilty,
                                 PANAS_1$panas.Scared,
                                 PANAS_1$panas.Hostile,
                                 PANAS_1$panas.Irritable,
                                 PANAS_1$panas.Ashamed,
                                 PANAS_1$panas.Nervous,
                                 PANAS_1$panas.Jittery,
                                 PANAS_1$panas.Afraid, na.rm = T))

PANAS_2$Neg_Aff <- rowSums(cbind(PANAS_2$panas.Distressed,
                                 PANAS_2$panas.Upset,
                                 PANAS_2$panas.Guilty,
                                 PANAS_2$panas.Scared,
                                 PANAS_2$panas.Hostile,
                                 PANAS_2$panas.Irritable,
                                 PANAS_2$panas.Ashamed,
                                 PANAS_2$panas.Nervous,
                                 PANAS_2$panas.Jittery,
                                 PANAS_2$panas.Afraid, na.rm = T))

reduced_1 <-select(PANAS_1, 
                   participant, 
                   Pos_aff_1 = Pos_Aff, 
                   Neg_aff_1 = Neg_Aff)

reduced_2 <- select(PANAS_2, 
                    participant, 
                    Pos_aff_2 = Pos_Aff, 
                    Neg_aff_2 = Neg_Aff)

panas_comb <- merge(reduced_1, reduced_2, by = 'participant')

### reshape PANAS for plots
panas <- reshape2::melt(panas_comb,
                        id.vars = c("participant"),
                        variable.name = "PANAS",
                        value.name = "PANAS_score")

df <- merge(df, panas_comb, by = 'participant')

##  Smile rating----------------------------------------------------------------
smile <- dat_csv %>% 
  select(participant, 
         rate_smile = rate_smile.response) %>% 
  na.exclude() %>% 
  mutate(rate_smile = as.numeric(rate_smile))

df <- merge(df, smile, by = 'participant')

## Split comfort into a separate dataframe and remove NAs-----------------------
comfort <- dat_csv %>% 
  select(participant, 
         discomfort = comfort_rating.response) %>% 
  na.exclude() %>% 
  mutate(discomfort = as.numeric(discomfort))

df <- merge(df, comfort, by = "participant")

# Data cleaning, print number of trials with no response (i.e. missed)----------
miss_percent_each_participant <- df %>%
  filter(emotion != 'none') %>% 
  group_by(participant) %>% 
  summarise(miss_count = sum(response == "miss"),
            total_count = n()) %>%
  mutate(miss_percent_ind = round(miss_count / total_count * 100, 2))

miss_df <- df %>%
  filter(emotion != 'none') %>% 
  summarise(miss_count = sum(response == "miss"),
            total_count = n()) %>%
  mutate(miss_percent = round(miss_count / total_count * 100, 2))

miss_df
# .3%

## Percent RTs less than 100 ms-------------------------------------------------
df %>%
  filter(response != 'miss' & emotion != 'none') %>% 
  mutate(under = response.rt < 0.1) %>%
  summarise(under_count = sum(under),
            total_count = n()) %>%
  mutate(under_percent = round(under_count / total_count * 100, 2)) %>%
  pull(under_percent) # = 2.49

## find trials with RT > 3*SD--------------------------------------------------
df %>%
  filter(response != 'miss' & emotion != 'none') %>% 
  mutate(mean_rt = mean(response.rt, na.rm = TRUE),
         sd_rt = sd(response.rt, na.rm = TRUE)) %>%
  mutate(over = response.rt > mean_rt + 3 * sd_rt) %>%
  summarise(over_count = sum(over),
            total_count = n()) %>%
  mutate(over_percent = round(over_count / total_count * 100, 2)) %>% 
  pull(over_percent) # = 2.18

# Sum total outlier %
sum(.3, 2.49, 2.18)

# Sum RT outlier %
sum(2.49, 2.18)

## ## Mark outliers for removal---------------------------------------------------
mean_rt <- mean(df$response.rt, na.rm=TRUE)
sd_rt <- sd(df$response.rt, na.rm=TRUE)

df$outlier_RT <- ifelse(!is.na(df$response.rt) & df$response.rt < 0.1, 1,
                        ifelse(!is.na(df$response.rt) & df$response.rt >
                                 ((3*sd(df$response.rt, na.rm=T)) +
                                    mean(df$response.rt, na.rm=T) ), 1, 0 ))


df$log_rt <- log(df$response.rt)

## Update dataframe--------------------
df <-  
  mutate(df, emotion = case_when(
    (emotion == '-30') ~ '-3',
    (emotion == '-20') ~ '-2', 
    (emotion == '-10') ~ '-1',
    (emotion == '10') ~ '1',
    (emotion == '20') ~ '2',
    (emotion == '30') ~ '3',
    (emotion == 'none') ~ '999') )

# Order levels so that 'off' is first, followed by on
df$nmes <- factor(df$nmes, levels = c("off", "on"))
df$participant <- as.factor(df$participant)

# Descriptives after cleaning - Print remaining trials for each participant after removing extreme RT trials and nmes only condition. 
# This mean it is now out of 600 trials where a face was displayed.
filter(df, outlier_RT != 1 & emo != 'none') %>% 
  group_by(participant) %>%
  summarise(remaining_trials = n()) %>%
  mutate(remaining_trials_pct = remaining_trials/600*100) %>%
  ggplot(aes(x = participant, y = remaining_trials)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(as.integer(remaining_trials_pct),"%")),
            vjust = -1, size = 2.5, color = "purple", angle=60) +
  xlab("Participant") +
  ylab("Remaining Trials after removing outliers")

check_trials<- 
  df %>% 
  group_by(participant) %>% 
  summarise(n())

# export DF---------------------------------------------------------------------
df <- df %>% select(-qual_ID, -completed, -right_mt, -left_mt)

write_csv(df, "./data/df_without_eeg.csv")

# Merge P100 data---------------------------------------------------------------
EEG_P1 <- read_csv('../data/amplitudes_fully_cleaned_P1.csv') %>% 
  mutate(participant = as.numeric(participant),
         TrialType = fct_recode(as.factor(TrialType),
                                'sad/-1/off' = "1",
                                'sad/-2/off' = "2",
                                'sad/-3/off' = "3",
                                'sad/-1/on' = "4",
                                'sad/-2/on' = "5",
                                'sad/-3/on' = "6",
                                'happy/1/off' = "7",
                                'happy/2/off' = "8",
                                'happy/3/off' = "9",
                                'happy/1/on' = "10",
                                'happy/2/on' = "11",
                                'happy/3/on' = "12",
                                'none/999/on' = "999")) %>%
  separate(TrialType, c("emo", "emotion", "nmes"), "/") %>% 
  rename(quality_P1 = quality, 
         trial = trial,
         P1 = amplitude) 

# Merge N170 data-------------------------------------------------------------
EEG_N170_left <- read_csv('../data/amplitudes_fully_cleaned_N170L.csv') %>% 
  mutate(participant = as.numeric(participant),
         TrialType = fct_recode(as.factor(TrialType),
                                'sad/-1/off' = "1",
                                'sad/-2/off' = "2",
                                'sad/-3/off' = "3",
                                'sad/-1/on' = "4",
                                'sad/-2/on' = "5",
                                'sad/-3/on' = "6",
                                'happy/1/off' = "7",
                                'happy/2/off' = "8",
                                'happy/3/off' = "9",
                                'happy/1/on' = "10",
                                'happy/2/on' = "11",
                                'happy/3/on' = "12",
                                'none/999/on' = "999")) %>%
  separate(TrialType, c("emo", "emotion", "nmes"), "/") %>% 
  rename(quality_N170_left = quality, 
         trial = trial, 
         N170_left = amplitude) 

df_eeg <- merge(EEG_P1, EEG_N170_left, by = c('participant', 'trial', 'emo', 'emotion', 'nmes'))
# Merge N170 data-------------------------------------------------------------
EEG_N170_right <- read_csv('../data/amplitudes_fully_cleaned_N170R.csv') %>% 
  mutate(participant = as.numeric(participant),
         TrialType = fct_recode(as.factor(TrialType),
                                'sad/-1/off' = "1",
                                'sad/-2/off' = "2",
                                'sad/-3/off' = "3",
                                'sad/-1/on' = "4",
                                'sad/-2/on' = "5",
                                'sad/-3/on' = "6",
                                'happy/1/off' = "7",
                                'happy/2/off' = "8",
                                'happy/3/off' = "9",
                                'happy/1/on' = "10",
                                'happy/2/on' = "11",
                                'happy/3/on' = "12",
                                'none/999/on' = "999")) %>%
  separate(TrialType, c("emo", "emotion", "nmes"), "/") %>% 
  rename(quality_N170_right = quality, 
         trial = trial, 
         N170_right = amplitude) 

df_eeg <- merge(df_eeg, EEG_N170_right, by = c('participant', 'trial', 'emotion', 'emo', 'nmes'))
# Merge LPP data-------------------------------------------------------------
EEG_LPP <- read_csv('../data/amplitudes_fully_cleaned_LPP.csv') %>% 
  mutate(participant = as.numeric(participant),
         TrialType = fct_recode(as.factor(TrialType),
                                'sad/-1/off' = "1",
                                'sad/-2/off' = "2",
                                'sad/-3/off' = "3",
                                'sad/-1/on' = "4",
                                'sad/-2/on' = "5",
                                'sad/-3/on' = "6",
                                'happy/1/off' = "7",
                                'happy/2/off' = "8",
                                'happy/3/off' = "9",
                                'happy/1/on' = "10",
                                'happy/2/on' = "11",
                                'happy/3/on' = "12",
                                'none/999/on' = "999")) %>%
  separate(TrialType, c("emo", "emotion", "nmes"), "/") %>% 
  rename(quality_LPP = quality, 
         trial = trial, 
         LPP = amplitude) 

df_eeg <- left_join(df_eeg, EEG_LPP, by = c('participant', 'trial', 'emo', 'emotion', 'nmes'))

# Export data---------------------------------------------------------------

# export DF with no behavioural data
df_eeg %>% 
  write_csv("../data/df_eeg.csv")

# export DF with behavioural data

df_eeg <- merge(df_eeg, df, by = c('participant', 'trial', 'nmes')) 
  
write_csv(df_eeg, "../data/df_behav_and_eeg.csv")

# Video analysis--------------------------------------------------------------------
openface <- read_csv('../data/clean_openface.csv') %>% 
  mutate(., 
         nmes = case_when(
           (nmes == 'OFF') ~ 'off',
           (nmes == 'ON') ~ 'on' ) ) %>% 
  select(., -1) %>% 
  mutate(trial = as.character(trial))

# Remove leading zeroes from participant numbers
openface$participant <- gsub("^0+", "", openface$participant)

# remove subjects 3 trials after 476 as the videos are not in sync.
openface <- mutate(openface, flag_bad_videos = ifelse((participant == 3 & trial > 476), 0, 1))

openface$participant <- as.numeric(openface$participant)
openface$participant <- sort(openface$participant)
openface$participant <- factor(openface$participant) 

write_csv(openface, '../Data/openface_raw.csv')

openface <- read_csv('../Data/openface_raw.csv')

plot_raw_of <- 
  openface %>% 
  filter(confidence >= .95) %>% 
  # filter(nmes == 'on') %>% 
  # filter(participant == '30') %>%
  # filter(!participant %in% c('11', '6', '34', '39', '5', '13', '21', '30')) %>%
  mutate(AU12_r = as.numeric(AU12_r),
         nmes = as.factor(nmes),
         frame = as.numeric(frame)) %>% 
  group_by(nmes, frame) %>% 
  summarise(AU12 = mean(AU12_r)) %>% 
  ggplot(aes(x = frame, y = AU12, color = nmes)) +
  geom_line()

ggplotly(plot_raw_of)

plot_raw_of <- 
  openface %>% 
  filter(confidence >= .95) %>% 
  filter(nmes == 'on') %>% 
  # filter(participant == '30') %>%
  # filter(!participant %in% c('11', '6', '34', '39', '5', '13', '21', '30')) %>%
  mutate(AU12_r = as.numeric(AU12_r),
         nmes = as.factor(nmes),
         frame = as.numeric(frame)) %>% 
  group_by(nmes, frame, participant) %>% 
  summarise(AU12 = mean(AU12_r)) %>% 
  ggplot(aes(x = frame, y = AU12, color = participant)) +
  geom_line()

ggplotly(plot_raw_of)

# Correct the baseline by taking the average of the first 15 frames (1000 ms)

## Baseline correct using the average for the first 15 frames 
data = openface

##  Create an empty data frame to store the corrected data
corrected_data <- data.frame()

# Group the data by participant, frame, muscle, intensity
grouped_data <- data %>% 
  select(participant, timestamp, confidence, frame, nmes, trial,
         AU02_r,
         AU04_r,
         AU05_r,
         AU06_r,
         AU09_r,
         AU10_r,
         AU12_r,
         AU14_r,
         AU15_r,
         AU17_r,
         AU20_r,
         AU23_r,
         AU25_r,
         AU26_r) %>% 
  group_by(participant, nmes, trial)

# Calculate the mean of the first 7 values of the frame variable for each group
baseline_mean <- grouped_data %>% 
  filter(frame >= 0, frame <= 15) %>% 
  summarise(baseline_mean_AU02 = mean(AU02_r),
            baseline_mean_AU04 = mean(AU04_r), 
            baseline_mean_AU05 = mean(AU05_r),
            baseline_mean_AU06 = mean(AU06_r),
            baseline_mean_AU09 = mean(AU09_r),
            baseline_mean_AU10 = mean(AU10_r),
            baseline_mean_AU12 = mean(AU12_r),
            baseline_mean_AU14 = mean(AU14_r),
            baseline_mean_AU15 = mean(AU15_r),
            baseline_mean_AU17 = mean(AU17_r), 
            baseline_mean_AU20 = mean(AU20_r),
            baseline_mean_AU23 = mean(AU23_r),
            baseline_mean_AU25 = mean(AU25_r),
            baseline_mean_AU26 = mean(AU26_r)
  )

# Join the baseline mean to the grouped data
corrected_data <- left_join(grouped_data, 
                            baseline_mean, by = c("participant", "nmes", "trial"))

# Subtract the baseline mean from each subsequent frame's AU12_r and AU15_r value
corrected_data <- corrected_data %>% 
  mutate(AU02 = AU02_r - baseline_mean_AU02,
         AU04 = AU04_r - baseline_mean_AU04,
         AU05 = AU05_r - baseline_mean_AU05,
         AU06 = AU06_r - baseline_mean_AU06,
         AU09 = AU09_r - baseline_mean_AU09,
         AU10 = AU10_r - baseline_mean_AU10,
         AU12 = AU12_r - baseline_mean_AU12,
         AU14 = AU14_r - baseline_mean_AU14,
         AU15 = AU15_r - baseline_mean_AU15,
         AU17 = AU17_r - baseline_mean_AU17,
         AU20 = AU20_r - baseline_mean_AU20,
         AU23 = AU23_r - baseline_mean_AU23,
         AU25 = AU25_r - baseline_mean_AU25,
         AU26 = AU26_r - baseline_mean_AU26)

write_csv(corrected_data, '../Data/openface_corrected.csv')

corrected_data <- read_csv('../Data/openface_corrected.csv')

## Then average AU12_r from frame 15:30

# Create an empty data frame to store the results
results_df <- data.frame(participant = integer(),
                         nmes = character(),
                         trial = numeric(),
                         AU12 = numeric(),
                         AU06 = numeric(),
                         AU04 = numeric(),
                         AU15 = numeric(),
                         stringsAsFactors = FALSE)

# Loop through each participant
for(i in unique(corrected_data$participant)) {
  # Create an empty data frame for each participant
  participant_df <- data.frame(participant = integer(),
                               nmes = character(),
                               trial = numeric(),
                               AU12 = numeric(),
                               AU06 = numeric(),
                               AU04 = numeric(),
                               AU15 = numeric(),
                               stringsAsFactors = FALSE)
  
  # Subset the data for the current participant
  data_subset <- corrected_data[corrected_data$participant == i, ]
  
  for(j in unique(data_subset$nmes)) {
    # Subset the data for the current nmes level
    data_subset_j <- data_subset[data_subset$nmes == j, ]
    
    for (k in unique(data_subset_j$trial)) {
      # Subset the data for the current trial
      data_subset_k <- data_subset_j[data_subset_j$trial == k, ]
      
      # Subset the data between frame 15 and 30
      data_subset_k <- data_subset_k[data_subset_k$frame >= 15 & data_subset_k$frame <= 30, ]
      
      # Calculate the mean of the subsetted data
      if (nrow(data_subset_k) > 0) {
        mean_corrected_AU12_r <- mean(data_subset_k$AU12, na.rm = TRUE)
        mean_corrected_AU06_r <- mean(data_subset_k$AU06, na.rm = TRUE)
        mean_corrected_AU04_r <- mean(data_subset_k$AU04, na.rm = TRUE)
        mean_corrected_AU15_r <- mean(data_subset_k$AU15, na.rm = TRUE)
      } 
      
      # Append the results to the participant-specific data frame
      participant_df <- rbind(participant_df, data.frame(participant = i, 
                                                         trial = k,
                                                         nmes = j,
                                                         AU12 = mean_corrected_AU12_r,
                                                         AU06 = mean_corrected_AU06_r,
                                                         AU04 = mean_corrected_AU04_r,
                                                         AU15 = mean_corrected_AU15_r))
    }
  }
  
  # Append the participant-specific data frame to the overall results data frame
  results_df <- rbind(results_df, participant_df)
}

results_df <-  mutate(results_df, trial = as.numeric(trial),
                      participant = as.numeric(participant),
                      flag_bad_videos = ifelse((participant == 3 & trial > 476), 0, 1))

write_csv(results_df, '../Data/openface_averaged.csv')

