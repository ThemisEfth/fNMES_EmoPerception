# Dr Matteo's Lisi function to extract both group PSE's----
extract_i_PSE <- function(m){
  
  require(tidyverse)
  
  FX <- fixef(m)
  RX <- ranef(m)$participant
  dat <- m@frame
  
  # group-level parameters
  par_group <- expand.grid(nmes = unique(dat$nmes),
                           KEEP.OUT.ATTRS = F)
  par_group$bias <- NA
  par_group$noise <- NA
  
  # individual-level parameters
  par_i <- expand.grid(participant = 1:nrow(RX),
                       nmes = unique(dat$nmes),
                       KEEP.OUT.ATTRS = F)
  par_i$bias <- NA
  par_i$noise <- NA
  
  # loop over condition and calculate parameters
  for(k in 1:nrow(par_group)){
    
    # str_c = Join multiple strings into a single string
    nmes <- str_c("nmes", par_group$nmes[k])
    
  # get intercept estimate from the model of each nmes without emotion_c
    intercept_index <- which((str_detect(names(FX), nmes) ) 
                             & !str_detect(names(FX),"emotion_c"))
  # get slope estimate from the model of each nmes with emotion_c  
    slope_index <- which((str_detect(names(FX), nmes) ) 
                         & str_detect(names(FX), "emotion_c"))
    
    ## get the average PSEs of all participants
      # -intercept + sum of all intercepts / emotion_c + sum of all slopes
    par_group$bias[k] <- -(FX[1]+sum(FX[intercept_index]))/(FX[2]+sum(FX[slope_index]))
      # 1 / emotion_c + sum of all slopes
    par_group$noise[k] <- 1/(FX[2]+sum(FX[slope_index]))
    
    ## get the individual PSEs of all participants
    for(i in 1:nrow(par_i)){
      index <- which(par_i$participant==i & par_i$nmes==par_group$nmes[k])
      # -(intercept + sum of all intercepts + individual participant random intercept) / 
      # (fixed effect of emotion + sum of slopes + individual slope of emotion_c) 
      par_i$bias[index] <- -(FX[1]+sum(FX[intercept_index])+RX[i,1])/(FX[2]+sum(FX[slope_index])+RX[i,2])
      par_i$noise[index] <- 1/(FX[2]+sum(FX[slope_index])+RX[i,2])
    }
  }
  
  individual_parameters <- par_i

  return(individual_parameters)
}

# Dr Matteo's Lisi function to extract both group PSE's-----
extract_g_PSE <- function(m){
  
  require(tidyverse)
  
  FX <- fixef(m)
  RX <- ranef(m)$participant
  dat <- m@frame
  
  # group-level parameters
  par_group <- expand.grid(nmes = unique(dat$nmes),
                           KEEP.OUT.ATTRS = F)
  par_group$bias <- NA
  par_group$noise <- NA
  
  # individual-level parameters
  par_i <- expand.grid(participant=1:nrow(RX),
                       nmes = unique(dat$nmes),
                       KEEP.OUT.ATTRS = F)
  par_i$bias <- NA
  par_i$noise <- NA
  
  # loop over condition and calculate parameters
  for(k in 1:nrow(par_group)){
    
    # str_c = Join multiple strings into a single string
    nmes <- str_c("nmes", par_group$nmes[k])
    
    # get intercept estimate from the modelß of each nmes without emotion_c
    intercept_index <- which((str_detect(names(FX),nmes) ) 
                             & !str_detect(names(FX),"emotion_c"))
    # get slope estimate from the model of each nmes with emotion_c  
    slope_index <- which((str_detect(names(FX),nmes) ) 
                         & str_detect(names(FX),"emotion_c"))
    
    ## get the average PSEs of all participants
    # -intercept + sum of all intercepts / emotion_c + sum of all slopes
    par_group$bias[k] <- -(FX[1]+sum(FX[intercept_index]))/(FX[2]+sum(FX[slope_index]))
    # 1 / emotion_c + sum of all slopes
    par_group$noise[k] <- 1/(FX[2]+sum(FX[slope_index]))
    
    ## get the individual PSEs of all participants
    for(i in 1:nrow(par_i)){
      index <- which(par_i$participant==i & par_i$nmes==par_group$nmes[k])
      # -(intercept + sum of all intercepts + individual participant random intercept) / 
      # (fixed effect of emotion + sum of slopes + individual slope of emotion_c) 
      par_i$bias[index] <- -(FX[1]+sum(FX[intercept_index])+RX[i,1])/(FX[2]+sum(FX[slope_index])+RX[i,2])
      par_i$noise[index] <- 1/(FX[2]+sum(FX[slope_index])+RX[i,2])
    }
  }
  
  group_parameters <- par_group
  
  return(group_parameters)
}

# function for t.tests
perform_paired_t_test <- function(group1, group2) {
  # Perform paired t-test
  t_test_result <- t.test(group1, group2, paired = TRUE)
  
  # Extract relevant information from the t-test result
  p_value <- t_test_result$p.value
  test_statistic <- t_test_result$statistic
  degrees_of_freedom <- t_test_result$parameter
  
  # Create a named list of the test results
  test_results <- list(
    p_value = p_value,
    test_statistic = test_statistic,
    degrees_of_freedom = degrees_of_freedom
  )
  
  return(test_results)
}

# function to export correlation results as a table
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# Correlation Matrix----

# Function was taken from Paul van der Laken
# https://paulvanderlaken.com/2020/07/28/publication-ready-correlation-matrix-significance-r/
# And has been adapted to include a Holmes correction

correlation_matrix <- function(df, 
                               type = "spearman",
                               digits = 3, 
                               decimal.mark = ".",
                               use = "all", 
                               show_significance = TRUE, 
                               replace_diagonal = FALSE, 
                               replacement = ""){
  
  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)
  require(RcmdrMisc)
  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]
  
  # transform input data frame to matrix
  x <- as.matrix(df)
  
  # run correlation analysis using Hmisc package
  correlation_matrix1 <- rcorr.adjust(x, type = )
  correlation_matrix2 <- rcorr(x, type = )
  R <- correlation_matrix2$r # Matrix of correlation coefficients
  p <- correlation_matrix1$P # Matrix of p-value 
  
  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)
  
  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(R < 0) > 0) {
    Rformatted = ifelse(R > 0, paste0(' ', Rformatted), Rformatted)
  }
  
  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "   ", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
    Rformatted = paste0(Rformatted, stars)
  }
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep =" ")
  
  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }
  
  return(Rnew)
}

# compute pure emotion---------------------------------------------------------
compute_emotion <- function(emotion, exclude_emotions) {
  dat2 %>%
    filter(!emotion %in% exclude_emotions) %>%
    left_join(mean_10_perc, by = c("participant", "nmes")) %>%
    group_by(participant, emotion, nmes) %>%
    summarise(P1 = mean(P1),
              N170_left = mean(N170_left),
              N170_right = mean(N170_right),
              LPP = mean(LPP),
              mean_P1 = mean(mean_P1),
              mean_N170_left = mean(mean_N170_left),
              mean_N170_right = mean(mean_N170_right),
              mean_LPP = mean(mean_LPP),
              response_n = mean(response_n),
              mean_resp = mean(mean_resp)) %>%
    mutate(P1 = (P1[nmes == 'on'] - mean_P1[nmes == 'on']) - (P1[nmes == 'off'] - mean_P1[nmes == 'off']),
           N170_left = (N170_left[nmes == 'on'] - mean_N170_left[nmes == 'on']) - (N170_left[nmes == 'off'] - mean_N170_left[nmes == 'off']),
           N170_right = (N170_right[nmes == 'on'] - mean_N170_right[nmes == 'on']) - (N170_right[nmes == 'off'] - mean_N170_right[nmes == 'off']),
           LPP = (LPP[nmes == 'on'] - mean_LPP[nmes == 'on']) - (LPP[nmes == 'off'] - mean_LPP[nmes == 'off']),
           resp = (response_n[nmes == 'on'] - mean_resp[nmes == 'on']) - (response_n[nmes == 'off'] - mean_resp[nmes == 'off']),
           participant = as.factor(participant),
           emotion = !!paste0(emotion),
           emotion = as.factor(emotion)) %>% 
    select(participant, emotion, P1, N170_left, N170_right, LPP, resp) %>% 
    distinct(participant, .keep_all = TRUE)
}

# compute pure emotion---------------------------------------------------------
compute_emotion_2 <- function(emotion, exclude_emotions) {
  dat2 %>%
    filter(!emotion %in% exclude_emotions) %>%
    left_join(mean_10_perc, by = c("participant", "nmes")) %>%
    group_by(participant, emotion, nmes) %>%
    summarise(P1 = mean(P1, na.rm = T),
              N170_left = mean(N170_left, na.rm = T),
              N170_right = mean(N170_right, na.rm = T),
              LPP = mean(LPP, na.rm = T),
              mean_P1 = mean(mean_P1, na.rm = T),
              mean_N170_left = mean(mean_N170_left, na.rm = T),
              mean_N170_right = mean(mean_N170_right, na.rm = T),
              mean_LPP = mean(mean_LPP, na.rm = T),
              response_n = mean(response_n, na.rm = T),
              mean_resp = mean(mean_resp, na.rm = T)) %>%
    mutate(P1_on = (P1[nmes == 'on'] - mean_P1[nmes == 'on']),
           P1_off = (P1[nmes == 'off'] - mean_P1[nmes == 'off']),
           N170_left_on = (N170_left[nmes == 'on'] - mean_N170_left[nmes == 'on']),
           N170_left_off = (N170_left[nmes == 'off'] - mean_N170_left[nmes == 'off']),
           N170_right_on = (N170_right[nmes == 'on'] - mean_N170_right[nmes == 'on']),
           N170_right_off = (N170_right[nmes == 'off'] - mean_N170_right[nmes == 'off']),
           LPP_on = (LPP[nmes == 'on'] - mean_LPP[nmes == 'on']),
           LPP_off = (LPP[nmes == 'off'] - mean_LPP[nmes == 'off']),
           resp_on = (response_n[nmes == 'on'] - mean_resp[nmes == 'on']),
           resp_off = (response_n[nmes == 'off'] - mean_resp[nmes == 'off']),
           participant = as.factor(participant),
           emotion = !!paste0(emotion),
           emotion = as.factor(emotion)) %>% 
    select(participant, emotion, P1_on, P1_off, 
           N170_left_on, N170_left_off, 
           N170_right_on, N170_right_off,
           LPP_on, LPP_off,
           resp_on, resp_off) %>% 
    distinct(participant, .keep_all = TRUE)
}

# function for demographic information-----------------------------------------
get_demographic_info <- function(dat) {
  
  # Number of participants by gender
  gender_info <- dat %>%
    group_by(gender) %>%
    summarise(participant = n_distinct(participant),
              M_age = mean(age),
              SD_age = sd(age))
  
  #  Number of participants overall
  overall_info <- dat %>%
    summarise(participant = n_distinct(participant),
              M_age = mean(age),
              SD_age = sd(age))
  
  #  Summary of age
  age_summary <- summary(dat$age)
  
  # Check handedness of participants
  handedness_info <- dat %>%
    distinct(participant, .keep_all = TRUE) %>%
    group_by(handedness) %>%
    summarise(handedness_c = n())
  
  # NMES descriptives
  nmes_info <- dat %>%
    filter(nmes == "on") %>%
    select(nmes, nmes_int) %>%
    na.exclude %>%
    summarise(inten_M = mean(nmes_int),
              inten_SD = sd(nmes_int),
              inten_min = min(nmes_int),
              inten_max = max(nmes_int))
  
  # Belief in FFH
  belief_info <- dat %>%
    distinct(participant, .keep_all = TRUE) %>%
    mutate(belief = as.character(Belief)) %>%
    group_by(belief) %>%
    summarise(belief_ffh_c = n())
  
  belief_rating_info <- dat %>%
    distinct(participant, .keep_all = TRUE) %>%
    mutate(belief_rating = as.numeric(Belief_rating)) %>%
    summarise(M_belief = mean(belief_rating),
              SD_belief = sd(belief_rating))
  
  belief_nmes_info <- dat %>%
    distinct(participant, .keep_all = TRUE) %>%
    mutate(belief_fnmes = as.character(`Does NMES induce FFH?`)) %>%
    group_by(belief_fnmes) %>%
    summarise(belief_fnmes_c = n())
  
  guess_cor_info <- dat %>%
    distinct(participant, .keep_all = TRUE) %>%
    mutate(guess_cor = as.character(guess_cor)) %>%
    group_by(guess_cor) %>%
    summarise(count = n())
  
  # Combine all of the information into a single data frame
  demographic_info <- bind_rows(gender_info, overall_info, age_summary, handedness_info, nmes_info, belief_info, belief_rating_info, belief_nmes_info, guess_cor_info)
  
  # Return the data frame
  return(demographic_info)
}

