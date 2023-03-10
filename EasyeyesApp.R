setwd("./ArabicFontsA9_2023-3-9_17-19-26")
# set the experiment folder as working directory
# get all the files end with .csv in the folder
file_names <- list.files(pattern = "*.csv")
# use the length() function n number of participants in the experiment
n = length(file_names)
data_list = list()
# data processing
j = 1
max_trails <- 0
for (i in 1 : n){
  t <- tibble()
  try({t <- read.csv(file_names[i], stringsAsFactors=F)}, silent = TRUE)
  if (!('participant' %in% colnames(t))) {
    t <- tibble(participant = str_split(file_names[i], "[_]")[[1]][1])
    t$error <- "invalid file"
  }
  if (!('ProlificParticipantID' %in% colnames(t))) {
    t$ProlificParticipantID <- str_split(file_names[i], "[_]")[[1]][2]
  }
  if (!('error' %in% colnames(t))) {
    t$error <- ""
  }
  if ('readingPageWords' %in% colnames(t)) {
    t$wordPerMin <- (t$readingPageWords + t$readingLinesPerPage - 1) / (t$readingPageDurationOnsetToOffsetSec / 60)
  }
  if (!('readingPageWords' %in% colnames(t))) {
    t$wordPerMin <- NA
  }
  if (!('targetFinishSec' %in% colnames(t))) {
    t$targetFinishSec <- NA
  }
  if (!('targetStartSec' %in% colnames(t))) {
    t$targetStartSec <- NA
  }
  if (!('targetMeasuredDurationSec' %in% colnames(t))) {
    t$targetMeasuredDurationSec <- t$targetFinishSec-t$targetStartSec
  }
  if (!('targetMeasuredLatenessSec' %in% colnames(t))) {
    t$targetMeasuredLatenessSec <- NA
  }
  if (!('questMeanAtEndOfTrialsLoop' %in% colnames(t))) {
    t$questMeanAtEndOfTrialsLoop <- NA
  }
  if (!('screenHeightPx' %in% colnames(t))) {
    t$screenHeightPx <- NA
  }
  if (!('screenWidthPx' %in% colnames(t))) {
    t$screenWidthPx <- NA
  }
  if (!('deviceBrowser' %in% colnames(t))) {
    t$deviceBrowser <- ""
  }
  if (!('deviceBrowserVersion' %in% colnames(t))) {
    t$deviceBrowserVersion <- ""
  }
  if (!('deviceType' %in% colnames(t))) {
    t$deviceType <- ""
  }
  if (!('deviceSystemFamily' %in% colnames(t))) {
    t$deviceSystemFamily <- ""
  }
  if (!('deviceLanguage' %in% colnames(t))) {
    t$deviceLanguage <- ""
  }
  if (!('deviceSystem' %in% colnames(t))) {
    t$deviceSystem <- ""
  }
  if (!('hardwareConcurrency' %in% colnames(t))) {
    t$hardwareConcurrency <- NA
  }
  if (!('block' %in% colnames(t))) {
    t$block <- NA
  }
  if (!('conditionName' %in% colnames(t))) {
    t$conditionName <- ""
  }
  if (!('block_condition' %in% colnames(t))) {
    t$block_condition <- ''
  }
  if (!('staircaseName' %in% colnames(t))) {
    t$staircaseName <- NA
  }
  if (!('font' %in% colnames(t))) {
    t$font <- ""
  }
  if (!('targetTask' %in% colnames(t))) {
    t$targetTask <- ""
  }
  if (!('targetKind' %in% colnames(t))) {
    t$targetKind <- ""
  }
  if (!('psychojsWindowDimensions' %in% colnames(t))) {
    t$psychojsWindowDimensions <- NA
  }
  t <- t %>% mutate(screenWidthPx = t$screenWidthPx[1],
                    screenHeightPx = t$screenHeightPx[1],
                    browser = ifelse(deviceBrowser == "", "", paste0(deviceBrowser, 
                                                                     " ", 
                                                                     str_split(deviceBrowserVersion, "[.]")[[1]][1])),
                    resolution = paste0(screenWidthPx, " x ", screenHeightPx),
                    block_condition = ifelse(block_condition == "",staircaseName, block_condition))
  t$resolution = ifelse(t$resolution[1] == "NA x NA", t$psychojsWindowDimensions, t$resolution)
  info <- filter(t, is.na(questMeanAtEndOfTrialsLoop)) %>% select(block_condition, conditionName) %>% distinct(block_condition, conditionName)
  t <- select(t, -conditionName) %>% left_join(info, by = "block_condition")
  assign(paste0("data", j), t)
  
  info <- t %>% 
    filter(is.na(questMeanAtEndOfTrialsLoop)) %>%
    distinct(participant, block, staircaseName, conditionName, targetKind, font)
  
  summaries <- t %>% 
    filter(!is.na(questMeanAtEndOfTrialsLoop)) %>% 
    select(
      staircaseName, 
      thresholdParameter,
      questMeanAtEndOfTrialsLoop)
  summaries <- merge(info, summaries, by = ("staircaseName"))
  assign(paste0("summary", j), summaries)
  data_list[[j]] <- t
  j = j + 1
  max_trails = max(max_trails, nrow(t))
}
all_summary <- summary1
for (i in 2:n) {
  all_summary <- rbind(all_summary, get(paste0("summary", i)))
}

error <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
  data_list[[i]] %>% select(ProlificParticipantID, participant, deviceType, hardwareConcurrency,                             browser, deviceSystemFamily,  deviceSystem, deviceLanguage,
                            block, block_condition,conditionName, targetTask, targetKind, 
                            resolution,error)
}

error <- error %>% 
  filter(error != "") %>% 
  arrange(block_condition) %>% 
  mutate(ok = emoji("x")) %>% 
  dplyr::rename(cores = hardwareConcurrency)
  
noerror_fails = tibble()
for (i in 1 : length(data_list)) {
  if (nrow(data_list[[i]]) < max_trails) {
    if (!data_list[[i]]$participant[1] %in% error$participant) {
      t <- data_list[[i]] %>% 
        distinct(ProlificParticipantID,participant, deviceType, 
                 hardwareConcurrency, deviceSystem, browser, resolution) %>% 
        mutate(error = "invalid file") %>% 
        select(error,ProlificParticipantID,participant, deviceType, 
               hardwareConcurrency, deviceSystem, browser, resolution) %>% 
        dplyr::rename("cores" = "hardwareConcurrency")
      if (!('block' %in% colnames(t))) {
        t$block <- ''
      }
      if (!('block_condition' %in% colnames(t))) {
        t$block_condition <- ''
      }
      if (!('conditionName' %in% colnames(t))) {
        t$conditionName <- ''
      }
      if (!('targetTask' %in% colnames(t))) {
        t$targetTask <- ''
      }
      if (!('targetKind' %in% colnames(t))) {
        t$targetKind <- ''
      }
      t$ok <- emoji("x")
      noerror_fails <- rbind(noerror_fails,t)
    }
  }
}
completes = tibble()
for (i in 1 : length(data_list)) {
  if (!data_list[[i]]$participant[1] %in% error$participant 
      &!data_list[[i]]$participant[1] %in% noerror_fails$participant) {
    t <- data_list[[i]] %>% 
      distinct(ProlificParticipantID,participant, deviceType, 
               hardwareConcurrency, deviceSystem, browser, resolution)
    t$block <- ''
    t$block_condition <- ''
    t$conditionName <- ''
    t$targetTask <- ''
    t$targetKind <- ''
    t$error <- ''
    t$ok <- emoji("white_check_mark")
    completes <- rbind(completes,t)
  }
}
completes <- completes %>% dplyr::rename("cores" = "hardwareConcurrency")

new <- rbind(noerror_fails,
             completes,
             error %>% 
               select(error, ProlificParticipantID,participant, deviceType, 
                      cores, deviceSystem, browser, resolution,
                      block, block_condition, conditionName, targetTask, targetKind, ok)) %>%
  arrange(desc(block_condition)) %>% 
  dplyr::rename("Prolific ID" = "ProlificParticipantID", "Pavlovia ID" = "participant") %>% 
  mutate(ok = factor(ok)) %>% 
  select(ok, error, `Prolific ID`, `Pavlovia ID`, deviceType, 
         cores, deviceSystem, browser, resolution,
         block_condition, conditionName, targetTask, targetKind)

crowding <- all_summary %>% 
  filter(thresholdParameter != "size", targetKind == "letter", conditionName != "practice") %>% 
  select(participant, conditionName, questMeanAtEndOfTrialsLoop, font) %>%
  dplyr::rename(log_crowding_distance_deg = questMeanAtEndOfTrialsLoop)


########################### RSVP READING ############################


# the rsvp_speed is the dataframe storing the rsvpReading summary from all trials of all participants
rsvp_speed <- all_summary %>% 
  filter(targetKind == "rsvpReading") %>% 
  select(block, participant, conditionName, questMeanAtEndOfTrialsLoop, font, targetKind) %>%
  dplyr::rename(log_duration_s_RSVP = questMeanAtEndOfTrialsLoop) %>% 
  mutate(block_avg_log_WPM = log10(60) - log_duration_s_RSVP) 
crowding_vs_rsvp <- merge(crowding,rsvp_speed, by = c("participant", "font"))


################################ READING #######################################


reading <- data1 %>% 
  select(block_condition, participant, conditionName, font, wordPerMin, targetKind) %>% 
  mutate(log_WPM = log10(wordPerMin)) %>% 
  filter(targetKind == "reading" & font !="")
for (i in 2:n) {
  reading_speed <- get(paste0("data", i)) %>% 
    filter(targetKind == "reading") %>% 
    select(block_condition, participant, conditionName, font, wordPerMin, targetKind) %>% 
    mutate(log_WPM = log10(wordPerMin))
  reading <- rbind(reading, reading_speed) %>% filter(font !="")
}

reading_each <- reading %>% 
  group_by(font, participant, block_condition) %>%
  dplyr::summarize(avg_wordPerMin = 10^(mean(log10(wordPerMin), na.rm = T)), .groups = "keep") %>% 
  ungroup()

reading_exceed_1500 <- reading_each %>% filter(avg_wordPerMin > 1500)
for (i in 1 : nrow(reading_exceed_1500)) {
  warning(paste("Participant:",
                reading_exceed_1500$participant[i],
                "reading speeds removed due to excessive max speed",
                round(reading_exceed_1500$avg_wordPerMin[i],2),
                "> 1500 word/min",
                sep = " "))
}

reading_valid <- reading_each %>% 
  filter(!participant %in% reading_exceed_1500$participant) %>% 
  mutate(targetKind = "reading")

threshold_all <- all_summary %>%
  filter(conditionName != "practice") %>% 
  group_by(conditionName) %>%
  dplyr::summarize(
    m = mean(questMeanAtEndOfTrialsLoop),
    se = sd(questMeanAtEndOfTrialsLoop)/sqrt(n()), 
    sd = sd (questMeanAtEndOfTrialsLoop),
    N = n(),
    unit = "threshold")

practice_all <- all_summary %>%
  filter(conditionName == "practice") %>% 
  group_by(conditionName, participant) %>%
  dplyr::summarize(pm = mean(questMeanAtEndOfTrialsLoop)) %>% 
  ungroup() %>% 
  group_by(conditionName) %>% 
  dplyr::summarize(
    m = mean(pm),
    se = sd(pm)/sqrt(n()), 
    sd = sd(pm),
    N = n(),
    unit = "threshold")

wpm_all <- reading %>% 
  filter(!participant %in% reading_exceed_1500$participant) %>%
  filter(conditionName != "") %>% 
  group_by(conditionName, participant) %>%
  dplyr::summarize(
    pm = mean(wordPerMin, na.rm =T), .group = "keep") %>% 
  filter(!is.na(pm)) %>% 
  ungroup() %>% 
  group_by(conditionName) %>% 
  dplyr::summarize(
    m = mean(pm),
    se = sd(pm)/sqrt(n()), 
    sd = sd(pm),
    N = n(),
    unit = "word per minute")
summary_table <- rbind(threshold_all, practice_all, wpm_all) %>% mutate(m = round(m,3),
                                                                        sd = round(sd,3),
                                                                        se = round(se,3))
reading_byfont <- reading_valid %>% 
  group_by(font) %>% 
  dplyr::summarise(avg_log_SpeedWPM = mean(log10(avg_wordPerMin), na.rm = T), 
            median_log_SpeedWPM = median(log10(avg_wordPerMin), na.rm = T),
            se = sd(log10(avg_wordPerMin), na.rm = T)/sqrt(n()))
reading_byfont
# average log crowding distance degree by font
crowding_byfont <- crowding%>% 
  group_by(font) %>% 
  dplyr::summarize(avg_log_crowding = mean(log_crowding_distance_deg, na.rm = T),
            median_log_crowding = median(log_crowding_distance_deg, na.rm = T),
            se_crowding = sd(log_crowding_distance_deg, na.rm = T)/sqrt(n()))
crowding_byfont
# average log rsvp reading wpm by font
rsvp_byfont <- rsvp_speed%>% 
  group_by(font) %>% 
  dplyr::summarize(avg_log_SpeedWPM = mean(block_avg_log_WPM),
            median_log_SpeedWPM = median(block_avg_log_WPM),
            se = sd(block_avg_log_WPM)/sqrt(n()))
rsvp_byfont

# inner join reading_byfont and crowding_byfont using font as index
reading_vs_crowding <- merge(reading_byfont,crowding_byfont, by = "font") %>% 
  mutate(targetKind = "reading") 
reading_vs_crowding <- merge(reading_byfont,crowding_byfont, by = "font") %>% 
  mutate(targetKind = "reading") 
RSVP_vs_crowding <- merge(rsvp_byfont,crowding_byfont, by = "font") %>% mutate(targetKind = "rsvpReading")
rsvp_vs_ordinary_vs_crowding <- rbind(reading_vs_crowding,RSVP_vs_crowding)

## scatter plot means connected
# calculate n
N_reading <- length(unique(reading_valid$participant))
N_rsvp <- length(unique(crowding_vs_rsvp$participant))
if (N_reading == N_rsvp) {
  N_text = paste0(" = ", N_reading)
} else {
  N_text = paste0(" = ", min(N_reading, N_rsvp), " to ", max(N_reading, N_rsvp))
}
# plot
rsvp_vs_ordinary_vs_crowding <- rsvp_vs_ordinary_vs_crowding %>% 
  mutate(language = ifelse(font == "MaestriRegular.woff2" | font == "TimesNewRoman", "English", "Arabic" ),
         languageAndKind = paste0(language, " ", targetKind))
p_mean <- ggplot(data = rsvp_vs_ordinary_vs_crowding, 
       aes(x = 10^(avg_log_crowding), 
           y = 10^(avg_log_SpeedWPM), 
           color = languageAndKind
       )) +
  geom_point(aes(shape = font), size = 3) + 
  geom_line() +
  scale_y_log10(limit = c(50,1000)) +
  scale_x_log10(limit = c(0.5,3)) + 
  coord_fixed(ratio = 1) +
  geom_errorbar(aes(ymin=10^(avg_log_SpeedWPM-se), ymax=10^(avg_log_SpeedWPM+se)), width=0) +
  geom_errorbar(aes(xmin=10^(avg_log_crowding-se_crowding), xmax=10^(avg_log_crowding+se_crowding)), width=0) +
  theme_bw() + 
  theme(legend.position = "right", 
        legend.box = "vertical", 
        legend.justification = c(1,1),
        legend.margin = margin(-0.4),
        legend.key.size = unit(4.5, "mm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.line = element_line(colour = "black")) + 
  labs(x = "Crowding distance (deg)", y = "Reading speed (word/min)") +
  annotation_logticks(short = unit(0.1, "cm"),                                                                        mid = unit(0.1, "cm"),
                      long = unit(0.3, "cm")) + 
  annotate("text", x=0.6, y=50, label= "n", fontface = "italic") +
  annotate("text", x=0.8, y=50, label= N_text) +
  ggtitle("MEANs") + 
  scale_shape_manual(values = c(15, 16, 17, 18))

p_median <- ggplot(data = rsvp_vs_ordinary_vs_crowding, 
       aes(x = 10^(median_log_crowding), 
           y = 10^(median_log_SpeedWPM), 
           color = languageAndKind)) +
  geom_point(aes(shape = font), size = 3) + 
  geom_line() +
  scale_y_log10() +
  scale_x_log10() + 
  coord_fixed(ratio = 1) +
  theme_bw() + 
  theme(legend.position = "right", 
        legend.box = "vertical", 
        legend.justification = c(1,1),
        legend.margin = margin(-0.4),
        legend.key.size = unit(4.5, "mm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.line = element_line(colour = "black")) + 
  labs(x = "Crowding distance (deg)", y = "Reading speed (word/min)") +
  annotation_logticks(short = unit(0.1, "cm"),                                                
                      mid = unit(0.1, "cm"),
                      long = unit(0.3, "cm")) +
  annotate("text", x=0.32, y=50, label= "n", fontface = "italic") +
  annotate("text", x=.5, y=50, label= N_text) +
  ggtitle("Median") +
  scale_shape_manual(values = c(15, 16, 17, 18))