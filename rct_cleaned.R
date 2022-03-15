#' ---------------------------
#'
#' Script name: 
#'
#' Purpose of script:
#'
#' Author: Helena Manguerra
#'
## ---------------------------

rm(list=ls())

library(data.table)
library(magrittr)
library(ggplot2)
library(lme4)

# ------- Data cleaning

# deleted for github
domo_id = "XXX"
secret_id = 'XXX'

domo <- rdomo::Domo(client_id=domo_id,
                    secret=secret_id)

write.csv(data, "G:/.shortcut-targets-by-id/0BwSiMwXYbhP-LUE0eW54TXl4RlE/ICM Research/1. Ongoing Projects/2021 GIF Attendance RCT B4/Attendance Analysis/rct_analysis_data.csv", row.names = F)

data <- domo$ds_get("48a6bb9f-0518-4690-a25e-32c8536745e8")
data <- data.table(data)

# No bacolod? 
data <- data[is.na(`Community Canceled`)]

# Mark participants who attended at least 1 week 
data[week1 == 1, first_session := TRUE]

#data <- data[part_type=="original"]

part_ids <- 
  c("treatment", "sys_participant_id",      "participant_id",          "original_participant_id", "sys_partattend_id",       "sys_community_id",       
    "community_id",            "sg_id",                   "batch_name",              "application_id",          "program_id",             
    "program_name",            "parent_program_name",     "base_name",               "branch_name",             "pastor_id",              
    "pastor_status",           "part_gender",             "participant_age_est",     "part_type",               "part_status",            
    "part_isvisitor",          "part_income",             "part_povscore",           "part_selectionscore",     "part_sg_role", 
    "first_session") 

data[, part_type := factor(part_type, levels = c("original_replacement", "original"), ordered = T)]
data[, part_isvisitor := factor(part_isvisitor, levels = c(TRUE, FALSE), ordered = T)]

# melt into long by participant-week 
data_long <- melt(data, id.vars = part_ids, measure.vars = paste0("week", 1:15))
data_long[, attended := ifelse(value == 1, "yes", "no")]
data_long[, attended_0_1 := value]

data_long[, week := as.integer(gsub(variable, pattern = "week", replacement = ""))]

# Create dummy variables for treatments 
data_long[, Treatment1 := ifelse(treatment %like% "V1", TRUE, FALSE)]
data_long[, Treatment2 := ifelse(treatment %like% "V2", TRUE, FALSE)]
data_long[, Control := ifelse(!treatment %like% "V", TRUE, FALSE)]

data_long[, sys_community_id_factor := as.character(sys_community_id)]

# Identify communities where program ended early (290 communities; 96 in Control, 98 in Treatment 1, 96 in Treatment 2)
data_long <- merge(data_long, data_long[, .(attendees = sum(value), no_program = TRUE), .(sys_community_id, week)][attendees == 0], all.x=T)

comm_ended_early <- unique(data_long[!is.na(no_program), sys_community_id])

data_long <- data_long[is.na(no_program)]
data_long[sys_community_id %in% comm_ended_early, comm_ended_early := TRUE]
data_long[is.na(comm_ended_early), comm_ended_early := FALSE]

data_long[, comm_ended_early := factor(comm_ended_early, levels = c(TRUE, FALSE), ordered = T)]

# Identify earliest week of dropout per participant 
data_long <- data_long[order(treatment, sys_community_id, sys_participant_id, week)]
data_long[, consec := sequence(rle(as.character(value))$lengths), by = sys_participant_id]

data_long[value == 1, consec_attended := consec]
data_long[value == 0, consec_absent := consec]

dropout_week <- data_long[consec_absent == 3, .(dropout_week = min(week)), by = sys_participant_id]
dropout_week[, dropout_week := dropout_week - 3]

data_long <- merge(data_long, dropout_week, all.x=T)

# Identify if participant has dropped out by this week 
data_long[week > dropout_week, dropped_out_starting_week := TRUE]
data_long[is.na(dropout_week) | week <= dropout_week, dropped_out_starting_week := FALSE]

# Identify dropouts
data_long[!is.na(dropout_week), is_dropout := TRUE]
data_long[is.na(is_dropout), is_dropout := FALSE]

# Identify non-starters
non_starter_participants <- unique(data_long[dropout_week == 0, sys_participant_id])

data_long[sys_participant_id %in% non_starter_participants, non_starter := TRUE]
data_long[!sys_participant_id %in% non_starter_participants, non_starter := FALSE]

model_data <- data_long[comm_ended_early == FALSE  & part_type == "original" & part_isvisitor == FALSE]

# ------- QUESTION 0: Do we have the correct population? 

# Subset out participants in comm_ended_early

ggplot(data_long[week == 1]) + 
  geom_bar(aes(x = "", fill = base_name), position = position_stack(), color = "black") + 
  facet_wrap(~treatment, nrow = 1)

ggplot(data_long) + 
  geom_bar(aes(x = week, fill = base_name), position = position_stack(), color = "black") + 
  facet_wrap(~treatment, nrow = 1)

ggplot(data_long) + 
  geom_bar(aes(x = week, fill = comm_ended_early), position = position_stack()) + 
  facet_wrap(~treatment, nrow = 1) +
  scale_fill_manual(values = c("darkred", "darkgreen")) + 
  ggtitle("Subset out participants in communities ending early")


# Subset out participants in original vs replacement

ggplot(data_long[comm_ended_early == FALSE & week == 1]) + 
  geom_bar(aes(x = "", fill = base_name), position = position_stack(), color = "black") + 
  facet_wrap(~treatment, nrow = 1) 
  
ggplot(data_long[comm_ended_early == FALSE & week == 1]) + 
  geom_bar(aes(x = "", fill = part_type), position = position_stack()) + 
  facet_wrap(~treatment, nrow = 1) + 
  scale_fill_manual(values = c("darkred", "darkgreen")) + 
  ggtitle("Subset out participants who are original replacements")


# Subset out participants in visitors

ggplot(data_long[comm_ended_early == FALSE & part_type == "original" & week == 1]) + 
  geom_bar(aes(x = "", fill = part_isvisitor), position = position_stack()) + 
  facet_wrap(~treatment, nrow = 1) + 
  scale_fill_manual(values = c("darkred", "darkgreen")) + 
  ggtitle("Subset out participants who are visitor")


# Final dataset


ggplot(model_data[week == 1]) + 
  geom_bar(aes(x = "", fill = base_name), position = position_stack(), color = "black") + 
  facet_wrap(~treatment, nrow = 1) + 
  theme_bw()

ggplot(model_data) + 
  geom_bar(aes(x = week, fill = attended), position = position_stack()) + 
  facet_wrap(~treatment, nrow = 1) + 
  theme_bw() + 
  ggtitle("Attendance by week")


ggplot(model_data) + 
  geom_bar(aes(x = week, fill = attended), position = position_stack()) + 
  facet_wrap(~treatment, scales = "free", nrow = 1) + 
  theme_bw() + 
  scale_y_continuous(breaks = c(500, 1000, 1500, 2000, 2500, 3000)) + 
  ggtitle("Attendance by week")


# ------- 


mod1a <- glm(data = model_data, attended_0_1 ~ Treatment1 + Treatment2, family = "binomial") 
summary(mod1a)

mod1b <- glmer(data = model_data, attended_0_1 ~ Treatment1 + Treatment2 + (1|base_name/branch_name), family = "binomial") 
summary(mod1b)

mod1c <- glmer(data = model_data, attended_0_1 ~ Treatment1 + Treatment2 + as.factor(week) + (1|base_name/branch_name)  , family = "binomial") 
summary(mod1c)

# boundary (singular) fit: see help('isSingular')

# ------- First session attendees only 

mod1d <- glm(data = model_data[first_session == TRUE], attended_0_1 ~ Treatment1 + Treatment2, family = "binomial") 
summary(mod1d)

mod1e <- glmer(data = model_data[first_session == TRUE], attended_0_1 ~ Treatment1 + Treatment2 + (1|base_name/branch_name), family = "binomial") 
summary(mod1e)

mod1f <- glmer(data = model_data[first_session == TRUE], attended_0_1 ~ Treatment1 + Treatment2 + as.factor(week) + (1|base_name/branch_name), family = "binomial") 
# boundary (singular) fit: see help('isSingular')
summary(mod1f)



# ------- Mirror IPA analysis

ipa_orig <- model_data[, .(attended = sum(attended_0_1), total = .N), by = .(base_name, branch_name, treatment, Treatment1, Treatment2, Control, week, sys_community_id)]
ipa_orig[, prop := attended / total]

ggplot(ipa_orig) + 
  geom_boxplot(aes(x = treatment, y = prop, color = treatment),  lwd=1, outlier.alpha = 0) + 
  geom_jitter(aes(x = treatment, y = prop)) + 
  facet_wrap(~ week, nrow = 2) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(ipa_orig) + 
  geom_boxplot(aes(x = treatment, y = prop, color = treatment), lwd=2, outlier.alpha = 0) + 
  geom_jitter(aes(x = treatment, y = prop), alpha = 0.5) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



mod2a <- lm(data = ipa_orig, prop ~ Treatment1 + Treatment2) 
summary(mod2a)


ipa_first_session <- model_data[first_session == TRUE, .(attended = sum(attended_0_1), total = .N), by = .(base_name, branch_name, treatment, Treatment1, Treatment2, Control, week, sys_community_id)]
ipa_first_session[, prop := attended / total]

ggplot(ipa_first_session) + 
  geom_boxplot(aes(x = treatment, y = prop, color = treatment),  lwd=1, outlier.alpha = 0) + 
  geom_jitter(aes(x = treatment, y = prop)) + 
  facet_wrap(~ week, nrow = 2) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(ipa_first_session) + 
  geom_boxplot(aes(x = treatment, y = prop, color = treatment), lwd=2, outlier.alpha = 0) + 
  geom_jitter(aes(x = treatment, y = prop), alpha = 0.5) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

mod2b <- lm(data = ipa_first_session, prop ~ Treatment1 + Treatment2) 
summary(mod2b)


# ------- Retention 

ggplot(model_data[week == 1]) + 
  geom_bar(aes(x = "", fill = is_dropout), color = "black", position = position_stack()) +
  facet_wrap(~treatment) + 
  theme_bw()

ggplot(model_data[week == 1]) + 
  geom_bar(aes(x = "", fill = is_dropout), color = "black", position = position_stack()) +
  facet_wrap(~treatment, scales = "free") + 
  theme_bw()


mod3a <- glm(data = model_data[week == 1], is_dropout ~ Treatment1 + Treatment2, family = "binomial") 
summary(mod3a)

mod3b <- glmer(data = model_data[week == 1], is_dropout ~ Treatment1 + Treatment2 + (1|base_name/branch_name), family = "binomial") 
summary(mod3b)

# mod3c <- glmer(data = model_data[], attended_0_1 ~ Treatment1 + Treatment2 + as.factor(week) + (1|base_name/branch_name), family = "binomial") 
# # boundary (singular) fit: see help('isSingular')
# summary(mod3c)


model_data[, dropped_out_by_this_week := dropped_out_starting_week]


ggplot(model_data) + 
  geom_bar(aes(x = as.factor(week), fill = dropped_out_by_this_week), color = "black", position = position_stack()) +
  facet_wrap(~treatment) + 
  theme_bw()

ggplot(model_data) + 
  geom_bar(aes(x = as.factor(week), fill = dropped_out_by_this_week), color = "black", position = position_stack()) +
  facet_wrap(~treatment, scales = "free") + 
  theme_bw()

mod5a <- glm(data = model_data[], dropped_out_by_this_week ~ week + Treatment1 + Treatment2, family = "binomial") 
summary(mod5a)

mod5a <- glm(data = model_data, dropped_out_by_this_week ~ week + treatment, family = "binomial") 
summary(mod5a)

# ------- Orig 1st session

ggplot(model_data[week == 1 & first_session  == T]) + 
  geom_bar(aes(x = "", fill = is_dropout), color = "black", position = position_stack()) +
  facet_wrap(~treatment) + 
  theme_bw()

ggplot(model_data[week == 1 & first_session == T]) + 
  geom_bar(aes(x = "", fill = is_dropout), color = "black", position = position_stack()) +
  facet_wrap(~treatment, scales = "free") + 
  theme_bw()

mod4a <- glm(data = model_data[week == 1 & first_session  == T], is_dropout ~ Treatment1 + Treatment2, family = "binomial") 
summary(mod4a)

mod4b <- glmer(data = model_data[week == 1 & first_session  == T], is_dropout ~ Treatment1 + Treatment2 + (1|base_name/branch_name), family = "binomial") 
summary(mod4b)

model_data[, dropped_out_by_this_week := dropped_out_starting_week]


ggplot(model_data[first_session == T & week > 1]) + 
  geom_bar(aes(x = as.factor(week), fill = dropped_out_by_this_week), color = "black", position = position_stack()) +
  facet_wrap(~treatment) + 
  theme_bw()



ggplot(model_data[first_session == T & week %in% 2:8]) + 
  geom_bar(aes(x = as.factor(week), fill = dropped_out_by_this_week), color = "black", position = position_stack()) +
  facet_wrap(~treatment, scales = "free") + 
  theme_bw()

