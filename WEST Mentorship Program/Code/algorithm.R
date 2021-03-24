setwd("~/Desktop/mentor_optimization/opt.v4")
.libPaths( c("~/rlibraries", .libPaths()))

###--------------------------------------------------------------### 
###  WEST Mentorship Program - Mentor Mentee Matching Algorithm  ###
###--------------------------------------------------------------### 

## SETUP ----
## Read in needed libraries
pacman::p_load(reshape2, dplyr, survey, purrr, tidyr, lubridate, readxl,here, gtools)

## Source program functions
source("helper_functions.R")

# Read in the variable inputs from Shiny
#session_date = input$week               # Session Date for the optimization
#optimize_mentors = input$opt_mentors    # Optimize the number of mentors, or use all available mentors?

max_mentees = input$max_mentees
min_mentees = input$min_mentees

 #session_date = "2021-03-23" # Session Date for the optimization
 #optimize_mentors = FALSE    # Optimize the number of mentors, or use all available mentors?
 #max_mentees = 3
 #min_mentees = 1


## DATA PREP ----

## Read in data
mentee = read.csv(input$menteeSurvey$datapath, stringsAsFactors = F)
mentor = read.csv(input$mentorSurvey$datapath, stringsAsFactors = F, skip = 1, header = T)
mentor_schedule = read.csv(input$mentorAvailability$datapath, stringsAsFactors = F)
# subskill_map = read.csv(file = "Data/subskill_map.csv", stringsAsFactors = F)

# mentee = read.csv("Data/latest_mentee.csv", stringsAsFactors = F)
# mentor = read.csv("Data/mentor_realistic.csv", stringsAsFactors = F, skip = 1, header = T)
# mentor_schedule = read.csv("Data/mentor_availability.csv", stringsAsFactors = F)
# subskill_map = read.csv(file = "Data/subskill_map.csv", stringsAsFactors = F)


# Map Module name to SK number
skills_map = data.frame(SK = c("SK1", "SK2", "SK3", "SK4", "SK5", "SK6", "SK7", "SK8", "SK9"),
                        character_name = c("Entrepreneurship","Career.Progress","Interpersonal.Relationships",
                                           "Leadership..Personal", "Leadership..Team", "Leadership..Strategic", 
                                           "Transition", "Work.Life.Considerations", "Personal.Growth"))
subskill_map = subskill_map[subskill_map$Character_Response!="",] 

## Enforce only the first three selections
mentee = mentee[,c(1:111)] # Drop the feedback column

# There was one mentee who, for some reason, marked 4 - 6 instead of 1-3. Check for this and correct.
mins = as.numeric(apply(mentee, 1, FUN=min, na.rm = T))
to_replace = as.numeric(match(4, mins))
mentee[to_replace,4:ncol(mentee)]  = sapply(mentee[to_replace,4:ncol(mentee)], function(x) FUN = as.numeric(ff(x, seq(4,6, by = 1), seq(1,3, by = 1))))

mentee[,4:ncol(mentee)][mentee[,4:ncol(mentee)] > 3] <- NA # Only allow the first three subskill choices
colnames(mentee)[1:3] = c("Mentee.Name","MOD1_TOPIC","MOD2_TOPIC") # Rename columns

## Wrangle the wide format to long format, so that subskill preferences are indicated 
## as the program expects to see them.
t1 = mentee[,c(1:57)]
t2 = mentee[,c(1:3,58:ncol(mentee))]

colnames(t1)[4:ncol(t1)] = subskill_map$Short_Response
colnames(t2)[4:ncol(t2)] = subskill_map$Short_Response

mentee = rbind(t1, t2)

mentee = mentee %>%
  arrange(Mentee.Name) %>%
  group_by(Mentee.Name) %>%
  tidyr::fill(subskill_map$Short_Response, .direction = "downup") %>%
  slice(n=1) %>%
  ungroup()

# Name columns in mentor survey
cols_exp = paste(ff(colnames(mentor[,2:10]), skills_map$character_name, skills_map$SK, ignore.case = T),"_exp", sep = "")
cols_int = paste(ff(colnames(mentor[,11:19]), skills_map$character_name, skills_map$SK, ignore.case = T),"_int", sep = "")
colnames(mentor) = c("Mentor.Name",cols_exp, cols_int)

# Drop values for mentor data where experience <= 3 and interest==1
exp = grep("exp",colnames(mentor)) # find the columns for experience
int = grep("int",colnames(mentor)) # find the columns for interest

# Recode the rankings
mentor[,2:ncol(mentor)] = sapply(mentor[,2:ncol(mentor)], function(x) FUN = as.numeric(ff(x, seq(1,5, by = 1), seq(1,5, by = 1))))

# Drop undesired values
mentor[,first(exp):last(exp)][mentor[,first(exp):last(exp)] <= 3] <- NA
mentor[,first(int):last(int)][mentor[,first(int):last(int)] == 1] <- NA

# Format columns in availability dataset
mentor_schedule$SessionDate = mdy(mentor_schedule$SessionDate)

available_mentors = mentor_schedule %>%
  filter(SessionDate == session_date & Available == "1") %>%
  summarize(Name)

mentor = subset(mentor, Mentor.Name %in% available_mentors$Name)

## Generate needed variables
if(optimize_mentors) {
  n_mentors = round(nrow(mentee) / max_mentees) # How many mentors do we need, at minimum?
} else {
  n_mentors = as.numeric(nrow(available_mentors))
}


## Reshape datafiles
# mentor datafile
mentor_long = reshape2::melt(mentor, id.vars = 'Mentor.Name')
colnames(mentor_long) = c("Mentor.Name","Skill.SubSkill","Ranking")
mentor_long$Skill.SubSkill  = as.character(mentor_long$Skill.SubSkill)
mentor_long$Skill = substr(mentor_long$Skill.SubSkill, 1, 3)

# mentee datafile
mentee$topics1 = ff(mentee$MOD1_TOPIC, skills_map$character_name, as.character(skills_map$SK), ignore.case = T)
mentee$topics2 = ff(mentee$MOD2_TOPIC, skills_map$character_name, as.character(skills_map$SK), ignore.case = T)
mentee = mentee[,c(1,58:59,4:57)]

mentee_long = reshape2::melt(mentee, id.vars = c('Mentee.Name','topics1','topics2'))
colnames(mentee_long) = c("Mentee.Name","Skill.Mod1","Skill.Mod2","Skill.SubSkill","Ranking")
mentee_long[,1:ncol(mentee_long)] = sapply(mentee_long[,1:ncol(mentee_long)], function(x) FUN = as.character(x))
mentee_long$Skill = substr(mentee_long$Skill.SubSkill, 1, 3)

# Make sure that mentee subskill preferences are not included if not part of main skills
mentee_long$extra_ss = ifelse(mentee_long$Skill.Mod1!= mentee_long$Skill & !is.na(mentee_long$Ranking) & mentee_long$Skill.Mod2!=mentee_long$Skill, TRUE, FALSE)
# If more than one ranking position is selected (i.e. two 1st positions, 2nd position, etc.) within main skill, then randomly choose one
mentee_long = mentee_long %>%
  filter(extra_ss==FALSE & !is.na(Ranking)) %>%
  group_by(Mentee.Name, Skill, Ranking) %>%
  mutate(n_rank = n()) %>%
  ungroup()

mentee_long$dupe = duplicated(mentee_long$n_rank)
mentee_long = mentee_long %>%
  mutate(drop = ifelse(dupe==FALSE & n_rank>1, TRUE, FALSE)) %>%
  filter(drop==FALSE) %>%
  select(-c(extra_ss, n_rank, dupe, drop))

rank1_module_ss = define_module_rank1(mentee_long, n_mentors, tries = 20)
rank2_module_ss = define_module_rank2(rank1_module_ss, mentee_long, n_mentors, tries = 20)
rank3_module_ss = define_module_rank3(rank2_module_ss, mentee_long, n_mentors, tries = 20)

rank3_module_ss$Skill = substr(rank3_module_ss$Skill.SubSkill, 1, 3)

# Determine which mentors to select based on module topics
# and number of breakout rooms needed per topic.

mentor_preferences = mentor_long %>%
  group_by(Mentor.Name, Skill) %>%
  mutate(sum_ranking = sum(as.numeric(Ranking), na.rm = T)) %>%
  slice(n=1) %>%
  arrange(Skill, desc(sum_ranking)) %>%
  ungroup()


# Assign mentors to modules + breakout rooms by interest + experience
assignments = select_mentors_v3(mentor_preferences, rank3_module_ss) # This will take some time to run as it searches for the best mentor / module combinations.

# Arrange by module and breakout room
assignments = assignments %>% arrange(mod, breakout_room) %>% 
  select(Module = mod, Breakout_Room = breakout_room, Mentor.Name = mentor,
         Skill = Skill, SubSkill = Skill.SubSkill,
         Mentee1 = Mentee.1, Mentee2 = Mentee.2, Mentee3 = Mentee.3)

# Map the Skill tags back to character values#
assignments$Skill = ff(assignments$Skill, as.character(skills_map$SK),  as.character(skills_map$character_name), ignore.case = T)
assignments$SubSkill = ff(assignments$SubSkill, as.character(subskill_map$Short_Response),  as.character(subskill_map$Character_Response), ignore.case = T)


# Remove commas and other characters so that output format isn't interrupted in .csv file.
assignments$SubSkill = gsub(",", "", assignments$SubSkill, fixed = TRUE)
assignments$SubSkill = gsub("&", "", assignments$SubSkill, fixed = TRUE)
