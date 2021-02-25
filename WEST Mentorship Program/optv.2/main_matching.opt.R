setwd("~/Desktop/mentor_optimization")
.libPaths( c("~/rlibraries", .libPaths()))

## Read in needed libraries
pacman::p_load(reshape2, dplyr, survey, purrr, tidyr, lubridate)

## Source program functions
source("functions.opt.R")

# Set Global Variables here
max_mentees = 3
min_mentees = 1

# Read in the Session Date from Shiny
session_date = "2021-03-02"

#### DATA PREP PHASE ####

## Read in data
mentor = read.csv("mentor_realistic.csv", stringsAsFactors = F)
mentee = read.csv("mentee_realistic.csv", stringsAsFactors = F)
mentor_schedule = read.csv("mentor_availability.csv", stringsAsFactors = F)

## Enforce only the first three selections
mentee[,4:ncol(mentee)][mentee[,4:ncol(mentee)] > 3] <- NA

# Set aside unneeded columns
#mentee = mentee[,c(1,4:ncol(mentee))]
mentor = mentor[,c(1, 5:ncol(mentor))]

# Format columns in availability dataset
mentor_schedule$SessionDate = mdy(mentor_schedule$SessionDate)

available_mentors = mentor_schedule %>%
  filter(SessionDate == session_date & Available == "1") %>%
  summarize(Name)

mentor = subset(mentor, Name %in% available_mentors$Name)
colnames(mentor)[1] = "Mentor.Name"

## Generate needed variables
min_mentors = round(nrow(mentee) / max_mentees) # How many mentors do we need, at minimum?
  
## Reshape datafiles
mentor_long = reshape2::melt(mentor, id.vars = 'Mentor.Name')
colnames(mentor_long) = c("Mentor.Name","Skill.SubSkill","Ranking")
mentor_long$Skill.SubSkill  = as.character(mentor_long$Skill.SubSkill)
mentor_long$Skill = substr(mentor_long$Skill.SubSkill, 1, 3)
mentor_long$Ranking = as.numeric(ff(mentor_long$Ranking, seq(1,5, by = 1), seq(1,5, by = 1))) # Recode Ranking

# Check that ranking Rankings are unique within mentees' main skill categories

# Map Module name to SK number
skills_map = data.frame(SK = c("SK1", "SK2", "SK3", "SK4", "SK5", "SK6", "SK7", "SK8", "SK9"),
                        character_name = c("Entrepreneurship","Career Progression","Interpersonal Relationships",
                                           "Leadership: Personal", "Leadership: Team", "Leadership: Strategic", 
                                           "Transition", "Work-Life Considerations", "Personal Growth"))


# Use the topics identified in columns 2 and 3 of the mentee responses to identify module topics
topics1 = ff(mentee$MOD1_TOPIC, skills_map$character_name, as.character(skills_map$SK), ignore.case = T)
topics2 = ff(mentee$MOD2_TOPIC, skills_map$character_name, as.character(skills_map$SK), ignore.case = T)

module_topics = define_modules(topics1, topics2, 6)

# Determine which mentors to select based on module topics

mentor_preferences = mentor_long %>%
  group_by(Mentor.Name, Skill) %>%
  mutate(sum_ranking = sum(as.numeric(Ranking), na.rm = T)) %>%
  slice(n=1) %>%
  arrange(Skill, desc(sum_ranking)) %>%
  ungroup()

# So we then need to see who is most comfortable with each topic, per module. 

mentor_module_df = select_mentors(mentor_preferences, module_topics, min_mentors)
  # We need to add a check here that says, 'if unique(mentor_module_df$Skill) < n_topics
  # then re-run the code' because we will not optimize the matching.

# Last step is to assign mentees to mentors and modules, based on topics. 
# Another piece of this is to identify which subtopics to cover.

assignments = assign_mentees(mentor_module_df, mentee)

assignments = assignments %>% arrange(Module) %>% 
  group_by(Module) %>%
  mutate(Breakout.Rooms = seq(1,n(), by = 1)) %>%
  ungroup()

