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
mentee = read.csv("mentee_24feb.csv", stringsAsFactors = F)
mentor_schedule = read.csv("mentor_availability.csv", stringsAsFactors = F)

## Enforce only the first three selections
mentee[,4:ncol(mentee)][mentee[,4:ncol(mentee)] > 3] <- NA

# Set aside unneeded columns
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
  
# Map Module name to SK number
skills_map = data.frame(SK = c("SK1", "SK2", "SK3", "SK4", "SK5", "SK6", "SK7", "SK8", "SK9"),
                        character_name = c("Entrepreneurship","Career Progression","Interpersonal Relationships",
                                           "Leadership: Personal", "Leadership: Team", "Leadership: Strategic", 
                                           "Transition", "Work-Life Considerations", "Personal Growth"))


    ## Reshape datafiles
# mentor datafile
mentor_long = reshape2::melt(mentor, id.vars = 'Mentor.Name')
colnames(mentor_long) = c("Mentor.Name","Skill.SubSkill","Ranking")
mentor_long$Skill.SubSkill  = as.character(mentor_long$Skill.SubSkill)
mentor_long$Skill = substr(mentor_long$Skill.SubSkill, 1, 3)
mentor_long$Ranking = as.numeric(ff(mentor_long$Ranking, seq(1,5, by = 1), seq(1,5, by = 1))) # Recode Ranking

# mentoe datafile
mentee$topics1 = ff(mentee$MOD1_TOPIC, skills_map$character_name, as.character(skills_map$SK), ignore.case = T)
mentee$topics2 = ff(mentee$MOD2_TOPIC, skills_map$character_name, as.character(skills_map$SK), ignore.case = T)
mentee = mentee[,c(1,58:59,4:57)]

mentee_long = reshape2::melt(mentee, id.vars = c('Mentee','topics1','topics2'))
colnames(mentee_long) = c("Mentee.Name","Skill.Mod1","Skill.Mod2","Skill.SubSkill","Ranking")
mentee_long[,1:ncol(mentee_long)] = sapply(mentee_long[,1:ncol(mentee_long)], function(x) FUN = as.character(x))
mentee_long$Skill = substr(mentee_long$Skill.SubSkill, 1, 3)

#mentee_long$Skill = substr(mentee_long$Skill.SubSkill, 1, 3)
# Check that ranking Rankings are unique within mentees' main skill categories


# Use the topics identified in columns 2 and 3 of the mentee responses to identify module topics
module_topics = find_ideal_module(mentee$topics1,mentee$topics2, 8) # This will take some time to run as it searches for the best module combinations.

module_breakout = define_modules_v2(module_topics, mentee$topics1,mentee$topics2)

# Determine which mentors to select based on module topics
  # and number of breakout rooms needed per topic.

mentor_preferences = mentor_long %>%
  group_by(Mentor.Name, Skill) %>%
  mutate(sum_ranking = sum(as.numeric(Ranking), na.rm = T)) %>%
  slice(n=1) %>%
  arrange(Skill, desc(sum_ranking)) %>%
  ungroup()


# Assign mentors to modules + breakout rooms by interest + experience
mentor_module_df = select_mentors_v2(mentor_preferences, module_breakout, min_mentors) # This will take some time to run as it searches for the best mentor / module combinations.

  # We need to add a check here that says, 'if unique(mentor_module_df$Skill) < n_topics
  # then re-run the code' because we will not optimize the matching.

mentee_preferences_mod1 = mentee_long %>%
  filter(Ranking!="NA" & Skill == Skill.Mod1) %>%
  group_by(Skill, Skill.SubSkill, Ranking) %>%
  mutate(n_ranking = n()) %>%
  slice(n=1) %>%
  group_by(Skill.SubSkill) %>%
  slice(n=1) %>%
  select(-c(Mentee.Name, Skill.Mod2, Ranking)) %>%
  ungroup()
  
mentee_preferences_mod2 = mentee_long %>%
  filter(Ranking!="NA" & Skill == Skill.Mod2) %>%
  group_by(Skill, Skill.SubSkill, Ranking) %>%
  mutate(n_ranking = n()) %>%
  slice(n=1) %>%
  group_by(Skill.SubSkill) %>%
  slice(n=1) %>%
  select(-c(Mentee.Name, Skill.Mod2, Ranking)) %>%
  ungroup()


## Above code identifies favorite subskill topics for both modules.
# In order to get unique subskill topics, you will need to subset to 
# unique subskills within 'Skill.Mod1' or Skill.Mod2' and then call slice(n=1)
  
#  Use mentee preferences to choose subskills.
mentor_module_df_ss = assign_subskills(mentor_module_df, mentee_preferences_mod1, mentee_preferences_mod2) 


######## Code complete until here #######


# Last step is to assign mentees to mentors and modules, 
# based on the identified subtopics.
# This function will sometimes get stuck - I am still looking into why - 
# But it is consistently yielding the correct result when it does run, which is most of the time.
# If assign_mentees_v2() takes more than a few seconds to run; force stop and try again.
mod1_assignments = assign_mentees_v2(mentor_module_df_ss, mentee_long,"Module1")

## Sanity check - unique names for each breakout room
#check = melt(mod1_assignments, id.vars = c("mentor","mod","topic","breakout_room","Skill.SubSkill"))
#length(unique(check$value)) # Should equal to nrow(mentee)

mod2_assignments = assign_mentees_v2(mentor_module_df_ss, mentee_long,"Module2")

## Sanity check - unique names for each breakout room
#check = melt(mod2_assignments, id.vars = c("mentor","mod","topic","breakout_room","Skill.SubSkill"))
#length(unique(check$value)) # Should equal to nrow(mentee)

### Finally, combine the assignments ###
assignments = rbind(mod1_assignments, mod2_assignments)
colnames(assignments)[1:5] = c("Mentor.Name","Module","Skill","Breakout_Room","SubSkill")

# Map the Skill tags back to English #
assignments$Skill = ff(assignments$Skill, as.character(skills_map$SK),  as.character(skills_map$character_name), ignore.case = T)

write.csv(assignments, file = "final_example_output.csv", row.names = F, quote = F)
