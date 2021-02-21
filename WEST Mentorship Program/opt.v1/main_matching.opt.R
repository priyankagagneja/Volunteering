

## Read in needed libraries
pacman::p_load(reshape2, dplyr, survey, purrr, tidyr)

# Set Global Variables here
max_mentees = 3
min_mentees = 1


# Set skill for modules A, B, etc.
# FEEDBACK : For a given module, we want to work with all the skills possible (when the mentors are available and mentees are looking)
#            instead of taking this as input can we add in the group by's instrad ?
module_skill = "Sk1" # Read this in from Shiny app and output the final matching for Module A, B based on varying inputs.

#### DATA PREP PHASE ####

## Read in data
mentor = read.csv("./WEST Mentorship Program/opt.v1/mentor_skills.csv", stringsAsFactors = F)
mentee = read.csv("./WEST Mentorship Program/opt.v1/mentee_skills.csv", stringsAsFactors = F)

# Correct for an irregularities in the randomly generated data
mentee[7,9] = 2 
mentee[9,9] = 2 
mentee[9,6] = 4 

mentee = mentee[1:10,]
mentor = mentor[1:5,]

## Reshape datafiles
mentor_long = reshape2::melt(mentor, id.vars = 'Mentor.Name')
colnames(mentor_long) = c("Mentor.Name","Skill","Ranking")
mentor_long$Skill  = as.character(mentor_long$Skill)
mentor_long$Topic = substr(mentor_long$Skill, 1, 3)


mentee_long = reshape2::melt(mentee, id.vars = 'Mentee.Name')
colnames(mentee_long) = c("Mentee.Name","Skill","Ranking")
mentee_long$Skill  = as.character(mentee_long$Skill)
mentee_long$Topic = substr(mentee_long$Skill, 1, 3)
# UPDATE: Renamed all fields named 'Skill' to 'Topic' & 'Skill.SubSkill' to 'Skill'

# Check that ranking Rankings are unique within mentees' main skill categories


marginal_prob_function = function(df) {
  
  ## A simple function to assign weighting 
  ## to rankings in a reverse manner. 
  ## Accepts a dataframe with unique
  ## or non-unique column Rankings, and 
  ## returns a dataframe with
  ## the weights in reverse order.
  
  df$probs = rev((df$Ranking)/sum(df$Ranking))

  return(df)
}
# UPDATE: Added a line in the comments


## Create marginal probabilities per mentee
# This is a separate dataframe which will be merged in later.
marginal_probs = mentee_long %>%
  # FEEDBACK : both the filter conditions look the same. Also you might want to do if(!is.na(Ranking)) to filter for all ranked values.
  filter(Ranking!="NA" & Ranking!="NA") %>%
  group_by(Mentee.Name, Topic) %>%
  arrange(Ranking) %>%
  nest() %>%
  mutate(map(data, marginal_prob_function)) %>%
  unnest() %>%
  ungroup() %>%
  select(Mentee.Name, Topic, Skill, Ranking, probs)

table(marginal_probs$Ranking, marginal_probs$probs) ## Sanity check 
# In the above table, rank 4 should only have probs = 0.1 
# Rank 3 may take on probs = c(0.1666, 0.2)
# Rank 2 may take on probs = c(0.3, 0.33)
# Rank 2 may take on probs = c(0.4, 0.5)


# FEEDBACK : Based on the above feedback, the filter(Topic == module_skill) here will go away and a group_by might be needed instead.
mentor.df = mentor_long %>%
  filter(Ranking==1 & Topic == module_skill) %>%
  mutate(Mentor.Name = as.character(Mentor.Name)) %>%
  select(-Ranking)

colnames(mentor.df) = c("Mentor.Name","Mentor.Skills","Topic")

unique_mentees  = as.character(unique(mentee_long$Mentee.Name))
unique_mentors  = as.character(unique(mentor_long$Mentor.Name))


match = data.frame(expand.grid(unique_mentees, unique_mentors)) # Create all possible combinations of mentors & mentees
colnames(match) = c("Mentee.Name","Mentor.Name") # Name columns
match[c(1:2)] = sapply(match[c(1:2)], function(x) as.character(x)) # Make sure they didn't come through as type factor


new_long = merge(mentee_long, match, by = "Mentee.Name", all = T) # Expand out the mentee skill ranking set by all possible mentor matches.
new_long  = merge(new_long, mentor.df, by = c("Mentor.Name","Topic"), all = T) # Add in the mentor skills to the new expanded dataframe.


new_long_mod = new_long %>%
  # filter(Ranking!="NA") %>%
  filter(!is.na(Ranking)) %>%
  group_by(Mentee.Name, Topic) %>%
  mutate(n_skills_cat = length(unique(Ranking))) %>% # Calculate the number of requested skills by category
  group_by(Mentee.Name, Mentor.Name, Topic, Ranking) %>%
  mutate(n_matches = sum(Skill == Mentor.Skills)) %>% # Number of matching unique skills between mentee + mentor
  ungroup()


new_long_mod = merge(new_long_mod, marginal_probs, by = c("Mentee.Name","Topic","Ranking"), all = T)  # Merge weighting scheme

#Now weight the number of matches 

match.df = new_long_mod %>%
  mutate(weighted_match = probs * n_matches) %>%
  group_by(Mentee.Name, Mentor.Name, Topic) %>% 
  mutate(sum_probs_cat = sum(weighted_match)) %>% # Total weighted matches by category
  slice(n=1) %>%
  group_by(Mentee.Name, Mentor.Name) %>%
  mutate(total_match = sum(sum_probs_cat, na.rm = T)) %>% #Total weighted matches over all categories, by mentee and mentor.
  group_by(Mentee.Name, Mentor.Name, Topic) %>%
  slice(n=1) %>%
  ungroup() %>%
  group_by(Mentee.Name, Mentor.Name) %>%
  mutate(overall_n_skills = sum(n_skills_cat)) %>% # Sum the total skills requested, per mentee.
  slice(n=1) %>%
  ungroup() %>%
  mutate(adj_match = total_match / overall_n_skills) %>% # Adjust weighted matches for number of requested skills - penalizes more skills
  select(Mentee.Name, Mentor.Name, total_match, overall_n_skills, adj_match) %>%
  arrange(-adj_match) # Arrange by adjusted weighted matches
  

# Pre-allocate space in a final matching dataframe
final_matches = data.frame(matrix(ncol = length(unique_mentors), nrow = max_mentees))
colnames(final_matches) = c(unique_mentors)


# Assign mentees to mentors
  ## Initialization phase
  mentees_placed = 0 # No one placed yet
  data = match.df

  data = data[order(data$adj_match, decreasing = T),]
  
  mentor = first(data$Mentor.Name) # Return first mentor
  mentee = first(data$Mentee.Name) # Return first mentee
  
  col = as.numeric(which(names(final_matches)==mentor)) # Identify the column of the mentor we're matching  
  row = 1 # First row, as we are initializing.
  final_matches[row, col] = mentee # Assign mentee to the selected cell in the final dataframe
  
  mentees_placed  = mentees_placed + 1
  mentees_left    = unique_mentees[unique_mentees != mentee]
  available_mentors = unique_mentors
  
  while(mentees_placed < (length(unique_mentees)+1)) {
   
    ## Check how many mentees have been assigned to a particilar mentor
    mentee_counts = colSums(!is.na(final_matches))
   
    data = subset(data, Mentee.Name %in% mentees_left)
    data = data[order(data$adj_match, decreasing = T),]
    
    mentor = first(data$Mentor.Name)
    mentee = first(data$Mentee.Name)
    
    n_assigned = as.numeric(mentee_counts[names(mentee_counts)==mentor]) 
    
    if(n_assigned < max_mentees){
      
      col = as.numeric(which(names(final_matches)==mentor))
      row = n_assigned + 1
      final_matches[row, col] = mentee
      
      mentees_placed  = mentees_placed + 1
      mentees_left    = mentees_left[mentees_left != mentee]
      
    
    } else if(n_assigned >= max_mentees) {
      
      available_mentors = available_mentors[available_mentors != mentor]
      data = subset(data, Mentor.Name %in% available_mentors)
      
    }
    
  }
  

## Check to see if minimum mentees are assigned per mentor
mentee_counts = colSums(!is.na(final_matches))
zero_mentees  = names(mentee_counts[mentee_counts==0])  # Identify mentors with no mentees
candidates    = as.character(final_matches[max_mentees,]) # Identify the last assigned mentees for mentors at max capacity
candidates    = candidates[!is.na(candidates) & candidates!="NA"] 

for(i in 1:length(zero_mentees)){ # Set in a for loop in case there is more than one mentor below the minimum
  
  mentor = zero_mentees[i] 
  
  # Look up which candidate(s) was most suited to this mentor
  sub = subset(match.df, Mentee.Name %in% candidates & Mentor.Name %in% mentor)
  sub = sub[order(sub$adj_match, decreasing = T),]
  mentee = first(sub$Mentee.Name)
  
  final_matches[final_matches==mentee] = NA # Remove current assignment for mentee
  n_assigned = as.numeric(mentee_counts[names(mentee_counts)==mentor]) 
  
  col = as.numeric(which(names(final_matches)==mentor))
  row = n_assigned + 1
  
  final_matches[row, col] = mentee # Assign new mentor for this mentee.
  
}
  


