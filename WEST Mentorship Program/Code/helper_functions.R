## Functions for mentor optimization project ##

ff = function(x, patterns, replacements = patterns, fill = NA, ...) {
  stopifnot(length(patterns) == length(replacements))
  
  ans = rep_len(as.character(fill), length(x))    
  empty = seq_along(x)
  
  for(i in seq_along(patterns)) {
    greps = grepl(patterns[[i]], x[empty], ...)
    ans[empty[greps]] = replacements[[i]]  
    empty = empty[!greps]
  }
  
  return(ans)
}

select_topics = function(x, n_topics) {
  
  ## A UDF which will select a total of n topics
  # from a string of character-formatted topics. 
  # If the most popular topics exceeds the desired
  # number selected, a random sample is taken in
  # order to return the required n topics.
  
  df = as.data.frame(table(x))
  df$x = as.character(df$x)
  df = df %>% arrange(desc(Freq))
  freq = as.numeric(max(df$Freq))
  
  most_popular = df$x[df$Freq==freq]
  
  n = as.numeric(length(most_popular))
  
  while(n < n_topics) { # Do this until we have reached the desired number of topics.
    
    n_needed = n_topics - n
    df = df %>% filter(Freq != freq) %>%
      arrange(desc(Freq))
    
    freq = as.numeric(max(df$Freq))

    next_picks = df$x[df$Freq==freq] 
    sample_size = min(n_needed, as.numeric(length(next_picks))) # Cannot pick a sample larger than the population.
    
    most_popular = c(most_popular, sample(next_picks, sample_size, replace = F))
    
    n = length(most_popular)
  } 
  if(n > n_topics){
    
    most_popular = sample(most_popular, n_topics, replace = F)
  }
   
  return(most_popular)
}

define_modules = function(topics1, topics2, n_topics) {
  
  ## This function depends on the output of the previous function.
  # The functions do not need to be run independently.
  # The selected topics from the 'selct_topics' function are 
  # used to generate module topics.
  
  combined_topics = c(topics1, topics2)
  selected_topics = select_topics(combined_topics, n_topics)
  
  df1 = as.data.frame(table(topics1))
  df1$topics = as.character(df1$topics1)
  df1 = df1 %>% arrange(desc(Freq)) %>% filter(topics %in% selected_topics)  %>% select(topics)
  df1$mod = "Module1"

  df2 = as.data.frame(table(topics2))
  df2$topics = as.character(df2$topics2)
  df2 = df2 %>% arrange(desc(Freq)) %>% filter(topics %in% selected_topics)  %>% select(topics)
  df2$mod = "Module2"

  mod_df_picks = rbind(slice(df1, (1:(n_topics / 2))), slice(df2, (1:(n_topics / 2))))
  
  nonunique_mods = length(unique(mod_df_picks$topics)) < n_topics # Check that we have unique topics per module.
  
  if(nonunique_mods==TRUE){
    
    ## Do a brute force option if we return non-unique modules.
    
    mod_df = rbind(df1, df2)
    #mod_df$order_freq = rep(seq(1:4),2) # Record ordering, just in case
    
    mod_df_picks = data.frame(mod = c("Module1","Module2","Module1","Module2"),
                              topic = c(NA)) # Pre-allocate dataframe space
    
    topics_left = selected_topics
    
    for(i in 1:nrow(mod_df_picks)) {
      
      # Order of the pre-allocated dataframe is used: the first pick 
      # for module one is recorded, then the first pick for module
      # two, then second choice for module one, and second choice for module two.
      # Enforces unique topics for each module.
      
      mod = mod_df_picks$mod[i]
      mod_df_picks$topic[i] = first(mod_df$topics[mod_df$mod==mod])  
      
      mod_df = subset(mod_df, topics != mod_df_picks$topic[i])
        
    }
  }
  
  return(mod_df_picks)
}

select_mentors = function(mentor_pref_df, topics_df, n_mentors) {
  
  ## A UDF which will select a total of n ideal mentors
  # for a dataframe of module topics. It will then assign those
  # mentors topics within each module.
  
  mentor_pref_df = mentor_pref_df %>%
    filter(Skill %in% topics_df$topics) # Subset to just needed topics
  
  mod1topics = topics_df$topics[topics_df$mod=="Module1"]
  mod2topics = topics_df$topics[topics_df$mod=="Module2"] 
  
  unique_rankings = unique(mentor_pref_df$sum_ranking)
  
  selected_mentors = "NA"
  i = 1 
  
  while(length(selected_mentors) < n_mentors) {
  
  mod1_mentors = mentor_pref_df %>% 
    filter(Skill %in% mod1topics & sum_ranking >= unique_rankings[i]) %>%
    select(Mentor.Name, Skill, sum_ranking)
  
  mod2_mentors = mentor_pref_df %>% 
    filter(Skill %in% mod2topics & sum_ranking >= unique_rankings[i]) %>%
    select(Mentor.Name, Skill, sum_ranking)
  
  total = c(mod1_mentors$Mentor.Name, mod2_mentors$Mentor.Name)
  selected_mentors =  unique(total[duplicated(total)==T])
  
  last_unique_rankings_value = unique_rankings[i]  
  i = i+1
  }
  
  if(length(selected_mentors) > n_mentors) {
    
    selected_mentors = sample(selected_mentors, n_mentors, replace = F)
    
  }
  
  ## Now assign skills to each mentor for each module
  
  df = data.frame(Mentor = rep(selected_mentors,2),
                  Breakout_Room = rep(seq(1,n_mentors,by=1),2),
                  Module = rep(1:2, each = n_mentors),
                  Skill = rep( as.character(NA), n_mentors*2))
  
  df$Mentor = as.character(df$Mentor)
  df$Skill = as.character(df$Skill)
  
  mod1_mentors = mod1_mentors %>%
    filter(Mentor.Name %in% selected_mentors)
  
  mod2_mentors = mod2_mentors %>%
    filter(Mentor.Name %in% selected_mentors)
  
  n = as.numeric(nrow(df[is.na(df$Skill) & df$Module==1,])) # How many skills left to be assigned, Module1
  available = selected_mentors  
  max_n_topic = ceiling(n_mentors / 2) # Set a max number of subskills / breakout room sessions
  
  
  while(n > 0) { # Do this until all skills for module 1 are filled in
    
  mod1_df = mod1_mentors %>%
    mutate(Module = 1) %>%
    filter(Mentor.Name %in% available) %>% 
    filter(sum_ranking >= last_unique_rankings_value) %>%
    mutate(n_row = n()) %>%
    group_by(Skill) %>%
    mutate(n_skill = n()) %>%
    ungroup() %>%
    mutate(prob = ((n_row - n_skill)/n_skill)/n_row) %>%
    select(-c(n_row, n_skill))
  
  mod1_df$prob[mod1_df$prob==0] = 1
  pick = sample_n(mod1_df, 1, weight = prob)  
    
  df$Skill[df$Mentor == pick$Mentor.Name & pick$Module == df$Module] <- as.character(pick$Skill)
  
  n = as.numeric(nrow(df[is.na(df$Skill) & df$Module==1,])) # How many skills left to be assigned, Module1
  available = available[available!= pick$Mentor.Name]
  
  skill_counts = as.data.frame(table(df$Skill[df$Module==1])) # Track how many skills are being assigned.
  colnames(skill_counts)[1] = "Skill"
  
  n_topic1 = as.numeric(skill_counts$Freq[skill_counts$Skill==mod1topics[1]])
  n_topic2 = as.numeric(skill_counts$Freq[skill_counts$Skill==mod1topics[2]])
  max(n_topic1, n_topic2)
  
  }
  
  n = as.numeric(nrow(df[is.na(df$Skill) & df$Module==2,])) # How many skills left to be assigned, Module2
  available = selected_mentors  
  #max_n_topic = ceiling(n_mentors / 2) # Set a max number of subskills / breakout room sessions
  
  
  while(n > 0) { # Do this until all skills for module 2 are filled in
    
    mod_df = mod2_mentors %>%
      mutate(Module = 2) %>%
      filter(Mentor.Name %in% available) %>% 
      filter(sum_ranking >= last_unique_rankings_value) %>%
      mutate(n_row = n()) %>%
      group_by(Skill) %>%
      mutate(n_skill = n()) %>%
      ungroup() %>%
      mutate(prob = ((n_row - n_skill)/n_skill)/n_row) %>%
      select(-c(n_row, n_skill))
    
    mod_df$prob[mod_df$prob==0] = 1
    pick = sample_n(mod_df, 1, weight = prob)  
    
    df$Skill[df$Mentor == pick$Mentor.Name & pick$Module == df$Module] <- as.character(pick$Skill)
    
    n = as.numeric(nrow(df[is.na(df$Skill) & df$Module==2,])) # How many skills left to be assigned, Module1
    available = available[available!= pick$Mentor.Name]
    
    skill_counts = as.data.frame(table(df$Skill[df$Module==2])) # Track how many skills are being assigned.
    colnames(skill_counts)[1] = "Skill"
    
    n_topic1 = as.numeric(skill_counts$Freq[skill_counts$Skill==mod2topics[1]])
    n_topic2 = as.numeric(skill_counts$Freq[skill_counts$Skill==mod2topics[2]])
    #max(n_topic1, n_topic2)
    
  }
  
  return(df)
}

assign_mentees = function(mentor_module_df, mentee_df) {
  
  unique_mentee = unique(mentee_df$Mentee)
  new_mentee = data.frame(Mentee.Name = rep(unique_mentee,2),
                          Module = rep(1:2, each = length(unique_mentee)),
                          SK = as.character("NA"))
  
  new_mentee$Mentee.Name = as.character(new_mentee$Mentee.Name)
  new_mentee$SK = as.character(new_mentee$SK)
  
  new_mentee$SK[new_mentee$Module=="1"] = ff(mentee_df$MOD1_TOPIC, skills_map$character_name, as.character(skills_map$SK), ignore.case = T)
  new_mentee$SK[new_mentee$Module=="2"] = ff(mentee_df$MOD2_TOPIC, skills_map$character_name, as.character(skills_map$SK), ignore.case = T)
  
  new_mentee_mod_skills = subset(new_mentee, SK %in% module_topics$topics)
  
  mentor_module_df$mentee1 = as.character(NA) # Pre-allocate space in the dataframe for mentee assignment
  mentor_module_df$mentee2 = as.character(NA)
  mentor_module_df$mentee3 = as.character(NA)
  
  ## For assignation purposes, convert to long. 
  d_long = melt(mentor_module_df[c(1,3:7)], id.vars = c("Mentor","Module","Skill"))
  colnames(d_long) = c("Mentor","Module","Skill","Mentee","Mentee.Name")
  d_long = d_long %>% arrange(Module, Mentee)
  
  for(i in 1:nrow(new_mentee)) {
    
  next_skill  = new_mentee$SK[i]
  next_mentee = new_mentee$Mentee.Name[i]

  available_slots = as.numeric(length(d_long$Mentee.Name[d_long$Skill==next_skill & d_long$Mentee.Name!="NA"])) # How many slots available?
  match       = next_skill %in% module_topics$topics & available_slots > 0
  
  if(match==TRUE){
    
    counts = as.data.frame(table(d_long$Skill[d_long$Mentee.Name!="NA"]))
    colnames(counts)[1] = "Skill"
    
    if(next_skill %in% counts$Skill) {
    j = as.numeric(counts$Freq[counts$Skill==next_skill]) + 1
    d_long$Mentee.Name[d_long$Skill==next_skill][j] = new_mentee$Mentee.Name[i]
    
    } else {
      
      j = 1
      d_long$Mentee.Name[d_long$Skill==next_skill][j] = new_mentee$Mentee.Name[i]
      
        }
      }
  
  }
  
  d_wide = dcast(d_long, Mentor + Module + Skill ~ Mentee, value = "Mentee.Name")
  return(d_wide)    
}