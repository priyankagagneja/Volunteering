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
    
    mod_df_picks = data.frame(mod = rep(c("Module1","Module2"), each = n_topics/2),
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
    filter(Skill %in% topics_df$topic) # Subset to just needed topics
  
  topics_df$mod = as.character(topics_df$mod)
  mod1topics = topics_df$topic[topics_df$mod=="Module1"]
  mod2topics = topics_df$topic[topics_df$mod=="Module2"] 
  
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
  #max(n_topic1, n_topic2)
  
  }
  
  n = as.numeric(nrow(df[is.na(df$Skill) & df$Module==2,])) # How many skills left to be assigned, Module2
  available = selected_mentors  
  #max_n_topic = ceiling(n_mentors / 2) # Set a max number of subskills / breakout room sessions
  
  
  while(n > 0) { # Do this until all skills for module 2 are filled in
    
    mod_df = mod2_mentors %>%
      mutate(Module = 2) %>%
      filter(Mentor.Name %in% available) %>% 
      #filter(sum_ranking >= last_unique_rankings_value) %>%
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

find_ideal_module = function(x1, x2, n_topics) {
  
  ## A UDF which examines how many mentees will be naturally
  # fitted into two modules of n topics each, and optimizes that
  # selection so as many mentees as appeased as possible.
  
  library(gtools)
  
  combined_topics = c(x1, x2)
  v1 = sort(as.character(unique(x1)))
  v2 = sort(as.character(unique(x2)))

  combos_topic1 = combinations(length(v1), (n_topics/2), v=v1, repeats.allowed=FALSE)
  combos_topic2 = combinations(length(v2), (n_topics/2), v=v2, repeats.allowed=FALSE)
  combos_topic1 = data.frame(combos_topic1)
  combos_topic2 = data.frame(combos_topic2)
  combos_topic1[,1:ncol(combos_topic1)] = sapply(combos_topic1[,1:ncol(combos_topic1)], function(x) FUN = as.character(x))
  combos_topic2[,1:ncol(combos_topic2)] = sapply(combos_topic2[,1:ncol(combos_topic2)], function(x) FUN = as.character(x))
  
  exp1 = combos_topic1[rep(1:nrow(combos_topic1), each = nrow(combos_topic2)),] 
  exp2 = combos_topic2[rep(1:nrow(combos_topic2), times = nrow(combos_topic1)),]
  df = cbind(exp1, exp2)
  rownames(df) = NULL
  df[,1:ncol(df)] = sapply(df[,1:ncol(df)], function(x) FUN = as.character(x)) # format as character
  colnames(df) = c('sk1','sk2','sk3','sk4','sk5','sk6')
  
  #df$count <- apply(df[,1:6], 1, function(x) length(unique(x)))
  
  #df = df %>% filter(count == n_topics) %>% select(-count) # try only the unique combos.
  df$n_matches = NA # Ideally this number would be 2*n mentees

  for(i in 1:nrow(df)){
      
      t1 = as.character(unlist(c(df[i,1:3])))
      t2 = as.character(unlist(c(df[i,4:6])))

      freq1 = data.frame(table(x1))
      n_mod1 = freq1 %>% mutate(x1 = as.character(x1)) %>% 
        filter(x1 %in% t1) %>% mutate(n_mod1 = sum(Freq, na.rm = T)) %>% 
        slice(n=1) %>% select(n_mod1)
      n_mod1 = as.numeric(n_mod1$n_mod1)
      
      freq2 = data.frame(table(x2))
      n_mod2 = freq2 %>% mutate(x2 = as.character(x2)) %>% 
        filter(x2 %in% t2) %>% mutate(n_mod2 = sum(Freq, na.rm = T)) %>% 
        slice(n=1) %>% select(n_mod2)
      n_mod2 = as.numeric(n_mod2$n_mod2)
      
      df$n_matches[i] = n_mod1 + n_mod2
    
    
  }
  
  final_df = df %>%
    arrange(desc(n_matches)) %>%
    filter(n_matches == max(n_matches))
  
  if(nrow(final_df) > 1) {
    
    final_df = sample_n(final_df, 1)
    
  }
  
  long_final_df = data.frame(mod = rep(c("Module1","Module2"), each = (n_topics/2)))
  long_final_df$mod = as.character(long_final_df$mod)
  long_final_df$topic = as.character(final_df[1,1:n_topics])
  
  return(long_final_df)
  
}

define_modules_v2 = function(module_topics, topics1, topics2) {
  
  ## This function depends on the output of the previous function.
  # The functions do not need to be run independently.
  # The selected topics from the 'find_ideal_module' function are 
  # used to generate module topics.
  
  # idea here is to see the minimum number of breakout rooms needed given max_mentees
  # then randomly assign topic of selected topics per module to fill space

  df1 = as.data.frame(table(topics1))
  
  df1 = df1 %>% 
    mutate(topics = as.character(topics1)) %>%
  arrange(desc(Freq)) %>% filter(topics %in% module_topics$topic[module_topics$mod=="Module1"]) %>%
    select(topics, count = Freq)
  df1$mod = "Module1"
  
  rooms_mod1 = data.frame(topic = as.character(df1$topics),
                          rooms = NA)
  rooms_mod1$topic = as.character(rooms_mod1$topic)
  
  for(i in 1:nrow(rooms_mod1)){
    
    topic = rooms_mod1$topic[i]
    
    topic_freq  = as.numeric(df1$count[df1$topics==topic])
    rooms_mod1$rooms[i] <- ceiling(topic_freq/max_mentees) 
    
  }
  
  # Will this number of rooms accomodate all mentees? 
  
  current_assigned = sum(rooms_mod1$rooms) * max_mentees
  not_assigned = nrow(mentee) - current_assigned
  n_rooms_needed = ceiling(not_assigned / max_mentees) # how many more rooms are necessary?
  
  additional_rooms = sample(module_topics$topic[module_topics$mod=="Module1"], n_rooms_needed, replace = T)
  additional_rooms = data.frame(table(additional_rooms))
  additional_rooms$topic = as.character(additional_rooms$additional_rooms)
  
    rooms_mod1 = merge(rooms_mod1, additional_rooms[,2:3], by = "topic", all = T)
    rooms_mod1$Freq[is.na(rooms_mod1$Freq)] <- 0
    rooms_mod1 = rooms_mod1 %>%
    group_by(topic) %>%
    mutate(final_rooms = rooms + Freq) %>%
    ungroup() %>%
    select(topic, final_rooms) 
  
  ## Now run the assignments for module 2
  df2 = as.data.frame(table(topics2))
  
  df2 = df2 %>% 
    mutate(topics = as.character(topics2)) %>%
    arrange(desc(Freq)) %>% filter(topics %in% module_topics$topic[module_topics$mod=="Module2"]) %>%
    select(topics, count = Freq)
  df2$mod = "Module2"
  
  rooms_mod2 = data.frame(topic = as.character(df2$topics),
                          rooms = NA)
  rooms_mod2$topic = as.character(rooms_mod2$topic)
  
  for(i in 1:nrow(rooms_mod2)){
    
    topic = rooms_mod2$topic[i]
    
    topic_freq  = as.numeric(df2$count[df2$topics==topic])
    rooms_mod2$rooms[i] <- ceiling(topic_freq/max_mentees) 
    
  }
  
  # Will this number of rooms accomodate all mentees? 
  
  current_assigned = sum(rooms_mod2$rooms) * max_mentees
  not_assigned = nrow(mentee) - current_assigned
  n_rooms_needed = ceiling(not_assigned / max_mentees) # how many more rooms are necessary?
  
  additional_rooms = sample(module_topics$topic[module_topics$mod=="Module2"], n_rooms_needed, replace = T)
  additional_rooms = data.frame(table(additional_rooms))
  additional_rooms$topic = as.character(additional_rooms$additional_rooms)
  
  rooms_mod2 = merge(rooms_mod2, additional_rooms[,2:3], by = "topic", all = T)
  rooms_mod2$Freq[is.na(rooms_mod2$Freq)] <- 0
  rooms_mod2 = rooms_mod2 %>%
    group_by(topic) %>%
    mutate(final_rooms = rooms + Freq) %>%
    ungroup() %>%
    select(topic, final_rooms) 
  
  # Generate final module with breakout rooms
  
  module1 = data.frame(topic = rep(rooms_mod1$topic, times = rooms_mod1$final_rooms))
  module1$topic = as.character( module1$topic)
  module1$breakout_room = seq(1,nrow(module1), by = 1)
  module1$mod = "Module1"
  
  module2 = data.frame(topic = rep(rooms_mod2$topic, times = rooms_mod2$final_rooms))
  module2$topic = as.character( module2$topic)
  module2$breakout_room = seq(1,nrow(module2), by = 1)
  module2$mod = "Module2"
  
  module_w_breakouts = rbind(module1, module2)
  
  return(module_w_breakouts)
}

select_mentors_v2 = function(mentor_pref_df, modules_defined, n_mentors) {
  
  ## A UDF which will assign mentors best suited to each topic.
  
  mentor_pref_df = mentor_pref_df %>%
    filter(Skill %in% modules_defined$topic) # Subset to just needed topics
  
  modules_defined$mod = as.character(modules_defined$mod)
  mod1topics = unique(modules_defined$topic[modules_defined$mod=="Module1"])
  mod2topics = unique(modules_defined$topic[modules_defined$mod=="Module2"]) 
  
  unique_rankings = unique(mentor_pref_df$sum_ranking)
  
  ## Now assign skills to each mentor for each module
  
  df = data.frame(table(modules_defined$topic, modules_defined$mod))
  df$topic = as.character(df$Var1)
  df$mod = as.character(df$Var2)
  df$Mentor = as.character(NA)
  
  df = subset(df, Freq!=0, -c(Var1, Var2, Freq))

  mod1_mentors = mentor_pref_df %>% # select mentors with skills in module 1
    filter(Skill %in% mod1topics & !is.na(Ranking)) %>%
    mutate(mod = "Module1")
  
  mod2_mentors = mentor_pref_df %>% # select mentors with skills in module 2
    filter(Skill %in% mod2topics & !is.na(Ranking)) %>%
    mutate(mod = "Module2")
  
  combined_mentors = c(unique(mod1_mentors$Mentor.Name),unique(mod2_mentors$Mentor.Name))
  combined_mentors = combined_mentors[duplicated(combined_mentors) == TRUE] # Subset to mentors with skills in both modules.
  
  module_mentors = rbind(mod1_mentors, mod2_mentors) # Total subset to pull from
  module_mentors = module_mentors %>%
    filter(Mentor.Name %in% combined_mentors)
  
  # Do combinations of mentors and assess fit to breakout rooms.
  
  combos = combinations(length(combined_mentors), min_mentors, v=combined_mentors, repeats.allowed=FALSE)
  combos = data.frame(combos)
  combos[,1:ncol(combos)] = sapply(combos[,1:ncol(combos)], function(x) FUN = as.character(x))
  colnames(combos) = c('mentor1','mentor2','mentor3','mentor4','mentor5','mentor6','mentor7','mentor8','mentor9')
  
  topics = expand.grid(mod1topics, mod2topics)
  topics[,1:ncol(topics)] = sapply(topics[,1:ncol(topics)], function(x) FUN = as.character(x))
  colnames(topics) = c('Module1_SK','Module2_SK')

  
  # Go through and find the best combination of mentors for both modules, and assign to breakout room.
  
  exp1 = topics[rep(1:nrow(topics), times = nrow(combos)),]
  exp2 = combos[rep(1:nrow(combos), each = nrow(topics)),] 
  df = cbind(exp1, exp2)
  rownames(df) = NULL
  colnames(df)[1:2] = c('Module1_SK','Module2_SK')
  df$combo = rep(seq(1,nrow(combos), by = 1), each = nrow(topics))
  df$mentor1_rank = NA
  df$mentor2_rank = NA
  df$mentor3_rank = NA
  df$mentor4_rank = NA
  df$mentor5_rank = NA
  df$mentor6_rank = NA
  df$mentor7_rank = NA
  df$mentor8_rank = NA
  df$mentor9_rank = NA
  
  # Now go through and test each set of 9 mentors for appropriateness of each module.
  # There are 9 unique possible combinations of topics breakout room configurations. 
  
    
  for(i in 1:nrow(df)){ # This will take awhile to run through.
    
    next_skills = as.character(df[i,1:2])
    
    for(j in 3:11) {
      
      next_mentor = df[i,j]
      
      if(next_skills[1]==next_skills[2]){
      
      Skills_Rank = module_mentors %>%
        filter(Mentor.Name %in% next_mentor & Skill %in% next_skills) %>%
        mutate(skill_rank = sum(sum_ranking, na.rm = T)) %>%
        slice(n=1) %>%
        select(skill_rank)
        
    df[i,j+10] = as.numeric(Skills_Rank$skill_rank)  
    
      }else{
        
        Skills_Rank = module_mentors %>%
          filter(Mentor.Name %in% next_mentor & Skill %in% next_skills) %>%
          group_by(Skill) %>%
          slice(n=1) %>%
          ungroup() %>%
          mutate(skill_rank = sum(sum_ranking, na.rm = T)) %>%
        slice(n=1) %>%
        select(skill_rank)
        
        df[i,j+10] = as.numeric(Skills_Rank$skill_rank)  
        
        
      }
    
    }
    
  }
  
  df$sum = rowSums(df[,13:21])
  
  df1 = df %>% group_by(combo) %>%
    arrange(desc(sum)) %>%
    mutate(combo_sum = sum(sum)) %>%
    slice(n=9) %>%
    ungroup() %>%
    arrange(desc(combo_sum))
    
  maximized_combo = df1 %>%
    arrange(desc(combo_sum)) %>%
    slice(n=1) %>%
    select(combo, sum, combo_sum)
  
  breakout_selection = df %>% 
    filter(combo == maximized_combo$combo) 
  
  breakout_selection$index = seq(1,nrow(breakout_selection), by = 1)
  # Now the task is to identify which combinations maximize the variable 'sum'  
  # and adequately assign mentors.
  
  # Set the number of rooms needed, per module and skill
  rooms = as.data.frame(table(modules_defined[c(1,3)])) # This is the initial starting number.
  rooms = subset(rooms, Freq!=0)
  
  ## Order the mentors within the breakout selection.
  long = melt(breakout_selection[,c(23,13:21)], id.vars = "index")
  long$variable = as.character(long$variable)
  
  long = long %>%
    group_by(index) %>%
    arrange(desc(value)) %>%
    mutate(position = seq(1,n(),1)) %>%
    ungroup()
  
  long$variable = ff(long$variable, colnames(breakout_selection[13:21]), breakout_selection[1,3:11], ignore.case = T)
  
  brs = subset(breakout_selection, select = c('index','Module1_SK','Module2_SK'))
  brs$Mentor.Name = NA
  
  mentors_left = unique(long$variable)
  rooms_left = unique(long$index)
  
  while(length(mentors_left) > 0){
    
    slice = long %>%
      filter(variable %in% mentors_left & index %in% rooms_left) %>% arrange(position, desc(value)) %>%
      slice(n=1)
    
    brs$Mentor.Name[brs$index==slice$index] <- slice$variable
    
    mentors_left = mentors_left[mentors_left!= slice$variable]
    rooms_left = rooms_left[rooms_left!= slice$index]
  }
  
  colnames(brs)[1] = "breakout_room"
  
  brs_long = melt(brs,id.vars = c("breakout_room","Mentor.Name"))
  colnames(brs_long) = c('breakout_room','mentor','mod','topic')
  brs_long$mod = as.character(brs_long$mod)
  brs_long = brs_long[,c(4,1,3,2)]
  brs_long$mod = ff(brs_long$mod, unique(brs_long$mod), c("Module1","Module2"), ignore.case = T)
  
  return(brs_long)
}
