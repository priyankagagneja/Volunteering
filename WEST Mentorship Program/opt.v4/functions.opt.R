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

define_modules_v4 = function(mentee_df, n_mentors) {
  
  ## A UDF which examines how many mentees will be naturally
  # fitted into two modules with n mentors, and optimizes that
  # selection so that as many mentees as appeased as possible.
  
  # Consider all possible combinations of mentee and module.
  # Then consider which of those combinations allows for the 
  # greatest number of subskills to be tackled.
  df = mentee_df[complete.cases(mentee_df)==T,]
  
  module_df = data.frame(table(df$Skill.SubSkill[df$Ranking==1])) %>%
    mutate(n_rooms = ceiling(Freq/3)) %>%
    group_by(n_rooms) %>%
    sample_frac(1) %>% # Randomly shuffle the subskills within ranking
    arrange(desc(n_rooms)) %>%
    ungroup() %>%
    mutate(Skill.SubSkill = as.character(Var1)) %>%
    select(Skill.SubSkill, n_rooms) %>%
    mutate(Mentee.Name = NA)

  # Create a tracker for who has yet to be assigned
  remaining_assignments = data.frame(Mentee.Name = rep(unique(mentee_df$Mentee.Name), times = 2),
                                     mod = rep(c("Module1","Module2"), each = length(unique(mentee_df$Mentee.Name))))

  rank1 = mentee_df  %>%  # Filter to rank 2, unassigned mentees
    filter(Ranking==1) %>%
    filter(Mentee.Name %in% remaining_assignments$Mentee.Name)
  
  
  # Now go through and try each mentee to the subskill.
  # Object is to find the best selection of 17 rooms over two modules
# Set up loop parameters
try = 1
allowable_tries = 20
df = mentee_df[complete.cases(mentee_df==T) & mentee_df$Ranking == 1,] # reset the program
data = module_df %>%
  sample_frac(1)
result = list(data)
count = 1
          
while(nrow(data[is.na(data$Mentee.Name),]) > 0 & length(result) < (allowable_tries+1)) {

            slice = rank1 %>%
              filter(Skill.SubSkill == data$Skill.SubSkill[count]) %>%
              sample_frac(1) %>%
              slice(n =1) %>%
              select(Mentee.Name, Skill.SubSkill)
            
            if(length(slice$Mentee.Name)==0) {
              data$Mentee.Name[count] = NA
            }else{
              data$Mentee.Name[count] = slice$Mentee.Name
              
              df = df %>%
                mutate(drop = ifelse(Mentee.Name==slice$Mentee.Name & Skill.SubSkill == slice$Skill.SubSkill, TRUE, NA)) %>%
                group_by(Mentee.Name, Skill) %>%
                fill(drop, .direction = "downup") %>%
                filter(is.na(drop)) %>%
                ungroup() %>%
                select(-drop)
            }
            
            if(count==nrow(data)){
            
              d = list(data)
              result <- cbind(result, d)
              df = mentee_df[complete.cases(mentee_df==T) & mentee_df$Ranking == 1,] # reset the program
              data = module_df %>%
                sample_frac(1)
              count = 1
              try = try + 1
              
     
            } else{
              
              count = count + 1
            }
          }

## Save which `try` minimizes the number of non-assigned mentees.
count = 1
n_miss = data.frame(result_number = seq(1, length(result), by = 1),
                    n_missing = NA)
while(count < length(result)){
  
  try = result[[count]]
  n_miss$n_missing[count] = nrow(try[is.na(try$Mentee.Name),])
  
  count = count + 1
}

min_miss = n_miss %>% # Filter to the minimum number of missing values per try
  mutate(min = min(n_missing, na.rm = T)) %>%
  filter(n_missing == min) %>%
  select(result_number)

  ## Sanity check - remove later
  #check = result[[first(min_miss$result_number)]]
  #check$Skill = substr(check$Skill.SubSkill, 1, 3)
  #check$Skill = ff(check$Skill, as.character(skills_map$SK),  as.character(skills_map$character_name), ignore.case = T)
  # Looks good up to this point - main skills are on point with what we expect.

# Split rooms into two modules so that room space is maximized but mentees aren't repeated
# in the same module
  
# Something is going wrong here.
  # We need to arrange subskills by mentee
  dataset = 1
  result2 = list() # Store the new results
  while(dataset < (length(min_miss$result_number)+1)){
    
  index = min_miss$result_number[dataset]
  temp =  result[[index]]
  
  # Ok, so split the dupe column so no one is replicated.
  temp$dupe = duplicated(temp$Mentee.Name)
  temp$index = seq(1,nrow(temp),by=1)
  n_rooms_mod1 = ceiling(nrow(temp)/2)
  n_rooms_mod2 = nrow(temp) - n_rooms_mod1
  
  mod1 = temp %>%
    filter(dupe==TRUE) %>%
    #sample_n(n_rooms_mod1) %>%
    mutate(mod = "Module1")
  
  mod2 = temp %>%
    filter(!index %in% mod1$index) #%>%
    #mutate(mod = "Module2")
  
  mod1_extra = mod2 %>% 
    filter(!Mentee.Name %in% mod1$Mentee.Name) %>%
    sample_n((n_rooms_mod1 - nrow(mod1)), replace = F) %>%
    mutate(mod = "Module1")
  
  mod1 = rbind(mod1, mod1_extra)
  
  mod2 = temp %>%
    filter(!index %in% mod1$index) %>%
    mutate(mod = "Module2")
  
  data = rbind(mod1, mod2)
  data = data %>% select(Skill.SubSkill, Mentee.Name, mod)
  #table(data1$Mentee.Name, data1$mod) # Make sure we have just one mentee per module
  
  d = list(data)
  result2 <- cbind(result2, d)
  dataset = dataset + 1
} 
  
# Next we select the configuration which minimizes the number of rooms 
  
  count = 2
  n_rooms = data.frame(result_number = seq(count, length(result2), by = 1),
                       n_rooms = NA)
  while(count < length(result2)){
    
    try = result2[[count]]
    #temp = data.frame(table(try$Skill.SubSkill, try$mod))
    temp = data.frame(table(try$Skill.SubSkill))
    temp = temp %>%
      mutate(Skill.SubSkill = as.character(Var1)) %>%
      mutate(mod = as.character(Var2)) %>%
      filter(Freq>0) %>%
      mutate(n_rooms = sum(ceiling(Freq/3))) %>%
      #mutate(n_rooms = ceiling(n/3)) %>%
      select(n_rooms) %>%
      slice(n=1)
    
    n_rooms$n_rooms[count] = temp$n_rooms
    
    count = count + 1
  }
  
  min_rooms = n_rooms %>% # Filter to the minimum number of missing values per try
    mutate(min = min(n_rooms, na.rm = T)) %>%
    filter(n_rooms == min) %>%
    select(result_number) %>%
    sample_n(1)
  
  #index = min_rooms$result_number[1]
  
  index = 5
  
  assign = data.frame(result2[[index]])
  #### Skip the above because it's changing the skills ... 
  # If we're going to skip, though, we still need to assign a module.
  #assign = result[[first(min_miss$result_number)]]
  
  assign = assign %>%
    group_by(mod, Skill.SubSkill) %>%
    mutate(Mentee = paste("Mentee",seq(1,n(),1),sep=".")) %>%
    ungroup()

  assign_wide = dcast(assign, mod + Skill.SubSkill ~ Mentee, value.var ="Mentee.Name")
  if(ncol(assign_wide) ==3 ){
    new_cols = paste("Mentee",c(2,3),sep = ".")
    assign_wide[new_cols] = NA
  } else if(ncol(assign_wide) == 4 ){
    new_cols = paste("Mentee",c(3),sep = ".")
    assign_wide[new_cols] = NA
  }else{
    assign_wide = assign_wide
  }
  
  # Just doing a sanity check here - not to leave in
  #assign_wide$Skill = substr(assign_wide$Skill.SubSkill, 1, 3)
  #assign_wide$Skill = ff(assign_wide$Skill, as.character(skills_map$SK),  as.character(skills_map$character_name), ignore.case = T)
  #assign$Skill = substr(assign$Skill.SubSkill, 1, 3)
  #assign$Skill = ff(assign$Skill, as.character(skills_map$SK),  as.character(skills_map$character_name), ignore.case = T)
  
  
  # And back to long with all three mentee places allocated
  assign_long = melt(assign_wide, id.vars = c("mod","Skill.SubSkill"))

  ## Now consider rank = 2 skills, so that we can assign unassigned mentees
  
  df = mentee_df[complete.cases(mentee_df)==T,]
  df$Ranking = as.numeric(df$Ranking)
    
  # Filter out mentees already assigned to both modules.
  assigned = assign %>% group_by(Mentee.Name) %>% 
    mutate(n = n()) %>%
    filter(n==2) %>%
    select(Mentee.Name) %>%
    slice(n=1) %>%
    ungroup()
  
  # We need to consider remaining mentees per module now. First, grab the modules for which already assigned
  # mentees still need a skill for
  remaining_assignments = data.frame(table(assign$Mentee.Name, assign$mod))
  remaining_assignments = remaining_assignments %>%
    mutate(Mentee.Name = as.character(Var1)) %>%
    mutate(mod = as.character(Var2)) %>%
    filter(Freq==0) %>%
    select(-c(Var1, Var2, Freq))
   
  unique_mentee = unique(df$Mentee.Name)
  unassigned = unique_mentee[!(unique_mentee %in% assigned$Mentee.Name)]
  no_module_assigned = unassigned[!(unassigned %in% remaining_assignments$Mentee.Name)]
  
  extra_rows = data.frame(Mentee.Name = rep(no_module_assigned, times = 2),
                          mod = rep(c("Module1","Module2"), each = length(no_module_assigned)))
  
  # Now we have a complete dataframe of everyone yet to be assigned, by module.
  remaining_assignments = rbind(remaining_assignments, extra_rows) 
  
  rank2 = mentee_df  %>%  # Filter to rank 2, unassigned mentees
    filter(Ranking<=2) %>%
    filter(Mentee.Name %in% unassigned)
  
  ## Maximize use of current subskills & room space
  # This is easier than the initial room optimization. Now we try
  # to make use of existing rooms. So to move forward, calculate the
  # number of spaces per subskill available. Then go through the process 
  # of assigning remaining mentees and maximizing space. This time, 
  # we need to be mindful of placing people in module 1 / module 2.
  
  assign_long = melt(assign_wide, id.vars = c("mod","Skill.SubSkill")) # Ensure data are in long format
  colnames(assign_long) = c("mod","Skill.SubSkill","Mentee","Mentee.Name")
  assign_long$Mentee = as.character(assign_long$Mentee )
  
  long_orig = assign_long
  long_orig_na = long_orig %>% filter(is.na(Mentee.Name))
  
  
  # This assignment could take awhile, depending on how many times it needs to go through the sampling procedure
  # to ensure that everyone who may possible be assigned has a spot.
  # Set up the loop
  try = 1
  allowable_tries = 10
  df = remaining_assignments # reset the program
  data = long_orig_na %>%
    sample_frac(1)
  result = list(data)
  count = 1
  
while(nrow(df) > 0 & length(result) < (allowable_tries+1)) {
            
            slice = rank2 %>%
              filter(Mentee.Name %in% df$Mentee.Name & !is.na(Ranking)) %>%
              filter(Skill.SubSkill == data$Skill.SubSkill[count]) %>%
              sample_frac(1) %>%
              slice(n=1) %>%
              select(Mentee.Name, Skill.SubSkill)
            
            # if the subskill and mentee name don't match for the module they need, continue
            if(length(slice$Mentee.Name)==0) {
              
              data$Mentee.Name[count] = as.character(NA) # Assign NA
              #df = df
              count = count + 1
              
            }else{
              
              module_avail = data$mod[data$Skill.SubSkill == slice$Skill.SubSkill]
              module_needed =  df$mod[df$Mentee.Name == slice$Mentee.Name]
              match = any(module_needed %in% module_avail)
              
              if(match){
              data$Mentee.Name[count] = slice$Mentee.Name # Assign mentee
            
              # Then drop that mentee / module combo
              df = df %>%
                mutate(drop = ifelse(Mentee.Name==slice$Mentee.Name & mod == data$mod[count], TRUE, FALSE)) %>%
                filter(drop == FALSE) %>%
                select(-drop)
              count = count + 1 
              }else{
                data$Mentee.Name[count] = as.character(NA) # Assign NA
                #df = df
                count = count + 1
              }
            }
            
            if((count-1)==nrow(data)){
              
              d = list(data)
              result <- cbind(result, d)
              df = remaining_assignments # reset the program
              data = long_orig_na %>%
                sample_frac(1)
              count = 1
              try = try + 1
            }
          } # Run the loop as long until number of tries is reached

## See which `try` minimizes the number of non-assigned mentees when using rank <= 2.
  count = 1
  n_miss = data.frame(result_number = seq(1, length(result), by = 1),
                      n_missing = NA)
  while(count < length(result)){
    
    try = result[[count]]
    n_miss$n_missing[count] = nrow(try[is.na(try$Mentee.Name),])
    
    count = count + 1
  }
  
  min_miss = n_miss %>% # Filter to the minimum number of missing values per try
    mutate(min = min(n_missing, na.rm = T)) %>%
    filter(n_missing == min) %>%
    select(result_number) %>%
    sample_n(1)
  
  rank_2_assignments = result[[min_miss$result_number]] # Keep this assignment
  
  #table(rank_2_assignments$Mentee.Name, rank_2_assignments$mod) # Check that only one person is assigned per module
  #Sanity check here
  #rank_2_assignments$Skill = substr(rank_2_assignments$Skill.SubSkill, 1, 3)
  #rank_2_assignments$Skill = ff(rank_2_assignments$Skill, as.character(skills_map$SK),  as.character(skills_map$character_name), ignore.case = T)
  
  
  # Rbind the rank 2 results with the assgin_long dataframe
  
  bind1 = long_orig %>% filter(!is.na(Mentee.Name)) # Select those already assigned
  assign_long_2 = rbind(bind1, rank_2_assignments) # Add those just assigned by rank 2 or less
  
  #assign_long_2$Skill = substr(assign_long_2$Skill.SubSkill, 1, 3)
  #assign_long_2$Skill = ff(assign_long_2$Skill, as.character(skills_map$SK),  as.character(skills_map$character_name), ignore.case = T)
  #table(assign_long_2$Mentee.Name, assign_long_2$mod)
  
  ################ Rank 3
  remaining_assignments = data.frame(table(assign_long_2$Mentee.Name, assign_long_2$mod))
  remaining_assignments = remaining_assignments %>%
    mutate(Mentee.Name = as.character(Var1)) %>%
    mutate(mod = as.character(Var2)) %>%
    filter(Freq==0) %>%
    select(-c(Var1, Var2, Freq))
  
  unique_mentee = unique(df$Mentee.Name)
  unassigned = unique_mentee[!(unique_mentee %in% assign_long_2$Mentee.Name)]
  no_module_assigned = unassigned[!(unassigned %in% remaining_assignments$Mentee.Name)]
  
  extra_rows = data.frame(Mentee.Name = rep(no_module_assigned, times = 2),
                          mod = rep(c("Module1","Module2"), each = length(no_module_assigned)))
  
  # Now we have a complete dataframe of everyone yet to be assigned, by module.
  remaining_assignments = rbind(remaining_assignments, extra_rows) 
  
  rank3 = mentee_df %>%  # Filter to rank 2, unassigned mentees
    filter(Ranking<=3) %>%
    filter(Mentee.Name %in% remaining_assignments$Mentee.Name)
  
  long_orig_na = assign_long_2 %>% filter(is.na(Mentee.Name))
  
  # Set up the loop
  try = 1
  allowable_tries = 100
  df = remaining_assignments # reset the program
  data = long_orig_na %>%
    sample_frac(1)
  result = list(data)
  count = 1
  
while(nrow(df) > 0 & length(result) < (allowable_tries+1)) {
    
    slice = rank3 %>%
      filter(Mentee.Name %in% df$Mentee.Name & !is.na(Ranking)) %>%
      filter(Skill.SubSkill == data$Skill.SubSkill[count]) %>%
      sample_frac(1) %>%
      slice(n=1) %>%
      select(Mentee.Name, Skill.SubSkill)
    
    # if the subskill and mentee name don't match for the module they need, continue
    if(length(slice$Mentee.Name)==0) {
      
      data$Mentee.Name[count] = as.character(NA) # Assign NA
      #df = df
      count = count + 1
      
    }else{
      
      module_avail = data$mod[data$Skill.SubSkill == slice$Skill.SubSkill]
      module_needed =  df$mod[df$Mentee.Name == slice$Mentee.Name]
      match = any(module_needed %in% module_avail)
      
      if(match){
        data$Mentee.Name[count] = slice$Mentee.Name # Assign mentee
        
        # Then drop that mentee / module combo
        df = df %>%
          mutate(drop = ifelse(Mentee.Name==slice$Mentee.Name & mod == data$mod[count], TRUE, FALSE)) %>%
          filter(drop == FALSE) %>%
          select(-drop)
        count = count + 1 
      }else{
        data$Mentee.Name[count] = as.character(NA) # Assign NA
        #df = df
        count = count + 1
      }
    }
    
    if((count-1)==nrow(data)){
      
      d = list(data)
      result <- cbind(result, d)
      df = remaining_assignments # reset the program
      data = long_orig_na %>%
        sample_frac(1)
      count = 1
      try = try + 1
    }
  }
  
  ## See which `try` minimizes the number of non-assigned mentees when using rank <= 3
  count = 1
  n_miss = data.frame(result_number = seq(1, length(result), by = 1),
                      n_missing = NA)
  while(count < length(result)){
    
    try = result[[count]]
    n_miss$n_missing[count] = nrow(try[is.na(try$Mentee.Name),])
    
    count = count + 1
  }
  
  min_miss = n_miss %>% # Filter to the minimum number of missing values per try
    mutate(min = min(n_missing, na.rm = T)) %>%
    filter(n_missing == min) %>%
    select(result_number) %>%
    sample_n(1)
  
  rank_3_assignments = result[[min_miss$result_number]] # Keep this assignment

  #rank_3_assignments$Skill = substr(rank_3_assignments$Skill.SubSkill, 1, 3)
  #rank_3_assignments$Skill = ff(rank_3_assignments$Skill, as.character(skills_map$SK),  as.character(skills_map$character_name), ignore.case = T)
  
  #table(rank_3_assignments$Mentee.Name, rank_3_assignments$mod) # Check that only one person is assigned per module
  
  # Rbind the rank 3 and above results with the assign_long dataframe
  
  if(length(unique(rank_3_assignments$Mentee.Name)) >1){
  bind1 = assign_long_2 %>% filter(!is.na(Mentee.Name)) # Select those already assigned
  assign_long_3 = rbind(bind1, rank_3_assignments) # Add those just assigned by rank 3 or less
  } else{
    
    assign_long_3 = assign_long_2
  }
  table(assign_long_3$Mentee.Name, assign_long_3$mod)
  
  #assign_long_3$Skill = substr(assign_long_3$Skill.SubSkill, 1, 3)
  #assign_long_3$Skill = ff(assign_long_3$Skill, as.character(skills_map$SK),  as.character(skills_map$character_name), ignore.case = T)
  
  
  ##### After all ranks have been considered
  remaining_assignments = data.frame(table(assign_long_3$Mentee.Name, assign_long_3$mod))
  remaining_assignments = remaining_assignments %>%
    mutate(Mentee.Name = as.character(Var1)) %>%
    mutate(mod = as.character(Var2)) %>%
    filter(Freq==0) %>%
    select(-c(Var1, Var2, Freq))
  
  n_rooms_mod = assign_long_3 %>% group_by(mod) %>%
    mutate(n_rooms = n()/3) %>%
    slice(n=1) %>%
    ungroup() %>%
    select(mod, n_rooms) %>%
    mutate(avail_rooms = n_mentors - n_rooms)
    
  
  ## Finally, go back and create new rooms based on mentees who have not been assigned
  
    # Where are new rooms needed? 
  space_needed = remaining_assignments %>% group_by(mod) %>% mutate(n_spaces = n()) %>%
    mutate(n_rooms = ceiling(n_spaces/3))%>% ungroup()
  
    # Where are new rooms available?
  assign_wide_3 = dcast(assign_long_3, mod + Skill.SubSkill ~ Mentee, value.var ="Mentee.Name")
  rooms = data.frame(table(assign_wide_3$mod))

  # Assign mentees to the available spaces
  
  # First determine which of their main skills they're not assigned to yet.
  already_assigned = assign_long_3 %>%
    filter(Mentee.Name %in% remaining_assignments$Mentee.Name) %>%
    mutate(Skill = substr(Skill.SubSkill, 1, 3))

    # find commonalities in their skill interests
  slice = mentee_df %>% filter(Mentee.Name %in% remaining_assignments$Mentee.Name & !is.na(Ranking)) 
  # If mentees to be assigned are already considered for one module, 
  # make sure they are not considered for the same skill ...
  for(i in 1:nrow(already_assigned)){
    
    next_mentee = already_assigned$Mentee.Name[i]
    next_skill = already_assigned$Skill[i]
    slice = slice %>%
      mutate(drop = ifelse(Mentee.Name==next_mentee & Skill == next_skill, "DROP","KEEP")) %>%
      filter(drop == "KEEP") %>%
      select(-drop)
    
  }
  
  
  max_skills = slice %>%
    group_by(Skill.SubSkill) %>%
    mutate(n = n()) 
  
    #arrange(desc(n)) %>%
    slice(n=1) %>%
    select(Skill.SubSkill)
  
  slice_skill = slice %>%
    filter(Skill.SubSkill == max_skills$Skill.SubSkill) %>%
    select(-n)
  
  slice_skill = max_skills %>% filter(n > 1) %>% select(Skill.SubSkill, Mentee.Name)
  slice_skill1 = slice %>% filter(!Mentee.Name %in% slice_skill$Mentee.Name) %>% 
    filter(Ranking==1) %>% slice(n=1)
  
  slice_skill = rbind(slice_skill, slice_skill1)
  
  space_needed = merge(space_needed, slice_skill, by = c("Mentee.Name"))
  space_needed = space_needed %>% select(-n_spaces)
  space_needed$Mentee = paste("Mentee",seq(1,nrow(space_needed),by=1),sep=".")
  space_needed = space_needed %>% select(mod, Skill.SubSkill, Mentee,Mentee.Name)
  
  assign_long_4 = rbind(assign_long_3, space_needed)
  assign_wide_4 = dcast(assign_long_4, mod + Skill.SubSkill ~ Mentee, value.var ="Mentee.Name")
  
  ## Final sanity check - fails .... 
  #assign_long_4$Skill = substr(assign_long_4$Skill.SubSkill, 1, 3)
  #assign_long_4$Skill = ff(assign_long_4$Skill, as.character(skills_map$SK),  as.character(skills_map$character_name), ignore.case = T)
  
  return(assign_wide_4)
  
}

select_mentors_v3= function(mentor_pref_df, modules_defined) {
  
  ## A UDF which will assign mentors best suited to each topic.
  
  mentor_pref_df = mentor_pref_df %>%
    filter(Skill %in% modules_defined$Skill & !is.na(Ranking)) # Subset to just needed topics
  
  modules_defined$mod = as.character(modules_defined$mod)
  mod1topics = unique(modules_defined$Skill[modules_defined$mod=="Module1"])
  mod2topics = unique(modules_defined$Skill[modules_defined$mod=="Module2"]) 
  modules_defined = modules_defined %>%
    group_by(mod) %>%
    mutate(breakout_room = seq(1,n(),by=1))%>%
    ungroup()
  ## Now assign skills to each mentor for each module
  
  df = data.frame(table(modules_defined$Skill, modules_defined$mod))
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
  
  library(gtools)
  
  mentors_needed = max(modules_defined$breakout_room)
  
  combos = combinations(length(combined_mentors), mentors_needed, v=combined_mentors, repeats.allowed=FALSE)
  combos = data.frame(combos)
  combos[,1:ncol(combos)] = sapply(combos[,1:ncol(combos)], function(x) FUN = as.character(x))
  colnames(combos) = paste("mentor",seq(1:mentors_needed),sep="")
  
  
  bo_rooms1  = data.frame(br = modules_defined$breakout_room[modules_defined$mod=="Module1"],
                          #Module2_BR = modules_defined$breakout_room[modules_defined$mod=="Module2"],
                          Module1_SK = modules_defined$Skill[modules_defined$mod=="Module1"])
  bo_rooms2 = data.frame(#Module1_BR = modules_defined$breakout_room[modules_defined$mod=="Module1"],
    br = modules_defined$breakout_room[modules_defined$mod=="Module2"],
    #Module1_SK = modules_defined$topic[modules_defined$mod=="Module1"])
    Module2_SK = modules_defined$Skill[modules_defined$mod=="Module2"])
  
  bo_rooms = merge(bo_rooms1, bo_rooms2, by = "br", all = T)
  colnames(bo_rooms)[1] = "Module1_BR"
  bo_rooms$Module2_BR = bo_rooms$Module1_BR
  bo_rooms$Module2_BR[is.na(bo_rooms$Module2_SK)] <- NA
  bo_rooms = bo_rooms[,c(1,4,2:3)] 
  
  # Go through and find the best combination of mentors for both modules, and assign to breakout room.
  
  exp1 = bo_rooms[rep(1:nrow(bo_rooms), times = nrow(combos)),]
  exp2 = combos[rep(1:nrow(combos), each = nrow(bo_rooms)),] 
  df = cbind(exp1, exp2)
  rownames(df) = NULL
  #colnames(df)[1:2] = c('Module1_SK','Module2_SK')
  df$combo = rep(seq(1,nrow(combos), by = 1), each = nrow(bo_rooms))
  
  new_cols = paste("Mentor",seq(1:mentors_needed),"rank",sep="") # Preallocate space for mentor rank
  df[new_cols] = NA
  
  # Now go through and test each set of 9 mentors for appropriateness of each module.
  # There are n unique possible combinations of topics breakout room configurations. 
  
  
  for(i in 1:nrow(df)){ # This will take awhile to run through.
    
    next_skills = as.character(df[i,3:4])
    if(any(is.na(next_skills))){
      
      next_skills = rep(next_skills[!is.na(next_skills)], 2)
      
    }
    
    for(j in 5:(mentors_needed+4)) {
      
      next_mentor = df[i,j]
      
      if(next_skills[1]==next_skills[2]){
        
        Skills_Rank = module_mentors %>%
          filter(Mentor.Name %in% next_mentor & Skill %in% next_skills) %>%
          mutate(skill_rank = sum(sum_ranking, na.rm = T)) %>%
          slice(n=1) %>%
          select(skill_rank)
        
        if(nrow(Skills_Rank)==0){
          df[i,j+(mentors_needed+1)] = 0
          
        } else{
          
          df[i,j+(mentors_needed+1)] = as.numeric(Skills_Rank$skill_rank)  
        }
      } else{
        
        Skills_Rank = module_mentors %>%
          filter(Mentor.Name %in% next_mentor & Skill %in% next_skills) %>%
          group_by(Skill) %>%
          slice(n=1) %>%
          ungroup() %>%
          mutate(skill_rank = sum(sum_ranking, na.rm = T)) %>%
          slice(n=1) %>%
          select(skill_rank)
        
        if(nrow(Skills_Rank)==0){
          df[i,j+(mentors_needed+1)] = 0
          
        } else{
          
          df[i,j+(mentors_needed+1)] = as.numeric(Skills_Rank$skill_rank)  
        }
        
      }
      
    }
    
  }
  
  df$sum = rowSums(df[,(mentors_needed+6):((mentors_needed*2)+5)])
  
  df1 = df %>% group_by(combo) %>%
    arrange(desc(sum)) %>%
    mutate(combo_sum = sum(sum)) %>%
    slice(n=mentors_needed) %>%
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
  long = melt(breakout_selection[,c(ncol(breakout_selection),(mentors_needed+6):((mentors_needed*2)+5))], id.vars = "index")
  long$variable = as.character(long$variable)
  
  long = long %>%
    group_by(index) %>%
    arrange(desc(value)) %>%
    mutate(position = seq(1,n(),1)) %>%
    ungroup()
  
  long$variable = ff(long$variable, colnames(breakout_selection[(mentors_needed + 6):(ncol(breakout_selection)-2)]), breakout_selection[1,5:(mentors_needed + 4)], ignore.case = T)
  
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
  colnames(brs_long) = c('breakout_room','mentor','mod','Skill')
  brs_long$mod = as.character(brs_long$mod)
  brs_long = brs_long[,c(4,1,3,2)]
  brs_long$mod = ff(brs_long$mod, unique(brs_long$mod), c("Module1","Module2"), ignore.case = T)
  
  brs_long = brs_long[complete.cases(brs_long)==T,]
  
  brs_long1 = merge(modules_defined, brs_long, by = c("breakout_room","mod","Skill"))
    
  return(brs_long1)
}
