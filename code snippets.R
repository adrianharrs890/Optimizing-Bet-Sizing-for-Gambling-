# How I pull sports data

oct_ben <- c('https://www.cbssports.com/nhl/scoreboard/20221007/',
             'https://www.cbssports.com/nhl/scoreboard/20221008/',
             'https://www.cbssports.com/nhl/scoreboard/20221009/')

oct <- paste0( 'https://www.cbssports.com/nhl/scoreboard/202210',10:31, "/")

nov_ben <- c('https://www.cbssports.com/nhl/scoreboard/20221101/',
             'https://www.cbssports.com/nhl/scoreboard/20221102/',
             'https://www.cbssports.com/nhl/scoreboard/20221103/',
             'https://www.cbssports.com/nhl/scoreboard/20221104/',
             'https://www.cbssports.com/nhl/scoreboard/20221105/',
             'https://www.cbssports.com/nhl/scoreboard/20221106/',
             'https://www.cbssports.com/nhl/scoreboard/20221107/',
             'https://www.cbssports.com/nhl/scoreboard/20221108/',
             'https://www.cbssports.com/nhl/scoreboard/20221109/')

nov <- paste0( 'https://www.cbssports.com/nhl/scoreboard/202211',10:31, "/")

dec_ben <- c('https://www.cbssports.com/nhl/scoreboard/20221201/',
             'https://www.cbssports.com/nhl/scoreboard/20221202/',
             'https://www.cbssports.com/nhl/scoreboard/20221203/',
             'https://www.cbssports.com/nhl/scoreboard/20221204/',
             'https://www.cbssports.com/nhl/scoreboard/20221205/',
             'https://www.cbssports.com/nhl/scoreboard/20221206/',
             'https://www.cbssports.com/nhl/scoreboard/20221207/',
             'https://www.cbssports.com/nhl/scoreboard/20221208/',
             'https://www.cbssports.com/nhl/scoreboard/20221209/')

dec <- paste0( 'https://www.cbssports.com/nhl/scoreboard/202212',10:31, "/")

jan_ben <- c('https://www.cbssports.com/nhl/scoreboard/20230101/',
             'https://www.cbssports.com/nhl/scoreboard/20230102/',
             'https://www.cbssports.com/nhl/scoreboard/20230103/',
             'https://www.cbssports.com/nhl/scoreboard/20230104/',
             'https://www.cbssports.com/nhl/scoreboard/20230105/',
             'https://www.cbssports.com/nhl/scoreboard/20230106/',
             'https://www.cbssports.com/nhl/scoreboard/20230107/',
             'https://www.cbssports.com/nhl/scoreboard/20230108/',
             'https://www.cbssports.com/nhl/scoreboard/20230109/')

jan <- paste0( 'https://www.cbssports.com/nhl/scoreboard/202301',10:12, "/")
dates <- c(oct, nov_ben , nov, dec_ben,dec,
           jan_ben,jan)


date_ind <- substring(dates,44,last=1000000L)

aList <- c()
for(i in 1:length(dates)){
  aList[[i]] <- read_html(dates[i])  %>%
    html_table(fill = TRUE) 
  
}

prac <- aList


for(i in 1:length(prac)){
  for(j in 1:length(prac[[i]])){
    if(length(prac[[i]])  == 0 ){
      next
    } else if ("SO" %in% names(prac[[i]][[j]])){
      print('Its there')
    } else
      prac[[i]][[j]]$SO <- NA
  }
}

for(i in 1:length(prac)){
  for(j in 1:length(prac[[i]])){
    if(length(prac[[i]])  == 0 ){
      next
    } else if ("OT"  %in% names(prac[[i]][[j]])){
      print('Its there')
    } else
      prac[[i]][[j]]$OT <- NA
  }
}

for(i in 1:length(prac)){
  for(j in 1:length(prac[[i]])){
    if(length(prac[[i]])  == 0 ){
      next
    } else
      prac[[i]][[j]] <- prac[[i]][[j]]  %>%
        rename(Team = 1) %>%
        as.data.frame() 
  }
}

clean_data <- vector("list", length(dates))

date_ind <- substring(dates,44,last=1000000L)

for(i in 1:length(clean_data)){
  if(i %in% c(47,48,76,77,78,79,80)){
    next
  }
  clean_data[[i]] <- do.call(rbind,prac[[i]])
  clean_data[[i]]$date_ind  <- date_ind[[i]]
}

clean_data <- clean_data %>% discard(is.null)
remove <- c()
for(i in 1:length(clean_data)){
  if(length(names(clean_data[[i]])) != 8){
    remove[i] <- i
  } 
}
remove <- as.numeric(na.omit(unique(remove)))
df <- do.call(rbind,clean_data[-c(remove)])

df <- df %>%
  mutate(Year = paste0(20,substring(date_ind,1,2)),
         Month = substring(date_ind,3,4),
         Day =  substring(date_ind,5,6)) %>%
  unite(date, c(Year , Month, Day), sep = "-", remove = FALSE) %>%
  mutate(date = ymd(date)) %>%
  select(- date_ind, -Year, -Month, -Day)

df <- df %>%
  group_by(date) %>%
  mutate(n = n()/2,
         vs = rep(1:n , each=2)) %>%
  ungroup() %>%
  select(-n)

df <- df %>%
  mutate(team_name = gsub('[0-9]+', '', Team),
         Record = str_sub(Team, start= -3))%>%
  separate(Record, c('Wins', 'Loss'), sep="-") %>%
  mutate(team_name = gsub('-', '', team_name)) 

# Fix 10-10
df <- df %>%
  filter(date > '2022-10-10')

df <- df %>%
  left_join(
    df %>%
      group_by(date,vs) %>%
      slice(which.max(T)) %>%
      select(Winner = team_name, date,vs)
  )

df <- df %>%
  select(date, team_name, Winner,vs) %>%
  mutate(Team = case_when(
    team_name == "Lightning" ~ "Tampa Bay Lightning",
    team_name == "Rangers" ~ "New York Rangers" ,
    team_name == "Golden Knights" ~ "Vegas Golden Knights",
    team_name == "Kings"  ~ "Los Angeles Kings" ,
    team_name == "Bruins" ~  "Boston Bruins",
    team_name == "Capitals" ~ "Washington Capitals",
    team_name == "Blue Jackets"  ~ "Columbus Blue Jackets",
    team_name == "Hurricanes" ~  "Carolina Hurricanes",
    team_name == "Maple Leafs" ~ "Toronto Maple Leafs",
    team_name == "Canadiens" ~ "Montreal Canadiens",
    team_name == "Blackhawks" ~ "Chicago Blackhawks" ,
    team_name == "Avalanche"~ "Colorado Avalanche",
    team_name == "Kraken" ~ "Seattle Kraken" ,
    team_name == "Ducks" ~ "Anaheim Ducks",
    team_name == "Canucks"  ~ "Vancouver Canucks" ,
    team_name == "Oilers" ~ "Edmonton Oilers",
    team_name == "Coyotes" ~ "Arizona Coyotes",
    team_name == "Penguins" ~ "Pittsburgh Penguins" ,
    team_name == "Devils" ~ "New Jersey Devils",
    team_name == "Flyers"~ "Philadelphia Flyers",
    team_name == "Senators" ~ "Ottawa Senators",
    team_name == "Sabres" ~ "Buffalo Sabres" ,
    team_name == "Panthers" ~ "Florida Panthers",
    team_name == "Islanders" ~  "New York Islanders" ,
    team_name == "Wild" ~  "Minnesota Wild",
    team_name == "Stars" ~ "Dallas Stars",
    team_name == "Predators" ~  "Nashville Predators",
    team_name == "Flames"~ "Calgary Flames"  ,
    team_name == "Red Wings" ~  "Detroit Red Wings",
    team_name == "Jets" ~ "Winnipeg Jets",
    team_name == "Sharks" ~ "San Jose Sharks"  ,
    team_name == "Blues" ~ "St. Louis Blues"
    
    
  ),
  
  Winner = case_when(
    Winner == "Lightning" ~ "Tampa Bay Lightning",
    Winner == "Rangers" ~ "New York Rangers" ,
    Winner == "Golden Knights" ~ "Vegas Golden Knights",
    Winner == "Kings"  ~ "Los Angeles Kings" ,
    Winner == "Bruins" ~  "Boston Bruins",
    Winner == "Capitals" ~ "Washington Capitals",
    Winner == "Blue Jackets"  ~ "Columbus Blue Jackets",
    Winner == "Hurricanes" ~  "Carolina Hurricanes",
    Winner == "Maple Leafs" ~ "Toronto Maple Leafs",
    Winner == "Canadiens" ~ "Montreal Canadiens",
    Winner == "Blackhawks" ~ "Chicago Blackhawks" ,
    Winner == "Avalanche"~ "Colorado Avalanche",
    Winner == "Kraken" ~ "Seattle Kraken" ,
    Winner == "Ducks" ~ "Anaheim Ducks",
    Winner == "Canucks"  ~ "Vancouver Canucks" ,
    Winner == "Oilers" ~ "Edmonton Oilers",
    Winner == "Coyotes" ~ "Arizona Coyotes",
    Winner == "Penguins" ~ "Pittsburgh Penguins" ,
    Winner == "Devils" ~ "New Jersey Devils",
    Winner == "Flyers"~ "Philadelphia Flyers",
    Winner == "Senators" ~ "Ottawa Senators",
    Winner == "Sabres" ~ "Buffalo Sabres" ,
    Winner == "Panthers" ~ "Florida Panthers",
    Winner == "Islanders" ~  "New York Islanders" ,
    Winner == "Wild" ~  "Minnesota Wild",
    Winner == "Stars" ~ "Dallas Stars",
    Winner == "Predators" ~  "Nashville Predators",
    Winner == "Flames"~ "Calgary Flames"  ,
    Winner == "Red Wings" ~  "Detroit Red Wings",
    Winner == "Jets" ~ "Winnipeg Jets",
    Winner == "Sharks" ~ "San Jose Sharks"  ,
    Winner == "Blues" ~ "St. Louis Blues"
    
  )) %>%
  select(-team_name)  %>%
  mutate(
    win_loss = case_when(
      Team   == Winner  ~ 1,
      TRUE ~ 0
    )
  ) 

write.csv(df, "Data/NHL_record_data.csv", row.names=FALSE)







# Reserves happen at the averages 
record_data %>%
  arrange(Team,date) %>%
  group_by(Team, sport) %>%
  mutate(streak= streak_run(win_loss),
         streak = as.numeric(streak)) %>%
  ungroup() %>%
  mutate(streak = case_when(
    win_loss == 1 ~ streak,
    win_loss == 0 ~ streak*(-1)
  )) %>%
  left_join(
    record_data %>%
      arrange(Team,date) %>%
      group_by(Team, sport) %>%
      mutate(streak= streak_run(win_loss),
             streak = as.numeric(streak)) %>%
      ungroup() %>%
      mutate(streak = case_when(
        win_loss == 1 ~ streak,
        win_loss == 0 ~ streak*(-1)
      )) %>%
      filter(win_loss == 1) %>%
      group_by(Team) %>%
      summarise(avg_streak_win = mean(streak)) 
    
  ) %>%
  left_join(
    record_data %>%
      arrange(Team,date) %>%
      group_by(Team, sport) %>%
      mutate(streak= streak_run(win_loss),
             streak = as.numeric(streak)) %>%
      ungroup() %>%
      mutate(streak = case_when(
        win_loss == 1 ~ streak,
        win_loss == 0 ~ streak*(-1)
      )) %>%
      filter(win_loss == 0) %>%
      group_by(Team) %>%
      summarise(avg_streak_loss = mean(streak)) 
  ) %>%
  filter(sport == "NBA") %>%
  mutate(avg_streak_win = round(avg_streak_win),
         avg_streak_loss = round(avg_streak_loss)) %>%
  ggplot(.) +
  aes(date,streak) +
  geom_line() +
  geom_hline(aes(yintercept=avg_streak_win), color="green") +
  geom_hline(aes(yintercept=avg_streak_loss), color="red") +
  facet_wrap(~Team)


rrent_data %>%
  mutate(streak_type = substring(STRK,1,1),
         streak_val = substring(STRK,2,2),
         streak_val = as.numeric(streak_val),
         streak = case_when(
           streak_type == 'L' ~ streak_val*(-1),
           streak_type == 'W' ~ abs(streak_val)
           
           
         )) %>%
  mutate(prob_boost = case_when(
    streak >= 2 ~ 1,
    streak <= -2 ~ 2,
    TRUE ~ 0
  )) #... more code placed here

# Making weights for sampling
streak_data %>%
  rename(prev_streak = streak) %>%
  mutate(date = current_day) %>%
  left_join(
    record_plus %>%
      select(Team,avg_streak_win, avg_streak_loss  ) %>%
      distinct(Team, .keep_all = T)
  ) %>%
  arrange(Team, date) %>%
  group_by(Team) %>%
  mutate(direction_change_loss  = ifelse(prev_streak > avg_streak_win,1,0),
         direction_change_win  = ifelse(prev_streak < avg_streak_loss,1,0),
         next_outcome = case_when(
           direction_change_loss == 1 ~ "Should Lose Game",
           direction_change_win == 1 ~ "Should Win Game"
         ),
         
         class = case_when(
           
           next_outcome ==  "Should Lose Game"  & prev_streak ==  2 ~ 1 ,
           next_outcome ==  "Should Lose Game"  & prev_streak %in% c(3:5) ~ 2,
           next_outcome ==  "Should Lose Game"  & prev_streak > 5 ~ 3,
           next_outcome ==  "Should Win Game"  & prev_streak == -2 ~ 4,
           next_outcome ==  "Should Win Game"  & prev_streak %in% c(-3:-5) ~ 5,
           next_outcome ==  "Should Win Game"  & prev_streak < -5 ~ 6,
           
           
           TRUE ~ 0
           
           
         )) 

# Sampling each data
sampling %>%
  group_by(date) %>%
  mutate(n = n()) %>%
  filter(n  >= games*pairs) %>%
  ungroup() %>%
  select(-n) %>%
  mutate(odds = as.numeric(ML),
         prob = case_when(
           odds < 0 ~ abs(odds)/(abs(odds) + 100),
           odds > 0 ~ 100/(odds + 100)
           
         ), prob = case_when(
           #class == 1~  prob - w[1], #w[4]
           #class == 2 ~ prob - w[1], #w[5]
           #class == 3 ~ prob - w[1], #w[6
           class == 4 ~ prob + w[1], #w[4]
           class == 5 ~ prob + w[1], #w[5]
           class == 6 ~ prob + w[1], #w[6
           
           
           TRUE ~ prob
         ),
         
         deci = case_when(
           odds > 0 ~  (odds/100) + 1,
           odds < 0 ~ (100/abs(odds)) + 1
           
         )) ## more code..

# making every possible combination and then computing expect value of the N length of teamss
num_com <- 3

inter %>%
  group_by(date) %>% 
  do(data.frame(t(combn(.$Team, num_com)))) %>%
  rename(Team_One = X1,
         Team_Two = X2, 
         Team_Three = X3) %>%
  left_join(
    inter %>%
      
      select(date,Team_One = Team, ev_one =  ev, win_loss_one = win_loss)
  ) %>%
  left_join(
    inter %>%
      select(date,Team_Two = Team, ev_two =  ev, win_loss_two = win_loss)
  ) %>%
  left_join(
    inter %>%
      select(date,Team_Three = Team, ev_three =  ev,win_loss_three = win_loss)
  ) %>%
  ungroup() %>%
  mutate(ev = rowSums(select(., contains("ev"))),
         win_percent = rowMeans(select(., contains("win")))) %>%
  rowwise() %>%
  mutate( pair = paste(Team_One,Team_Two,Team_Three, collapse="----")) %>%
  select(date,pair,ev,win_percent) %>%
  ungroup() %>%
  group_by(date) %>%
  arrange(date,desc(ev)) %>%
  slice(4) %>%
  ungroup() %>%
  as.data.frame() %>%
  summarise(ovr = mean(win_percent, na.rm = T))

if(counter == 0){
  find_max[[counter + 1 ]] <- tab
  
} else
  find_max[[counter]] <- tab

if(tab > some_value_){
  find_weight[[counter]] <- as.numeric(c(w[2], tab))
}



# Seeing difference in parlay/non parlay ev distrution while sampling 
tab %>%
  mutate(parlay = case_when(
    win_percent == 1 ~ "Parlay",
    
    TRUE ~ "Miss"
  )) %>%
  ggplot(.) +
  aes(ev, group  = parlay, fill = parlay, color = parlay) + 
  geom_density(alpha=.05) +
  geom_vline(xintercept = some_value, linetype="dotted", 
             color = "blue", size=1.5) +
  geom_vline(xintercept = -some_value, linetype="dotted", 
             color = "red", size=1.5)
