# clear workspace and console
rm(list = ls())
cat("\014")

# # load required packages
# install.packages("RPostgreSQL")
# install.pcakages("tidyverse")
library(tidyverse)
library(RPostgreSQL)
library(lubridate)
library(data.table)


#load("1000Observation-11.RData")
load("1000Observation.RData")
#load("5tablesobservation.RData")

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))
mapped_dfs <- list()

name <- names(dfs)

for (i in 1:length(dfs)) {
    if(name[i] == "recipes"){
        df <- dfs[[i]]
        mapped_dfs[[i]] <- df %>% drop_na(recipe_id, type)
    }
    else if(name[i] == "recipes_rate"){
        df <- dfs[[i]]
        mapped_dfs[[i]] <- df %>% drop_na(user_id, recipe_id, rating, timestamp)
    }
    else if(name[i] == "private_messages"){
        df <- dfs[[i]]
        mapped_dfs[[i]] <- df %>% drop_na(source, target, timestamp)
    }
    else if(name[i] == "unfriends"){
        df <- dfs[[i]]
        mapped_dfs[[i]] <- df %>% drop_na(requester, approver, timestamp)
    }
    else if(name[i] == "friend_accepts"){
        df <- dfs[[i]]
        mapped_dfs[[i]] <- df %>% drop_na(requester, approver, timestamp)
    }
    else if(name[i] == "users"){
        df <- dfs[[i]]
        mapped_dfs[[i]] <- df %>% drop_na(user_id, birthday, cooking_skills)
    }
    else {
        df <- dfs[[i]]
        mapped_dfs[[i]] <- df
    }

}

names(mapped_dfs) <- name
rm(list=setdiff(ls(), "mapped_dfs"))
list2env(mapped_dfs ,.GlobalEnv)

# Birthday's year difference criterion used to calculate homophily
homophily_bd_criterion <- 1

# Birthdays similarity point (used to calculate homophily)
homophily_bd_similarity_point <- 0.5

# Cooking similarity point (used to calculate homophily)
homophily_cooking_similarity_point <- 0.5



# check words for sust or non-sust
# replace or add more key words <<<====
words_sust <- c("sust", "vegetarian", "vegan")
words_non_sust <- c("fish", "meat")

# Get the currennt date as reference. This is used to calculate quarters (timepoints)
today <- Sys.Date()
y1 <- as.numeric(format(today, "%Y"))
m1 <- as.numeric(format(today, "%m"))

# check the recipies whether it has sust or vegetarian
# t is a one-column table each created for every columns of recipes.
# all t tables are concatenated to creates sust and non_sust tables.
# the last column is the sum of previous columns
sust <- NULL
non_sust <- NULL

for (i in 1:ncol(recipes)) {
    t <- grepl(paste0(words_sust, collapse = "|"),
               recipes[, ..i], ignore.case = TRUE) #added 
    t[is.na(t)] <- 0
    sust <- cbind(sust, t)
    t <- grepl(paste0(words_non_sust, collapse = "|"),
               recipes[, ..i], ignore.case = TRUE)
    t[is.na(t)] <- 0
    non_sust <- cbind(non_sust, t)
}
sust <- as.data.frame(sust)
sust$sust <- rowSums(sust)

non_sust <- as.data.frame(non_sust)
non_sust$non_sust <- rowSums(non_sust)

# sustainable versus not sustainable
recipes$sustain <- ifelse(sust$sust > 0, 1, 0)


# recipes_rate 
# create index using quarter to merge
# AD will be 1 if the user has rated the recipe, otherwise it will be 0
# timepoint shows the order in which each user has rated a recipe

#### I Have commented this code which was written by you 

# recipes_rate <- recipes_rate %>%
#     mutate(time_yr = format(as.Date(timestamp), "%Y"), #converted char to date
#            time_mth = format(as.Date(timestamp), "%m"), #converted char to date
#            time_mth = as.numeric(time_mth), 
#            m2 = time_mth,
#            y2 = as.numeric(time_yr),
#            time_q = ifelse(time_mth <= 3, 1, 
#                            ifelse(time_mth > 3 & time_mth <= 6, 2, 
#                     ifelse(time_mth > 6 & time_mth <= 9, 3, 
#                     ifelse(time_mth > 9 & time_mth <= 12, 4, NA)))), 
#            user_id_q = paste(user_id, time_yr, time_q), 
#            AD = ifelse(is.na(rating), 0, 1), 
#            #timepoints = ave(user_id, user_id, FUN = seq_along), 
#            #timepoints = time_q,
#            timepoints = floor(((12 * y1 + m1) - (12 * y2 + m2)) / 3),
#            timepoints = paste0("t-", timepoints)) %>%
#     select(-c(time_yr, time_mth))

recipes_rate <- recipes_rate %>%
    mutate(time_yr = format(as.Date(timestamp), "%Y"), #converted char to date
           time_mth = format(as.Date(timestamp), "%m"), #converted char to date
           time_mth = as.numeric(time_mth), 
           m2 = time_mth,
           y2 = as.numeric(time_yr),
           time_q = ifelse(time_mth <= 3, 1, 
                           ifelse(time_mth > 3 & time_mth <= 6, 2, 
                                  ifelse(time_mth > 6 & time_mth <= 9, 3, 
                                         ifelse(time_mth > 9 & time_mth <= 12, 4, NA)))), 
           user_id_q = paste(user_id, time_yr, time_q), 
           #AD = ifelse(is.na(rating), 0, 1), 
           #timepoints = ave(user_id, user_id, FUN = seq_along), 
           #timepoints = time_q,
           timepoints = floor(((12 * y1 + m1) - (12 * y2 + m2)) / 3),
           timepoints = paste0("t-", timepoints)) %>%
    select(-c(time_yr, time_mth, m2, y2, time_q))

time_dt <- data.frame(Dates = seq.Date(as.Date(min(format(as.Date(recipes_rate$timestamp), '%Y-%m-%28'))), as.Date(max(format(as.Date(recipes_rate$timestamp), '%Y-%m-%28'))),by = "1 month"))

time_dt <- time_dt %>%
    mutate(time_yr = format(Dates, "%Y"), #converted char to date
           time_mth = format(Dates, "%m"), #converted char to date
           time_mth = as.numeric(time_mth), 
           m2 = time_mth,
           y2 = as.numeric(time_yr),
           timepoints_num = floor(((12 * y1 + m1) - (12 * y2 + m2)) / 3),
           timepoints = paste0("t-", timepoints_num))

time_dt <- time_dt %>% group_by(timepoints) %>% distinct(timepoints_num)

recipes_timepoints <- merge(time_dt, recipes, all=TRUE) 

user_recipes_timepoints <- merge(recipes_timepoints[1:2000,], users, all = T) %>% 
    select(c(user_id, recipe_id, timepoints, type, sustain))

res <- merge(user_recipes_timepoints, recipes_rate, by = c("user_id", "recipe_id", "timepoints"), all.x = T) %>%
    mutate(AD = ifelse(is.na(rating), 0, 1)) %>%
    arrange(user_id, recipe_id, timepoints)

res <- res %>% 
    group_by(user_id, recipe_id) %>%
    slice(seq_len(min(which(AD == 1), n()))) %>%
    arrange(user_id, recipe_id, timepoints)



# private_messages
# creating 4 new columns showing year, month, quarter and a mix of user_id and time
private_messages <- private_messages %>%
    mutate(time_yr = format(as.Date(timestamp), "%Y"), 
           time_mth = format(as.Date(timestamp), "%m"), 
           time_mth = as.numeric(time_mth), 
           time_q = ifelse(time_mth <= 3, 1, 
                    ifelse(time_mth > 3 & time_mth <= 6, 2, 
                    ifelse(time_mth > 6 & time_mth <= 9, 3, 
                    ifelse(time_mth > 9 & time_mth <= 12, 4, NA)))),
           timepoints_num = floor(((12 * y1 + m1) - (12 * as.numeric(time_yr) + time_mth)) / 3),
           timepoints = paste0("t-", timepoints_num),
           user_id_q = paste(source, time_yr, time_q)) 


# number of messages sent by each user. 
# "source" is the user_id of the sender.
no_msg <- with(data = private_messages, 
                   aggregate(source, by = list(timepoints,timepoints_num,source), FUN = length))
names(no_msg) <- c("timepoints","timepoints_num","source", "no_msg")

# Find the cummulative messages at each timepoint (i have calcualed this but the end formula hasnt changed)
no_msg <- no_msg %>% group_by(source) %>% mutate(total_msgs = cumsum(no_msg))

# Below code generates data for no of msgs at each timepoint
library(data.table)

dt <- as.data.table(no_msg)
setkey(dt,source,timepoints_num)
test <- dt[setkey(dt[, .(min(timepoints_num):max(time_dt$timepoints_num)), by = source], source, V1)]

test$timepoints <- paste0("t-", test$timepoints_num)

no_msg <- test %>% group_by(source) %>% fill(no_msg, total_msgs)

# number of messages per friend
# tie_strength is number of messages between the user and friend divided by
# the total number of messages sent by the user
no_msg_friends <- with(data = private_messages, 
                       aggregate(source, by = list(timepoints,timepoints_num,source, user_id_q, target), 
                                 FUN = length))
names(no_msg_friends) <- c("timepoints","timepoints_num","source", "user_id_q", "target", "no_msg_friends")


dt <- as.data.table(no_msg_friends)
setkey(dt,source,timepoints_num)
test <- dt[setkey(dt[, .(min(timepoints_num):max(time_dt$timepoints_num)), by = source], source, V1)]

test$timepoints <- paste0("t-", test$timepoints_num)

no_msg_friends <- test %>% group_by(source) %>% fill(user_id_q, target, no_msg_friends)



msg <- merge(no_msg_friends[1:10000,], no_msg[1:10000,], by = c("source", "timepoints", "timepoints_num")) %>%
    #mutate(tie_strength = no_msg_friends / no_msg) %>%
    mutate(tie_strength = no_msg_friends / total_msgs) %>%
    merge(res[, c("user_id_q", "AD")], by = "user_id_q") %>%  
    #merge(res[, c("user_id", "AD")], by.y = "user_id", by.x = "source") %>% #changed the table recipe_rate to res where we calculated AD
    mutate(tie_strength = tie_strength * AD)

if(nrow(msg) == 0){
    tie_strength <- data.frame("user_id" = integer(), "tie_strength" = integer())
}else {
    tie_strength <- with(data = msg, 
                         aggregate(tie_strength, by = list(source), FUN = sum))
    names(tie_strength) <- c("user_id", "tie_strength")
}

    

# redesign unfriends table
# it creates the "sourceTarget" coluimn from (approver, requester) tuple.
# it also creates an "unfriends_flag" = 1, indicating that unfriending happened.
unfriends <- unfriends %>%
    mutate(sourceTarget = paste(approver, requester), 
           unfriends_flag = 1) %>%
    select(sourceTarget, unfriends_flag)


# friends_accepts table
# first it generates a combination from user_id and the quarter at which friend aceept happened.
# then create "sourceTarget" column from "approver" and "requester" pair.
# the unfriend table is then joinned with the resulting friend_acceps table for those who stayed 
# as firends (unfriend didn't happen)

friend_accepts <- transform(friend_accepts, timestamp = as.POSIXct(timestamp, origin = "1970-01-01"))

friend_accepts <- friend_accepts %>%
    mutate(time_yr = format(timestamp, "%Y"),
           time_mth = format(timestamp, "%m"),
           time_mth = as.numeric(time_mth),
           time_q = ifelse(time_mth <= 3, 1,
                    ifelse(time_mth > 3 & time_mth <= 6, 2,
                    ifelse(time_mth > 6 & time_mth <= 9, 3,
                    ifelse(time_mth > 9 & time_mth <= 12, 4, NA)))),
           timepoints_num = floor(((12 * y1 + m1) - (12 * as.numeric(time_yr) + time_mth)) / 3),
           timepoints = paste0("t-", timepoints_num),
           user_id_q = paste(approver, time_yr, time_q), 
           sourceTarget = paste(approver, requester)) %>%
    select(user_id_q, sourceTarget, approver, requester, timepoints, timepoints_num) %>%
    arrange(approver, timepoints_num) %>%
    left_join(unfriends, by = "sourceTarget") %>%
    filter(is.na(unfriends_flag))

# get number of friends (minus unfriends), for those the number of friends > 0
no_fri_accept <- with(data = friend_accepts, 
                      aggregate(approver, by = list(timepoints,timepoints_num,approver),
                                FUN = length))
names(no_fri_accept) <- c("timepoints","timepoints_num","user_id", "no_friends")

# Find the cummulative friends at each timepoint (i have calcualed this but the end formula hasnt changed)
no_fri_accept <- no_fri_accept %>% group_by(user_id) %>% mutate(total_friends = cumsum(no_friends))

# Below code generates data for no of friend accepts at each timepoint
library(data.table)

dt <- as.data.table(no_fri_accept)
setkey(dt,user_id,timepoints_num)
test <- dt[setkey(dt[, .(min(timepoints_num):max(time_dt$timepoints_num)), by = user_id], user_id, V1)]

test$timepoints <- paste0("t-", test$timepoints_num)

no_fri_accept <- test %>% group_by(user_id) %>% fill(no_friends, total_friends)


# get number of friends with AD
adopted_friends <- with(data = recipes_rate, 
                        aggregate(recipe_id, by = list(user_id, recipe_id),
                                  FUN = length))
names(adopted_friends) <- c("user_id", "recipe_id","no_fri_AD")



# friends table: joing no_fri_accept and adoptedFriends => showing number of friends and adopted friends
#AD_by_total = (no_fri_AD / no_friends)
friends <- no_fri_accept %>%
    left_join(adopted_friends, by = "user_id") %>%
    mutate(no_fri_AD = ifelse(is.na(no_fri_AD), 0, no_fri_AD)) %>%
    mutate(AD_by_total = no_fri_AD / total_friends)

# friends <- no_fri_accept %>%
#     left_join(adopted_friends, by = "user_id") 
# 
# test <- res %>%
#     left_join(friends,by = c("user_id", "recipe_id", "timepoints")) %>%
#     mutate(AD_by_total = no_fri_AD / total_friends)
    

# homophily
# first we joing friend_accept with users table (by user_id) to get birthdays, 
# and rename column birthday to birthday_approver
# Note: approver is the friend!
# bd_same is a binary which is 1 if birthdays of the user with their friend is less than a year, otherwise 0.
# if bd_same is 1, it will add homophily_bd_similarity_point to the overall homophily score.
# if user and their friend have the same cooking skill, 
# it will add homophily_cooking_similarity_point to the overall homophily score.
# for each row: homophily = homophily * AD
homophily <- friend_accepts %>% 
    rename(user_id = approver) %>%
    left_join(users, by = "user_id") %>%
    rename(birthday_approver = birthday, 
           cooking_skills_approver = cooking_skills) %>%
    rename(approver = user_id, 
           user_id = requester) %>%
    left_join(users, by = "user_id") %>%
    mutate(
           bd_diff_tmp = as.integer(difftime(birthday_approver, birthday, units = "days")),
           bd_diff = ifelse(bd_diff_tmp == 0, 0, bd_diff_tmp / 365.25), 
           bd_same = ifelse(abs(bd_diff) < homophily_bd_criterion, 1, 0), 
           bd_points = ifelse(bd_same == 1, homophily_bd_similarity_point, 0), 
           cs_points = ifelse(cooking_skills_approver == cooking_skills, homophily_cooking_similarity_point, 0), 
           homophily = bd_points + cs_points) %>%
    select(user_id_q, approver, homophily) %>%
    merge(res[, c("user_id_q", "AD")], by = "user_id_q") %>% #here as well replaced recipe_rate with res table having AD
    mutate(homophily = homophily * AD)

# We them sum up all homophily values for all friends
if(nrow(homophily) > 0){
    homophily <- with(data = homophily, 
                      aggregate(homophily, by = list(approver), FUN = sum))
    names(homophily) <- c("user_id", "homophily") 
}

if(nrow(homophily) > 0 && nrow(tie_strength) > 0) {
    # combine all datasets
    table.final <- res %>%
        arrange(timestamp) %>%
        left_join(homophily, by = "user_id") %>%
        left_join(tie_strength, by = "user_id") %>%
        left_join(friends, by = c("user_id" = "user_id", "recipe_id" = "recipe_id", "timepoints")) %>%
        mutate(tie_strength = ifelse(is.na(tie_strength), 0, tie_strength)) %>%
        mutate(homophily = ifelse(is.na(homophily), 0, homophily)) %>%
        mutate(no_friends = ifelse(is.na(no_friends), 0, no_friends)) %>%
        mutate(no_fri_AD = ifelse(is.na(no_fri_AD), 0, no_fri_AD)) %>%
        mutate(AD_by_total = ifelse(is.na(AD_by_total), 0, AD_by_total)) %>%
        # keep the columns as indicated in the table
        select(timestamp, 
               user_id, 
               timepoints,
               recipe_id,
               type,
               sustain,
               AD, 
               homophily, 
               tie_strength, 
               no_friends, 
               no_fri_AD,
               AD_by_total) %>%
        rename(recipe_type = type,
               Adopted = AD,
               Homophily = homophily,
               Tie_Strength = tie_strength)
    
    
    table.final <- table.final[order(as.integer(table.final$timestamp),decreasing = FALSE), ]
    
    table.final[grep("fish|meat", table.final$recipe_type, ignore.case = FALSE), "sustain"] <- 0
    table.final[grep("vegan|vegetarian", table.final$recipe_type, ignore.case = FALSE), "sustain"] <- 1
    
    # save to CSV
    # write.csv(table.final, file = "FINAL_TABLE2.csv", row.names = FALSE, na = "")  
    write.csv(table.final, file = "FINAL_TABLE2.csv", row.names = FALSE, na = "")  
}else{
    
    table.final <- res %>%
        arrange(timestamp) %>%
        #left_join(homophily, by = "user_id") %>%
        #left_join(tie_strength, by = "user_id") %>%
        left_join(friends, by = c("user_id" = "user_id", "recipe_id" = "recipe_id")) %>%
        mutate(homophily = 0, 
                tie_strength = 0) %>%
        select(timestamp, 
               user_id, 
               timepoints.x,
               recipe_id,
               type,
               sustain,
               AD, 
               homophily, 
               tie_strength, 
               no_friends, 
               no_fri_AD,
               AD_by_total) %>%
        rename(recipe_type = type,
               Adopted = AD,
               Homophily = homophily,
               Tie_Strength = tie_strength)
    
    table.final <- table.final[order(as.integer(table.final$timestamp),decreasing = FALSE), ]
    
    table.final[grep("fish|meat", table.final$recipe_type, ignore.case = FALSE), "sustain"] <- 0
    table.final[grep("vegan|vegetarian", table.final$recipe_type, ignore.case = FALSE), "sustain"] <- 1
    
    #write.csv(table.final, file = "FINAL_TABLE_v2.csv", row.names = FALSE, na = "")
    write.csv(table.final, file = "FINAL_TABLE_v2.csv", row.names = FALSE, na = "")
}



