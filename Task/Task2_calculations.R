# ================== FINN Task 2. NPS ====================#

#### 1. Import libraries ####

library(readxl)
library(dplyr)
library(wordcloud)
library(tm)


#### 2. Import datasets ####

nps_raw <- read_excel("Desktop/Case/datasets.xlsx", 
                       sheet = "NPS", 
                       col_types = c("text", "text", "date", "numeric", "text", "numeric"))

subscriptions_raw <- read_excel("Desktop/Case/datasets.xlsx", 
                                            sheet = "Subscriptions")

head(nps_raw)
head(subscriptions_raw)


#### 3. Clean, enrich, and join datasets ####

nps_clean <- nps_raw %>%
              select(-data_point) %>%
              filter(!is.na(ts_data_point))



subscriptions_clean <- unique(subscriptions_raw[ , ] )
subscriptions_clean <- subscriptions_clean %>%
                        filter(!is.na(Name))

              
nps_fin_data <- left_join(nps_clean, subscriptions_clean, by = c("deal_id" = "Name"))

write.csv(nps_fin_data, "Desktop/Case/Task2_NPS/nps_fin_data.csv")
 
                       
#### 4. NPS 2021 #### 

# assumption: each deal is from a different user

# prepare dataset and enrich it with 'last score' variable
working_dat_21 <- nps_fin_data %>%
                filter(ts_data_point >= "2021-01-01 0:00") %>%
                group_by(deal_id) %>%
                mutate(last_score_2021 = ifelse(ts_data_point == max(ts_data_point), 1, 0 ))

working_dat_21 <- working_dat_21 %>%
                filter(last_score_2021 == 1) %>%
                mutate(NPS_cat = case_when(score >= 9 ~ "Promoter", score <= 6 ~ "Detractor", TRUE ~ "Neutral"))

# compute NPS 2021
NPS_2021_perc <- working_dat_21 %>%
              group_by(NPS_cat) %>%
              mutate(percentage = round(n()/nrow(working_dat_21)*100, 2)) %>%
              select(NPS_cat, percentage) %>%
              distinct
                
NPS_2021 <- as.integer(NPS_2021_perc[NPS_2021_perc$NPS_cat == "Promoter",2] - NPS_2021_perc[NPS_2021_perc$NPS_cat == "Detractor",2])
NPS_2021


#### 5. NPS 2020 #### 

# assumption: each deal is from a different user

# prepare dataset and enrich it with 'last score' variable
working_dat_20 <- nps_fin_data %>%
  filter(ts_data_point < "2021-01-01 0:00") %>%
  group_by(deal_id) %>%
  mutate(last_score_2020 = ifelse(ts_data_point == max(ts_data_point), 1, 0 ))

working_dat_20 <- working_dat_20 %>%
  filter(last_score_2020 == 1) %>%
  mutate(NPS_cat = case_when(score >= 9 ~ "Promoter", score <= 6 ~ "Detractor", TRUE ~ "Neutral"))

# compute NPS 2020
NPS_2020_perc <- working_dat_20 %>%
  group_by(NPS_cat) %>%
  mutate(percentage = round(n()/nrow(working_dat_20)*100, 2)) %>%
  select(NPS_cat, percentage) %>%
  distinct

NPS_2020 <- as.integer(NPS_2020_perc[NPS_2020_perc$NPS_cat == "Promoter",2] - NPS_2020_perc[NPS_2020_perc$NPS_cat == "Detractor",2])
NPS_2020


#### 6. B2B vs B2C ####

# 2021

# count customers by type
count_cust <- working_dat_21 %>%
               group_by(customer_type) %>%
                tally()

# compute NPS for types with sufficent amount of scores

count_cust <- count_cust %>%
                filter(n >= 40)

NPS_by_type <- c()

for (i in 1:nrow(count_cust)) {
  x <- working_dat_21 %>%
        filter(customer_type == count_cust[i,1]) %>%
        group_by(NPS_cat) %>%
        mutate(percentage = round(n()/count_cust[i,2]*100, 2)) %>%
        select(NPS_cat, percentage) %>%
        distinct
  
  NPS_by_type <- c(NPS_by_type, as.integer(x[x$NPS_cat == "Promoter",2] - x[x$NPS_cat == "Detractor",2]))
}

NPS_by_cust_type_2021 <- cbind(count_cust$customer_type, NPS_by_type)
NPS_by_cust_type_2021

# 2020

# count customers by type
count_cust <- working_dat_20 %>%
  group_by(customer_type) %>%
  tally()

# compute NPS for types with sufficent amount of scores

count_cust <- count_cust %>%
  filter(n >= 40)

NPS_by_type <- c()

for (i in 1:nrow(count_cust)) {
  x <- working_dat_20 %>%
    filter(customer_type == count_cust[i,1]) %>%
    group_by(NPS_cat) %>%
    mutate(percentage = round(n()/count_cust[i,2]*100, 2)) %>%
    select(NPS_cat, percentage) %>%
    distinct
  
  NPS_by_type <- c(NPS_by_type, as.integer(x[x$NPS_cat == "Promoter",2] - x[x$NPS_cat == "Detractor",2]))
}

NPS_by_cust_type_2020 <- cbind(count_cust$customer_type, NPS_by_type)
NPS_by_cust_type_2020


#### 7. Overtime change within users - Pairwise t-test ####

# prepare data
paired_t_ids <- nps_fin_data %>%
                  count(deal_id) %>%
                  filter(n >= 2)

paired_t_data <- inner_join(nps_fin_data, paired_t_ids, by = "deal_id") %>%
                  filter(!is.na(deal_id))

paired_t_data <- paired_t_data %>%
                  group_by(deal_id) %>%
                  mutate(first_score = case_when(ts_data_point == min(ts_data_point) ~ score), last_score = case_when(ts_data_point == max(ts_data_point) ~ score)) %>%
                  select(deal_id, first_score, last_score, ts_data_point) %>%
                  distinct 

first_score <- paired_t_data$first_score[!is.na(paired_t_data$first_score)]
last_score <- paired_t_data$last_score[!is.na(paired_t_data$last_score)]

paired_t_data_fin <- cbind(first_score, last_score)

write.csv(paired_t_data_fin, "Desktop/Case/Task2_NPS/paired_t_data_fin.csv")

# paired t test
t.test(first_score, last_score, mu=0, alt="two.sided", paired=T, conf.level=0.95)

# data for post-hoc power analysis in g.power
mean(first_score)
sd(first_score)

mean(last_score)
sd(last_score)

cor(first_score, last_score)


###### 7.1 NPS class changes within users ######

NPS_change_within_user <- c()

for (i in 1:nrow(paired_t_data_fin)) {
  if (paired_t_data_fin[i,1] >=9 && paired_t_data_fin[i,2] <= 6) {
    y <- "P to D"
    NPS_change_within_user <- c(NPS_change_within_user, y)
  } else if (paired_t_data_fin[i,1] >=9 && paired_t_data_fin[i,2] >= 7 && paired_t_data_fin[i,2] <= 8) {
    y <- "P to N"
    NPS_change_within_user <- c(NPS_change_within_user, y)
  } else if (paired_t_data_fin[i,1] >=9 && paired_t_data_fin[i,2] >= 9) {
    y <- "P to P"
    NPS_change_within_user <- c(NPS_change_within_user, y)
  } else if (paired_t_data_fin[i,1] >=7 && paired_t_data_fin[i,1] <= 8 && paired_t_data_fin[i,2] <= 6) {
    y <- "N to D"
    NPS_change_within_user <- c(NPS_change_within_user, y)
  } else if (paired_t_data_fin[i,1] >=7 && paired_t_data_fin[i,1] <= 8 && paired_t_data_fin[i,2] >= 7 && paired_t_data_fin[i,2] <= 8) {
    y <- "N to N"
    NPS_change_within_user <- c(NPS_change_within_user, y)
  } else if (paired_t_data_fin[i,1] >=7 && paired_t_data_fin[i,1] <= 8 && paired_t_data_fin[i,2] >= 9) {
    y <- "N to P"
    NPS_change_within_user <- c(NPS_change_within_user, y)
  } else if (paired_t_data_fin[i,1] <= 6 && paired_t_data_fin[i,2] <= 6) {
    y <- "D to D"
    NPS_change_within_user <- c(NPS_change_within_user, y)
  } else if (paired_t_data_fin[i,1] <= 6 && paired_t_data_fin[i,2] >= 7 && paired_t_data_fin[i,2] <= 8) {
    y <- "D to N"
    NPS_change_within_user <- c(NPS_change_within_user, y)
  } else if (paired_t_data_fin[i,1] <= 6 && paired_t_data_fin[i,2] >= 9) {
    y <- "D to P"
    NPS_change_within_user <- c(NPS_change_within_user, y)
  }
} 

NPS_change_within_user_df <- as.data.frame(cbind(paired_t_data_fin, NPS_change_within_user))

count_NPS_changes_within_user <- NPS_change_within_user_df %>%
                                  group_by(NPS_change_within_user) %>%
                                  tally()


#### 8. Time subscription impact on score - Linear regression ####

t_sub_and_score_diff <- paired_t_data %>%
                          group_by(deal_id) %>%
                          mutate(rn = row_number()) %>%
                          arrange(deal_id, ts_data_point)

t_sub_and_score_diff <- t_sub_and_score_diff %>%
                          group_by(deal_id) %>%
                          filter(rn == 1 | rn == max(rn)) %>%
                          mutate(rn = replace(rn, rn != 1, 2))

t_sub_and_score_fin <- t_sub_and_score_diff %>%
                        group_by(deal_id) %>%
                        mutate(time_diff = as.numeric(round(difftime(max(ts_data_point), min(ts_data_point), units = c("days")), 0)),
                               score_diff = last(last_score) - first(first_score)) %>%
                        select(deal_id, time_diff, score_diff) %>%
                        distinct

write.csv(t_sub_and_score_fin, "Desktop/Case/Task2_NPS/t_sub_and_score_fin.csv")

# linear regression
lm_time_score <- lm(score_diff ~ time_diff, t_sub_and_score_fin)
summary(lm_time_score)

# regression and scatter plot
plot(t_sub_and_score_fin$time_diff, t_sub_and_score_fin$score_diff)
abline(lm_time_score)


#### 9. Word Cloud for all free text ####

text_df <- nps_fin_data %>%
            filter(!is.na(comment)) %>%
            select(score, comment)

# create text corpus
docs <- Corpus(VectorSource(text_df$comment))

# clean text
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("german"))

# create document matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
text_df_fin <- data.frame(word = names(words),freq=words)

# generate word cloud
set.seed(1234)

wordcloud(words = text_df_fin$word, 
          freq = text_df_fin$freq, 
          min.freq = 10,           
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))


###### 9.1 Word cloud for Promoters ######
text_df_prom <- nps_fin_data %>%
  filter(!is.na(comment), score >= 9) %>%
  select(score, comment)

# create text corpus
docs <- Corpus(VectorSource(text_df_prom$comment))

# clean text
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("german"))

# create document matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
text_df_fin <- data.frame(word = names(words),freq=words)

# generate word cloud
set.seed(1234)

wordcloud(words = text_df_fin$word, 
          freq = text_df_fin$freq, 
          min.freq = 10,           
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))


###### 9.2 Word cloud for Detractors ######
text_df_detr <- nps_fin_data %>%
  filter(!is.na(comment), score <= 6) %>%
  select(score, comment)

# create text corpus
docs <- Corpus(VectorSource(text_df_detr$comment))

# clean text
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("german"))

# create document matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
text_df_fin <- data.frame(word = names(words),freq=words)

# generate word cloud
set.seed(1234)

wordcloud(words = text_df_fin$word, 
          freq = text_df_fin$freq, 
          min.freq = 10,           
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))


#### 10. Brand impact on NPS - Pearson's correlation ####

# prepare dataset and enrich it with 'last score' variable
working_dat_21_brand <- nps_fin_data %>%
  filter(ts_data_point >= "2021-01-01 0:00") %>%
  group_by(deal_id) %>%
  mutate(last_score_2021 = ifelse(ts_data_point == max(ts_data_point), 1, 0 ))

working_dat_21_brand <- working_dat_21_brand %>%
  filter(last_score_2021 == 1) %>%
  mutate(NPS_cat = case_when(score >= 9 ~ "Promoter", score <= 6 ~ "Detractor", TRUE ~ "Neutral")) %>%
  select(NPS_cat, cars_brand)

# count scores by brand
count_scores <- working_dat_21_brand %>%
  group_by(cars_brand) %>%
  tally()

# compute NPS for types with sufficent amount of scores

count_scores <- count_scores %>%
  filter(n >= 50) %>%
  arrange(desc(n))

NPS_by_brand <- c()

for (i in 1:nrow(count_scores)) {
  x <- working_dat_21_brand %>%
    filter(cars_brand == count_scores[i,1]) %>%
    group_by(NPS_cat) %>%
    mutate(percentage = round(n()/count_scores[i,2]*100, 2)) %>%
    select(NPS_cat, percentage) %>%
    distinct
  
  NPS_by_brand <- c(NPS_by_brand, as.integer(x[x$NPS_cat == "Promoter",2] - x[x$NPS_cat == "Detractor",2]))
}

NPS_by_brand_2021 <- as.data.frame(cbind(count_scores$cars_brand, as.numeric(NPS_by_brand), as.numeric(count_scores$n)))
colnames(NPS_by_brand_2021) <- c("brand", "NPS_2021", "n_deals")
NPS_by_brand_2021$NPS_2021 <- as.numeric(NPS_by_brand_2021$NPS_2021)
NPS_by_brand_2021$n_deals <- as.numeric(NPS_by_brand_2021$n_deals)
NPS_by_brand_2021

write.csv(NPS_by_brand_2021, "Desktop/Case/Task2_NPS/NPS_by_brand_2021.csv")

# Pearson's Correlation
cor.test(NPS_by_brand_2021$NPS_2021, NPS_by_brand_2021$n_deals)


#### 11. NPS by Handover / Return separately. 2020 vs 2021 ####

# Handover/Return 2020
handover_return_2020 <- nps_fin_data %>%
  filter(ts_data_point < "2021-01-01 0:00" & (nps_type == "Handover" | nps_type == "Return" | nps_type == "In-Life")) %>%
  group_by(deal_id, nps_type) %>%
  mutate(last_score_2020 = ifelse(ts_data_point == max(ts_data_point), 1, 0 ))

handover_return_2020 <- handover_return_2020 %>%
  filter(last_score_2020 == 1) %>%
  mutate(NPS_cat = case_when(score >= 9 ~ "Promoter", score <= 6 ~ "Detractor", TRUE ~ "Neutral"))

handover_2020 <- handover_return_2020 %>% filter(nps_type == "Handover")
inlife_2020 <- handover_return_2020 %>% filter(nps_type == "In-Life")
return_2020 <- handover_return_2020 %>% filter(nps_type == "Return")

# compute NPS Handover 2020

NPS_handover_2020_perc <- handover_2020 %>%
  group_by(NPS_cat) %>%
  mutate(percentage = round(n()/nrow(handover_2020)*100, 2)) %>%
  select(NPS_cat, percentage) %>%
  distinct

NPS_handover_2020 <- as.integer(NPS_handover_2020_perc[NPS_handover_2020_perc$NPS_cat == "Promoter",2] - NPS_handover_2020_perc[NPS_handover_2020_perc$NPS_cat == "Detractor",2])
NPS_handover_2020

# compute NPS In-Life 2020

NPS_inlife_2020_perc <- inlife_2020 %>%
  group_by(NPS_cat) %>%
  mutate(percentage = round(n()/nrow(inlife_2020)*100, 2)) %>%
  select(NPS_cat, percentage) %>%
  distinct

NPS_inlife_2020 <- as.integer(NPS_inlife_2020_perc[NPS_inlife_2020_perc$NPS_cat == "Promoter",2] - NPS_inlife_2020_perc[NPS_inlife_2020_perc$NPS_cat == "Detractor",2])
NPS_inlife_2020

# compute NPS Return 2020

NPS_return_2020_perc <- return_2020 %>%
  group_by(NPS_cat) %>%
  mutate(percentage = round(n()/nrow(return_2020)*100, 2)) %>%
  select(NPS_cat, percentage) %>%
  distinct

NPS_return_2020 <- as.integer(NPS_return_2020_perc[NPS_return_2020_perc$NPS_cat == "Promoter",2] - NPS_return_2020_perc[NPS_return_2020_perc$NPS_cat == "Detractor",2])
NPS_return_2020


# Handover/Return 2021
handover_return_2021 <- nps_fin_data %>%
  filter(ts_data_point >= "2021-01-01 0:00" & (nps_type == "Handover" | nps_type == "Return" | nps_type == "In-Life")) %>%
  group_by(deal_id, nps_type) %>%
  mutate(last_score_2021 = ifelse(ts_data_point == max(ts_data_point), 1, 0 ))

handover_return_2021 <- handover_return_2021 %>%
  filter(last_score_2021 == 1) %>%
  mutate(NPS_cat = case_when(score >= 9 ~ "Promoter", score <= 6 ~ "Detractor", TRUE ~ "Neutral"))

handover_2021 <- handover_return_2021 %>% filter(nps_type == "Handover")
inlife_2021 <- handover_return_2021 %>% filter(nps_type == "In-Life")
return_2021 <- handover_return_2021 %>% filter(nps_type == "Return")

# compute NPS Handover 2021

NPS_handover_2021_perc <- handover_2021 %>%
  group_by(NPS_cat) %>%
  mutate(percentage = round(n()/nrow(handover_2021)*100, 2)) %>%
  select(NPS_cat, percentage) %>%
  distinct

NPS_handover_2021 <- as.integer(NPS_handover_2021_perc[NPS_handover_2021_perc$NPS_cat == "Promoter",2] - NPS_handover_2021_perc[NPS_handover_2021_perc$NPS_cat == "Detractor",2])
NPS_handover_2021

# compute NPS In-Life 2021

NPS_inlife_2021_perc <- inlife_2021 %>%
  group_by(NPS_cat) %>%
  mutate(percentage = round(n()/nrow(inlife_2021)*100, 2)) %>%
  select(NPS_cat, percentage) %>%
  distinct

NPS_inlife_2021 <- as.integer(NPS_inlife_2021_perc[NPS_inlife_2021_perc$NPS_cat == "Promoter",2] - NPS_inlife_2021_perc[NPS_inlife_2021_perc$NPS_cat == "Detractor",2])
NPS_inlife_2021

# compute NPS Return 2021

NPS_return_2021_perc <- return_2021 %>%
  group_by(NPS_cat) %>%
  mutate(percentage = round(n()/nrow(return_2021)*100, 2)) %>%
  select(NPS_cat, percentage) %>%
  distinct

NPS_return_2021 <- as.integer(NPS_return_2021_perc[NPS_return_2021_perc$NPS_cat == "Promoter",2] - NPS_return_2021_perc[NPS_return_2021_perc$NPS_cat == "Detractor",2])
NPS_return_2021
