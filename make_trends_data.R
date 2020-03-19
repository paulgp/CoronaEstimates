# if (!require("devtools")) install.packages("devtools")
# devtools::install_github("paulgp/gtrendsR")

#install.packages("RApiDatetime")

library(gtrendsR)
library(tidyverse)
library(RApiDatetime)

pull_data = function(date_string_start, date_string_end) {
  res_post = gtrends(keyword=c("file for unemployment"),  geo = c("US"), 
                     time =str_trim(paste0(c(paste0(c("2020-3-",date_string_start), collapse=""),
                                    paste0("2020-3-", date_string_end), collapse=""), collapse=" ")))
  print(paste0(c(paste0(c("2020-3-",date_string_start), collapse=""),
                 paste0("2020-3-", date_string_end), collapse=""), collapse=" "))
  return(list(state_data = res_post$interest_by_region %>% select(location, hits),
              dma_data = res_post$interest_by_dma %>% select(location, hits)))
}



dates =  str_pad(seq(1,19), width=2, pad="0")
data = list()
for (i in seq(2,19)) {
  data[[i]] = pull_data(dates[i-1], dates[i])
  Sys.sleep(2)
}

data_states = list()
for (i in seq(2,19)) {
  data_states[[i]] = data[[i]]$state_data
}

data_dma = list()
for (i in seq(2,19)) {
  data_dma[[i]] = data[[i]]$dma_data
  data_dma[[i]] = data_dma[[i]] %>% mutate(hits = as.numeric(hits))
}


data_states2 = bind_rows(data_states, .id="column_label") %>% mutate(date = '2012-03-') %>%
  unite(date, date,column_label, sep="") %>%
  mutate(date = ymd(date))

data_states2 %>% write_csv("state_data.csv")
state_labels = data_states2 %>% filter(location %in% c("New York", "Washington", "California", "Ohio")) %>%
  arrange(location, date) %>%
  group_by(location) %>%
  filter(!is.na(hits)) %>%
  filter(row_number() == n())
ggplot() + 
  geom_line(data = data_states2, 
            aes(y = hits, x = date,
                color=as.factor(location)), alpha = 0.2, show=FALSE) +
  geom_line(data = data_states2 %>% filter(location %in% c("Ohio", "California", "Washington", "New York")), 
            aes(y = hits, x = date,
                color=as.factor(location)), show=FALSE) +
  #geom_line(data = simple_mean, aes(y = change, x = date))+
  geom_text_repel(data = state_labels, aes(y = hits, x = date, label = location, color=as.factor(location)), nudge_x = 1.5, show.legend=FALSE, size=5) +
  #geom_text_repel(data = bind_rows(simple_mean %>% filter(row_number() == n())), 
  #                aes(y = change, x = date, label = City), 
  #                nudge_x = 1.5, show.legend=FALSE, size=5) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs(x = "Date",
       y = "",
       title="Indexed google searchs, daily, for 'file for unemployment'",
       subtitle = "Maximum of 100",
       note = "Called via google trends API"
       ) +
  geom_hline(yintercept = 0, linetype=3)


data_dma2 = bind_rows(data_dma, .id="column_label") %>% mutate(date = '2012-03-') %>%
  unite(date, date,column_label, sep="") %>%
  mutate(date = ymd(date)) 

## Remove NAs and count number of obs
data_dma3 = data_dma2 %>% filter(!is.na(hits)) %>%
  group_by(location) %>%
  mutate(num_obs = n()) %>%
  filter(num_obs > 10)

dma_labels = data_dma2 %>% filter(grepl("OH$", location)) %>%
  arrange(location, date) %>%
  group_by(location) %>%
  filter(!is.na(hits)) %>%
  filter(row_number() == n())
dma_labels2 = data_dma2 %>% 
  group_by(location) %>%
  filter(!is.na(hits)) %>%
  filter(row_number() == n()) %>%
  arrange(-hits)
ggplot() + 
  geom_line(data = data_dma2, 
            aes(y = hits, x = date,
                color=as.factor(location)), alpha = 0.2, show=FALSE) +
  geom_line(data = data_dma2 %>% filter(grepl("OH$", location)), 
            aes(y = hits, x = date,
                color=as.factor(location)), show=FALSE) +
  #geom_line(data = simple_mean, aes(y = change, x = date))+
  geom_text_repel(data = dma_labels, aes(y = hits, x = date, label = location, 
                                         color=as.factor(location)), nudge_x = 4, show.legend=FALSE, size=5) +
  #geom_text_repel(data = bind_rows(simple_mean %>% filter(row_number() == n())), 
  #                aes(y = change, x = date, label = City), 
  #                nudge_x = 1.5, show.legend=FALSE, size=5) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs(x = "Date",
       y = "",
       title="Indexed google searchs, daily, for 'file for unemployment'",
       subtitle = "Maximum of 100",
       note = "Called via google trends API"
  ) +
  geom_hline(yintercept = 0, linetype=3)
