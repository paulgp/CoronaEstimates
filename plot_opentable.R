library(readxl)
library(tidyverse)
library(ggrepel)

## STATE LEVEL GRAPH 
state_opentable <- read_excel("opentable_data.xlsx", 
                               sheet = "state", 
                               col_types = c("text", "text", "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))

state_opentable = state_opentable %>% group_by(State, Country) %>% 
  gather(date, change, -State, -Country) %>%
  mutate(date = janitor::excel_numeric_to_date(as.numeric(date))) %>%
  filter(Country == "United States") 

simple_mean = state_opentable %>% group_by(date) %>% summarize(change = mean(change)) %>%
  mutate(State = "Average")
state_labels = state_opentable %>% filter(State %in% c("New York", "Washington", "California")) %>%
  filter(row_number() == n()) %>%
  bind_rows(simple_mean %>% filter(row_number() == n()))
ggplot() + 
  scale_y_continuous(labels = scales::percent, 
                     limits = c(-.50,0.50)) +
  geom_line(data = state_opentable, 
            aes(y = change, x = date,
            color=as.factor(State)), alpha = 0.2, show=FALSE) +
  geom_line(data = state_opentable %>% filter(State %in% c("New York", "Washington", "California")), 
            aes(y = change, x = date,
            color=as.factor(State)), show=FALSE) +
  geom_line(data = simple_mean, aes(y = change, x = date))+
  geom_text(data = state_labels, aes(y = change, x = date, label = State), nudge_x = 1.5, size=5) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs(x = "Date",
           y = "",
           title="Restaurant reservations from Opentable",
           subtitle = "Year-on-year change in diners",
           note = "Includes phone, online, and walk-in diners. All breakouts have 50 or more restaurants in the sample set."
           ) +
  geom_hline(yintercept = 0, linetype=2)


### CITY LEVEL GRAPH

city_opentable <- read_excel("opentable_data.xlsx", 
                              sheet = "city", 
                              col_types = c("text", "text", "text", "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric"))
city_opentable = city_opentable %>% group_by(City, State, Country) %>% 
  gather(date, change, -City, -State, -Country) %>%
  mutate(date = janitor::excel_numeric_to_date(as.numeric(date))) %>%
  filter(Country == "United States") 
simple_mean = city_opentable %>% group_by(date) %>% summarize(change = mean(change)) %>%
  mutate(City = "Average")
city_labels = city_opentable %>% filter(City %in% c("Los Angeles", "Seattle", "New York", "Average")) %>%
  filter(row_number() == n())
ggplot() + 
  scale_y_continuous(labels = scales::percent, 
                     limits = c(-.75,0.75)) +
  geom_line(data = city_opentable, 
            aes(y = change, x = date,
                color=as.factor(City)), alpha = 0.2, show=FALSE) +
  geom_line(data = city_opentable %>% filter(City %in% c("Los Angeles", "Seattle", "New York")), 
            aes(y = change, x = date,
                color=as.factor(City)), show=FALSE) +
  geom_line(data = simple_mean, aes(y = change, x = date))+
  geom_text(data = city_labels, aes(y = change, x = date, label = City, color=as.factor(City)), nudge_x = 1.5, show.legend=FALSE, size=5) +
  geom_text(data = bind_rows(simple_mean %>% filter(row_number() == n())), 
                             aes(y = change, x = date, label = City), 
                             nudge_x = 1.5, show.legend=FALSE, size=5) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs(x = "Date",
       y = "",
       title="Restaurant reservations from Opentable",
       subtitle = "Year-on-year change in diners",
       note = "Includes phone, online, and walk-in diners. All breakouts have 50 or more restaurants in the sample set."
  ) +
  geom_hline(yintercept = 0, linetype=3)

