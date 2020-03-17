library(readxl)
library(tidyverse)
library(ggrepel)

## STATE LEVEL GRAPH 
state_opentable <- read_excel("opentable_data.xlsx", 
                               sheet = "state")

state_opentable = state_opentable %>% 
  rename(State = Name) %>%
  group_by(State, Country) %>% 
  gather(date, change, -State, -Country) %>%
  mutate(date = janitor::excel_numeric_to_date(as.numeric(date))) %>%
  filter(Country == "United States") 

simple_mean = state_opentable %>% group_by(date) %>% summarize(change = mean(change)) %>%
  mutate(State = "Average", Country = "United States")
state_opentable = state_opentable %>% bind_rows(simple_mean)


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
state_labels = state_opentable %>% filter(State %in% c("New York", "Washington", "California", "Average")) %>%
  arrange(State, date) %>%
  filter(row_number() == n())
ggplot() + 
  scale_y_continuous(labels = scales::percent, 
                     limits = c(-.9,0.50)) +
  geom_line(data = state_opentable, 
            aes(y = change, x = date,
            by=as.factor(State)), alpha = 0.2, show=FALSE) +
  geom_line(data = state_opentable %>% filter(State %in% c("New York", "Washington", "California", "Average")), 
            aes(y = change, x = date, color=State), show=FALSE) +
  #geom_line(data = simple_mean, aes(y = change, x = date))+
  geom_text_repel(data = state_labels, aes(y = change, x = date, label = State, color = State), show.legend=FALSE, nudge_x = 1.5, size=5) +
  theme_classic() +
  scale_color_manual(values=c("black", gg_color_hue(3))) +
  theme(text = element_text(size=20)) +
  labs(x = "Date",
           y = "",
           title="Restaurant reservations from Opentable",
           subtitle = "Year-on-year change in diners",
           note = "Includes phone, online, and walk-in diners. All breakouts have 50 or more restaurants in the sample set."
           ) +
  geom_hline(yintercept = 0, linetype=2)


### CITY LEVEL GRAPH
### BROKEN CODE
city_opentable <- read_excel("opentable_data.xlsx", 
                              sheet = "city")
city_opentable = city_opentable %>% 
  rename(City = Name) %>%
  group_by(City,  Country) %>% 
  gather(date, change, -City, -Country) %>%
  mutate(date = janitor::excel_numeric_to_date(as.numeric(date))) %>%
  filter(Country == "United States") 
simple_mean = city_opentable %>% group_by(date) %>% summarize(change = mean(change)) %>%
  mutate(City = "Average")
city_labels = city_opentable %>% filter(City %in% c("Los Angeles", "Seattle", "New York", "Average")) %>%
  filter(row_number() == 1)
ggplot() + 
  scale_y_continuous(labels = scales::percent, 
                     limits = c(-.95,0.75)) +
  geom_line(data = city_opentable, 
            aes(y = change, x = date,
                color=as.factor(City)), alpha = 0.2, show=FALSE) +
  geom_line(data = city_opentable %>% filter(City %in% c("Los Angeles", "Seattle", "New York")), 
            aes(y = change, x = date,
                color=as.factor(City)), show=FALSE) +
  geom_line(data = simple_mean, aes(y = change, x = date))+
  geom_text_repel(data = city_labels, aes(y = change, x = date, label = City, color=as.factor(City)), nudge_x = 1.5, show.legend=FALSE, size=5) +
  geom_text_repel(data = bind_rows(simple_mean %>% filter(row_number() == n())), 
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

