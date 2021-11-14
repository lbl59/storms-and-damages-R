# Run this line
if (!require("pacman")) install.packages("pacman")

# Then run lines 5-7
pacman::p_load(
  tidyverse
)

# Load dataset
storms <- read.csv("storms.csv")
damages <- read.csv("damages.csv")

damages <- damages %>% mutate(status = "tropical storm") %>%
  pivot_longer(-status, names_to="name", values_to="damages")

storms <- storms %>% mutate(date=as_datetime(
  paste0(year, "-", month, "-", day, " ", hour, ":00:00")))

ts <- storms %>% filter(stringr::str_detect(status, "tropical storm"))

ts <- ts %>% select(!ts_diameter:hu_diameter)

joined_sd <- inner_join(ts, damages)

# calculate average cost of damage per windspeed
joined_sd <- joined_sd %>% 
  mutate(damage_per_mph = mean(damages)/wind) %>% 
  arrange(desc(damage_per_mph))

# summarize by mean and sd of cost per windspeed
summary_sd <- joined_sd %>% summarize(
  damage_per_mph=mean(damage_per_mph), sd = sd(damage_per_mph))

lm_damages_wind = lm(joined_sd$damages ~ joined_sd$wind)

joined_sd <- joined_sd %>% mutate(
  estimate = lm_damages_wind$coefficients[1]
)

ggplot(joined_sd, aes(year, damages)) + 
  geom_point() + ylim(min(joined_sd$damages), max(joined_sd$damages)) +
  geom_smooth(method = lm) + ggtitle("Damages ($) with respect to time")


