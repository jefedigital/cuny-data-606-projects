
library(tidyverse)
library(openintro)
library(infer)

set.seed(1001001)


us_adults <- tibble(
  climate_change_affects = c(rep("Yes", 62000), rep("No", 38000))
)

ggplot(us_adults, aes(x = climate_change_affects)) + 
  geom_bar() +
  labs(x = "", 
       y = "",
       title = "Do you think climate change is affecting your local community?") + 
  coord_flip()

us_adults %>%
  count(climate_change_affects) %>%
  mutate(p = n /sum(n))

n <- 60
samp <- us_adults %>%
  sample_n(n)

samp %>%
  count(climate_change_affects) %>%
  mutate(p = n /sum(n))


# 1. 62%
# 2. No

samp %>%
  specify(response = climate_change_affects, success = "Yes") %>%
  generate(reps = 1000, type="bootstrap") %>%
  calculate(stat = "prop") %>%
  get_ci(level = 0.95)


tbl <- samp %>%
  specify(response = climate_change_affects, success = "Yes") %>%
  generate(reps = 1000, type="bootstrap") %>%
  calculate(stat = "prop")

# 3. 
# 4. 

samp %>%
  specify(response = climate_change_affects, success = "Yes") %>%
  generate(reps = 1000, type="bootstrap") %>%
  calculate(stat = "prop") %>%
  get_ci(level = 0.99)

