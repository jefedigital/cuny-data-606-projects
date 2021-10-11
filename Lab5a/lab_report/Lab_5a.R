library(tidyverse)
library(openintro)
library(infer)

global_monitor <- tibble(
  scientist_work = c(rep("Benefits", 80000), rep("Doesn't benefit", 20000))
)

ggplot(global_monitor, aes(x = scientist_work)) +
  geom_bar() +
  labs(
    x = "", y = "",
    title = "Do you believe that the work scientists do benefit people like you?"
  ) +
  coord_flip()

global_monitor %>%
  count(scientist_work)



samp1 <- global_monitor %>% sample_n(50)

samp1 %>% 
  count(scientist_work) %>% 
  mutate(p_hat=n/sum(n))

samp2 <- global_monitor %>% sample_n(50)
samp3 <- global_monitor %>% sample_n(100)
samp4 <- global_monitor %>% sample_n(1000)

samp2 %>% 
  count(scientist_work) %>% 
  mutate(p_hat=n/sum(n))

samp3 %>%
  count(scientist_work) %>% 
  mutate(p_hat=n/sum(n))

samp4 %>% 
  count(scientist_work) %>% 
  mutate(p_hat=n/sum(n))

sample_props50 <- global_monitor %>%
  rep_sample_n(size=50, reps=15000, replace=TRUE) %>%
  count(scientist_work) %>%
  mutate(p_hat = n/sum(n)) %>%
  filter(scientist_work == "Doesn't benefit")

ggplot(sample_props50, aes(x=p_hat)) +
  geom_histogram(binwidth=0.02) +
  labs(
    x="p_hat (Doesn't Benefit)",
    title="Sampling Dist",
    subtitle="subtitle"
  )

count(sample_props50)


sample_props50_test  <- global_monitor %>%
  rep_sample_n(size=50, reps=15000, replace=TRUE) %>%
  count(scientist_work) %>%
  mutate(p_hat = n/sum(n))

mean(sample_props50$p_hat)

sample_props_small <- global_monitor %>%
  rep_sample_n(size=20, reps=25, replace=TRUE)

summary(sample_props_small)

head(sample_props_small)

##

sample_15 <- global_monitor %>%
  sample_n(15) %>%
  count(scientist_work) %>%
  mutate(p_hat = n/sum(n))


sample_props_15 <- global_monitor %>%
  rep_sample_n(size=15, reps=15000, replace=TRUE) %>%
  count(scientist_work) %>%
  mutate(p_hat = n/sum(n)) %>%
  filter(scientist_work == 'Benefits')

ggplot(sample_props_15, aes(x=p_hat)) +
  geom_histogram(binwidth=.01) 

mean(sample_props_15$p_hat)

