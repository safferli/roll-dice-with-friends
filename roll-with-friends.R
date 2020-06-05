library(tidyverse)

v.file <- "vampi-roll-with-friends.txt"

dta <- tibble(lines = read_lines(v.file)) %>% 
  # every second line is the actual roll result
  mutate(roll = lead(lines)) %>% 
  # keep uneven rows
  slice(which(row_number() %% 2 == 1)) %>% 
  mutate(
    # keep only the results of after "d10"
    roll = str_remove(roll, "Total: \\d+ d10: ")
  ) %>% 
  rename(character = lines) %>% 
  # non-d10 rolls will still exist, let's remove them
  filter(!str_detect(roll, "Total: "))

by_char <- dta %>% 
  # get one row per character and concatenate the rolls
  group_by(character) %>%
  summarize(roll = str_c(roll, collapse = ", ")) %>% 
  ungroup() %>% 
  # tidy data: each observation has its own row; 
  # place each dieroll on a single, separate row
  separate_rows(roll, sep = ", ") %>% 
  # convert to integer for sorting in graph
  mutate(roll = as.integer(roll)) 

  
by_char %>% 
  ggplot()+
    geom_bar(aes(x=roll, fill=character))+
    theme_bw()+
    #theme(plot.title = element_text(lineheight=.6, face="bold"))+
    scale_x_continuous(breaks = 1:10)+
    labs(
      title = "Roll With Friends die roll bias checker", 
      x = "die roll", 
      y = "count"
    )

ggsave("roll-with-friends.png", width = 30, height = 30/((1+sqrt(5))/2), units = "cm")

table(by_char$roll)
# https://rpg.stackexchange.com/questions/70802/how-can-i-test-whether-a-die-is-fair
chisq.test(table(by_char$roll))
