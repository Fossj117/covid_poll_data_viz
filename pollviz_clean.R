library(tidyverse)
library(ggthemes)
library(scales)
library(MultinomialCI)
library(ggrepel)

# Read in the data and clean up
setwd("~/Files/research/pollvis/")
df <- read.csv("raw_data_2.csv")
trust_order <- rev(c("A lot ", "Some", "Not Much ", "Not at All ", "Don't Know / No Opinion"))
vote_order <-c("Clinton", "Trump")
vote_names<-c("Clinton Voters (N=710)", "Trump Voters (N=682)")

df %>% filter(Group != 'Cable') %>% 
  #filter(Trust != "Don't Know / No Opinion" ) %>% 
  mutate(Trust = factor(Trust, levels = trust_order), 
         Vote = factor(Vote, levels = vote_order, labels = vote_names)) %>% 
  group_by(Group, Vote) %>%
  mutate(share = Num/sum(Num)) %>% 
  ungroup() -> df

# Compute the confidence intervals
my_ci <- function(x){
  
  ci <- data.frame(multinomialCI(x$Num, alpha = 0.05))
  colnames(ci) <- c("lower", "upper")
  ci$Trust <- x$Trust
  
  return(ci)
  
}

df %>%
  group_by(Group, Vote) %>% 
  group_modify(~my_ci(.x)) %>% ungroup() -> cis 

# Join up 
df %>% 
  inner_join(cis) -> df_final

# Compute the locations of the labels
df_final %>% 
  group_by(Group, Vote) %>% 
  mutate(cs = cumsum(share), 
         l1 = lag(cs), 
         l2 = ifelse(is.na(l1),0,l1),
         val = .5*share+l2) %>%
  select(-cs,-l1,-l2) -> df_final

# Generate the plot
df_final %>%
  ggplot(aes(x=Group, y = share, fill=Trust)) + 
  geom_bar(stat='identity') + facet_grid(.~Vote) + 
  geom_text(aes(label=paste(round(100*share,1), "%", sep=""), y = val), colour = 'black', size = 4.5) +  
  theme_bw(base_size = 15) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(), plot.caption = element_text(size=8)) +
  labs(fill = "Response", caption = 'Raw data: https://bit.ly/2wrsPQd ; tables HR4_8, HR4_4,HR4_10') + 
  ylab("Share of Respondents in Group")+ 
  xlab("Source") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette="Reds") + 
  ggtitle("Responses to: 'To what extent do you trust the following (source) to provide \ncorrect information about the coronavirus outbreak?' by 2016 Vote", 
          subtitle = "Data from Hollywood Reporter / Morning Consult National Tracking Poll (3/12/20-3/15/20)") -> p

ggsave(p, filename = "pollviz.png")
