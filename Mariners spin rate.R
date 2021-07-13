library(tidyverse)
library(baseballr)
library(lubridate)
library(gt)


pitches<- read.csv("/Users/jimauer/R/Baseball/Ms Spin Rate/savant_data-2.csv")
pitches$game_date<-ymd(pitches$game_date)

#line graph of Kikuchi pitches and spin rate over time
pitches %>% filter(player_name=="Kikuchi, Yusei") %>% 
  group_by(game_date,pitch_type)%>% 
  mutate(release_spin_rate=mean(release_spin_rate)) %>%
  select(player_name,release_spin_rate,game_date,pitch_type) %>% 
  ggplot(aes(y=release_spin_rate,x=game_date,color=pitch_type))+
  geom_line()


#line graph of Marco pitches and spin rate over time
pitches %>% filter(player_name=="Gonzales, Marco") %>% 
  group_by(game_date,pitch_type)%>% 
  mutate(release_spin_rate=mean(release_spin_rate),spinvar=var(release_spin_rate)) %>%
  select(player_name,release_spin_rate,game_date,pitch_type,spinvar) %>% 
  ggplot(aes(y=release_spin_rate,x=game_date,color=pitch_type))+
  geom_line()

#list of player's names
pitches$player_name

#Chart of standard deviation of picth spin rate for Kuchi
pitches %>% filter(player_name=="Kikuchi, Yusei") %>% 
  group_by(game_date,pitch_type)%>%
  summarise(avespin=mean(release_spin_rate),spindev=sd(release_spin_rate),n=n()) %>% 
  filter(n>10) %>% 
  ggplot(aes(y=spindev,x=game_date,color=pitch_type))+
  geom_point()

#Chart of average spin of picth for Kuchi
pitches %>% filter(player_name=="Kikuchi, Yusei") %>% 
  group_by(game_date,pitch_type)%>%
  summarise(avespin=mean(release_spin_rate),spindev=sd(release_spin_rate),n=n()) %>% 
  filter(n>10) %>% 
  ggplot(aes(y=avespin,x=game_date,color=pitch_type))+
  geom_point()

#boxplot of kikuchi spin rate of 4 seamer   
pitches %>% filter(player_name=="Kikuchi, Yusei",pitch_type=="FF") %>% 
  group_by(game_date)%>%
  summarise(release_spin_rate,spindev=sd(release_spin_rate),n=n(),pitch_type) %>% 
  filter(n>10) %>% 
  ggplot(aes(y=release_spin_rate,x=game_date,group=game_date))+
  geom_boxplot()

pitches %>% filter(player_name=="Gonzales, Marco",pitch_type=="CU") %>% 
  group_by(game_date)%>%
  summarise(release_spin_rate,spindev=sd(release_spin_rate),n=n(),pitch_type) %>% 
  filter(n>10) %>% 
  ggplot(aes(y=release_spin_rate,x=game_date,group=game_date))+
  geom_boxplot()

#boxplot of kikuchi spin rate of 4 seamer boxplot with standard deviation  
pitches %>% filter(player_name=="Kikuchi, Yusei",pitch_type=="FF") %>% 
  group_by(game_date)%>%
  summarise(release_spin_rate,spindev=sd(release_spin_rate),n=n(),pitch_type,
            avespin=mean(release_spin_rate)) %>% 
  ggplot(aes(y=release_spin_rate,x=game_date,group=game_date))+
  geom_boxplot(aes(lower=avespin-spindev,upper=avespin+spindev,middle=avespin,
                   ymin=avespin-3*spindev,ymax=avespin+3*spindev),color="#0C2C56",
               fill="#005C5C") + 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                                 size = 0.75, linetype = "solid"),
                 panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                                 colour = "black"), 
                 panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                 colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
               )+
  labs(y="Release Spin Rate",x="Game Date",
       title="Yusei Kikuchi 4-Seam Fastball Spin Rate by Date",
       subtitle="Mean and Standard Deviation of Release Spin Rate by Date",
       caption="Data from https://baseballsavant.mlb.com pulled July 5, 2021") +
  ggsave("/Users/jimauer/R/Baseball/Ms Spin Rate/Kikuchi.png", width = 15, height = 10, units = "cm")


pitches %>% filter(player_name=="Kikuchi, Yusei",pitch_type=="FF") %>% 
  group_by(game_date)%>%
  summarise(avespin=mean(release_spin_rate),spindev=sd(release_spin_rate),n=n()
            ) %>% 
  ungroup %>% 
  gt() %>%
  tab_header(
    title = "Yusei Kikuchi Pitch Spin and Standard Deviation by Date",
    subtitle = "On Four Seam Fastballs"
  ) %>% 
  fmt_number(
    columns = c("avespin","spindev"),
    decimals = 0) %>% 
  cols_label("game_date"="Game Date",
             "avespin"="Average Spin",
             "spindev"="Standard Deviation") %>% 
  gtsave("/Users/jimauer/R/Baseball/Ms Spin Rate/Kuchi_chart.png")
  

#boxplot of Chris Flexen spin rate of 4 seamer boxplot with standard deviation  
pitches %>% filter(player_name=="Flexen, Chris",pitch_type=="FF") %>% 
  group_by(game_date)%>%
  summarise(release_spin_rate,spindev=sd(release_spin_rate),n=n(),pitch_type,
            avespin=mean(release_spin_rate)) %>% 
  ggplot(aes(y=release_spin_rate,x=game_date,group=game_date))+
  geom_boxplot(aes(lower=avespin-spindev,upper=avespin+spindev,middle=avespin,
                   ymin=avespin-3*spindev,ymax=avespin+3*spindev),color="#0C2C56",
               fill="#005C5C") + 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.75, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "black"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )+
  labs(y="Release Spin Rate",x="Game Date",
       title="Chris Flexen 4-Seam Fastball Spin Rate by Date",
       subtitle="Mean and Standard Deviation of Release Spin Rate by Date",
       caption="Data from https://baseballsavant.mlb.com pulled July 5, 2021")+
  ggsave("/Users/jimauer/R/Baseball/Ms Spin Rate/flexen.png", width = 15, height = 10, units = "cm")


#boxplot of Logan Gilbert spin rate of 4 seamer boxplot with standard deviation  
pitches %>% filter(player_name=="Gilbert, Logan",pitch_type=="FF") %>% 
  group_by(game_date)%>%
  summarise(release_spin_rate,spindev=sd(release_spin_rate),n=n(),pitch_type,
            avespin=mean(release_spin_rate)) %>% 
  ggplot(aes(y=release_spin_rate,x=game_date,group=game_date))+
  geom_boxplot(aes(lower=avespin-spindev,upper=avespin+spindev,middle=avespin,
                   ymin=avespin-3*spindev,ymax=avespin+3*spindev),color="#0C2C56",
               fill="#005C5C") + 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.75, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "black"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )+
  labs(y="Release Spin Rate",x="Game Date",
       title="Logan Gilbert 4-Seam Fastball Spin Rate by Date",
       subtitle="Mean and Standard Deviation of Release Spin Rate by Date",
       caption="Data from https://baseballsavant.mlb.com pulled July 5, 2021")+
  ggsave("/Users/jimauer/R/Baseball/Ms Spin Rate/Gilbert.png", width = 15, height = 10, units = "cm")

#boxplot of Hector Santiago spin rate of 4 seamer boxplot with standard deviation  
pitches %>% filter(player_name=="Santiago, Héctor",pitch_type=="FF") %>% 
  group_by(game_date)%>%
  summarise(release_spin_rate,spindev=sd(release_spin_rate),n=n(),pitch_type,
            avespin=mean(release_spin_rate)) %>% 
  ggplot(aes(y=release_spin_rate,x=game_date,group=game_date))+
  geom_boxplot(aes(lower=avespin-spindev,upper=avespin+spindev,middle=avespin,
                   ymin=avespin-3*spindev,ymax=avespin+3*spindev),color="#0C2C56",
               fill="#005C5C") + 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.75, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "black"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  )+
  labs(y="Release Spin Rate",x="Game Date",
       title="Héctor Santiago 4-Seam Fastball Spin Rate by Date",
       subtitle="Mean and Standard Deviation of Release Spin Rate by Date",
       caption="Data from https://baseballsavant.mlb.com pulled July 5, 2021")+
  ggsave("/Users/jimauer/R/Baseball/Ms Spin Rate/Santiago.png", width = 15, height = 10, units = "cm")
               
pitches %>% filter(pitch_type=="FF") %>% group_by(player_name) %>% 
  summarise(n=n()) %>% arrange(desc(n))
