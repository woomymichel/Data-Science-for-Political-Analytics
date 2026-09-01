################################################################################
# Maps and plots creation 
# Group Nº4
# Data Science for Political Analytics
################################################################################
library(tidyverse)
library(paletteer)

setwd('/Users/saurbina/Downloads/')

coef_16 <- read.csv('2016estimates.csv')
coef_20 <- read.csv('2020estimates.csv')

states <- c("AK", "AZ", "AR", "CA", "CO", "CT", "DE", "HI", "IL", "IN", "IA", "KS", "KY", "LA", "MD", "MA", "MI", "MN", "MT", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OR", "PA", "RI", "VT", "VA", "WA", "WV", "WI", "WY", 'OK', 'NE', 'ID', 'MO', 'UT', 'SD', 'ME', 'TX', 'AL', 'GA', 'MS', 'TN', 'FL', 'SC')

### Fixing the data 

coef_16 <- coef_16 %>%
  mutate(states = states)

a <- tolower(coef_16$state)


coef_16 <- coef_16 %>%
  mutate(state = a)

coef_16 <- coef_16 %>%
  mutate(state = ifelse(state == 'arkanas', 'arkansas', state))

coef_16 <- coef_16 %>%
  mutate(state = ifelse(states == 'MA', 'massachusetts', state))

coef_16 <- coef_16 %>%
  mutate(state = ifelse(state == 'tennesse', 'tennessee', state))

coef_16 <- coef_16 %>%
  mutate(state = ifelse(state == 'west virgina', 'west virginia', state))

us_states <- map_data("state") %>% mutate(state=region)

v_st20.df <- left_join(us_states, coef_16, by = "state", relationship = "many-to-many")

#################### 
# Map 2016 creation
#################### 

v_map16 <- ggplot() +
  geom_polygon(data = v_st20.df, aes(x = long, y = lat, group = group, fill = model_state_pref)) +
  geom_polygon(data = v_st20.df, aes(x = long, y = lat, group = group), color = "black", fill = NA) + # Adding state borders
  labs(title = "Support for Government Health Insurance 2016",
       subtitle = 'By state',
       x="",y="") +
  scale_x_continuous(breaks=c()) + 
  theme_void() +
  coord_fixed(ratio = 1.25) +
  scale_fill_gradientn(colours = c("#f3b884", "#84bff3"),breaks = c(0.28, 0.32, 0.36, 0.4, 0.6), name = "Support healthcare, ponint estimates",
                       limits = c(0, 0.6), guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 18, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.2, 0.09)
  ) +
  coord_map()


v_map16

ggsave("support_2016_map.png",v_map16,width=11.2,height=6,units="in")

######################################## Map 2020 ##############################
states <- c("AK", "AZ", "AR", "CA", "CO", "CT", "DE", "HI", "IL", "IN", "IA", "KS", "KY", "LA", "MD", "MA", "MI", "MN", "MT", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OR", "PA", "RI", "VT", "VA", "WA", "WV", "WI", "WY", 'OK', 'NE', 'ID', 'MO', 'UT', 'SD', 'ME', 'TX', 'AL', 'GA', 'MS', 'TN', 'FL', 'SC')

coef_20 <- coef_20 %>%
  mutate(states = states)

a <- tolower(coef_20$state)


coef_20 <- coef_20 %>%
  mutate(state = a)

coef_20 <- coef_20 %>%
  mutate(state = ifelse(state == 'arkanas', 'arkansas', state))

coef_20 <- coef_20 %>%
  mutate(state = ifelse(states == 'MA', 'massachusetts', state))

coef_20 <- coef_20 %>%
  mutate(state = ifelse(state == 'tennesse', 'tennessee', state))

coef_20 <- coef_20 %>%
  mutate(state = ifelse(state == 'west virgina', 'west virginia', state))

us_states <- map_data("state") %>% mutate(state=region)

v_st21.df <- left_join(us_states, coef_20, by = "state", relationship = "many-to-many")

#################### 
# Map 2016 creation
#################### 

v_map20 <- ggplot() +
  geom_polygon(data = v_st21.df, aes(x = long, y = lat, group = group, fill = model_state_pref)) +
  geom_polygon(data = v_st21.df, aes(x = long, y = lat, group = group), color = "black", fill = NA) + # Adding state borders
  labs(title = "Support for Government Health Insurance 2020",
       subtitle = 'By state',
       x="",y="") +
  scale_x_continuous(breaks=c()) + 
  theme_void() +
  coord_fixed(ratio = 1.25) +
  scale_fill_gradientn(colours = c("#f3b884", "#84bff3"),breaks = c(0.28, 0.32, 0.36, 0.4, 0.6), name = "Support healthcare, ponint estimates",
                       limits = c(0, 0.6), guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 18, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.2, 0.09)
  ) +
  coord_map()

v_map20

ggsave("support_2020_map.png",v_map20,width=11.2,height=6,units="in")


#######################
# Scatter plot

#######################


coef_16 <- coef_16 %>%
  mutate(year = 2016)

coef_20  <- coef_20 %>%
  mutate(year = 2020)


states <- c("AK", "AZ", "AR", "CA", "CO", "CT", "DE", "HI", "IL", "IN", "IA", "KS", "KY", "LA", "MD", "MA", "MI", "MN", "MT", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OR", "PA", "RI", "VT", "VA", "WA", "WV", "WI", "WY", 'OK', 'NE', 'ID', 'MO', 'UT', 'SD', 'ME', 'TX', 'AL', 'GA', 'MS', 'TN', 'FL', 'SC')


### Creating Confidence Intervals

for (i in 1:length(states)) {
# Calculate confidence intervals using mean and standard deviation
lower_bound <- coef_16$model_state_pref[i] - 1.96 * coef_16$model_state_sd[i]  # Assuming a 95% confidence interval
upper_bound <- coef_16$model_state_pref[i] + 1.96 * coef_16$model_state_sd[i]

# Store the confidence intervals in the dataframe
coef_16$lower_ci[i] <- lower_bound
coef_16$upper_ci[i] <- upper_bound

}


for (i in 1:length(states)) {
  # Calculate confidence intervals using mean and standard deviation
  lower_bound <- coef_20$model_state_pref[i] - 1.96 * coef_20$model_state_sd[i]  # Assuming a 95% confidence interval
  upper_bound <- coef_20$model_state_pref[i] + 1.96 * coef_20$model_state_sd[i]
  
  # Store the confidence intervals in the dataframe
  coef_20$lower_ci[i] <- lower_bound
  coef_20$upper_ci[i] <- upper_bound
  
}


### Row bind the two dataframes
coef_df <- rbind(coef_16,coef_20)

### Plots 

plot1 <- ggplot(coef_df,aes(factor(states), model_state_pref,color = factor(year))) +
  geom_point(size=2.5) +
  scale_color_manual(values=c("#b6add8","#2ca25f"), name = "Sample", labels = c("2016 sample MRP", "2020 sample MRP")) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.3) + 
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Support for Government Health Insurance",
       x = "States",
       y = "Support",
       color = "Year") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text = element_text(size = 10))

plot1
ggsave("support_graph.png",plot1,width=11.2,height=6,units="in")




