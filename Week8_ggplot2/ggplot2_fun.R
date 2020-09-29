#install packages
install.packages("psych")

install.packages("devtools")
devtools::install_github("an-bui/calecopal")

#load packages
library(tidyverse)
library(psych)
library(calecopal)


#read in your data
penguins <- read.csv("~/Git/Tidy_Tuesdays/Week8_ggplot2/penguins.csv")

#look at what you have
head(penguins)

#from psych package for visualization
pairs.panels(penguins)

#basic scatter plot with geom_point
ggplot(data = penguins) + 
  geom_point(aes(x = flipper_length_mm, y = body_mass_g))

#adjust colors + shapes
names(cal_palettes)
cal_palette("lake")
cal_palette("lake", n=50, "continuous")
cal_palette("lake") [4] #get 4th color in lake palette

#Set color by species
ggplot(data = na.omit(penguins)) + 
  geom_point(aes(x = flipper_length_mm, y= body_mass_g, color = species)) + 
  scale_color_manual(values = cal_palette("lake"))

#Set color by bill length (continuous variable)
ggplot(data = na.omit(penguins)) + 
  geom_point(aes(x = flipper_length_mm, y= body_mass_g, color = bill_length_mm)) + 
  scale_color_gradientn(colors = cal_palette("lake"))

#get fancy with geom_smooth
ggplot(data = na.omit(penguins), aes(x = flipper_length_mm, y= body_mass_g, color = species)) + 
  geom_point() + 
  scale_color_manual(values = cal_palette("lake")) +
  geom_smooth(method = "lm", level = 0.9)

#even fancier: other geom_lines
ggplot(data = na.omit(penguins), aes(x = flipper_length_mm, y= body_mass_g, color = species)) + 
  geom_point() + 
  geom_hline(yintercept = mean(penguins$body_mass_g, na.rm = TRUE)) #add horizontal line
#geom_vline for vertical line
#geom_abline: any line based on slope / intercept

#basic histogram
ggplot(data = penguins) + 
  geom_histogram(aes(body_mass_g), bins = 10)

ggplot(data = penguins) + 
  geom_histogram(aes(body_mass_g), binwidth = 100)

ggplot(data = penguins) + 
  geom_histogram(aes(body_mass_g), breaks = c(2500, 3000, 3500, 4000))

#bins / binwidth/ breaks
#categories
ggplot(data = penguins, aes(fill= species)) + 
  geom_histogram(aes(body_mass_g), bins = 10) +
  facet_wrap(vars(species))

ggplot(data = penguins, aes(fill= species)) + 
  geom_histogram(aes(body_mass_g), bins = 10) +
  facet_wrap(vars(species)) +
  facet_grid(cols = vars(species), rows = vars(sex))

#density plot
ggplot(data = penguins) +
  geom_density(aes(x = body_mass_g))

#Box plots
#basic boxplot with geom_boxplot
ggplot(data = penguins, aes(y = body_mass_g, x = species, fill = species)) +
  geom_boxplot() + 
  scale_fill_manual(values = cal_palette("fire"))

#fancy: geom_label (2 ways)


ggplot(data = penguins, aes(y = body_mass_g, x = species, fill = species)) +
  geom_boxplot() + 
  scale_fill_manual(values = cal_palette("fire")) +
  geom_label(aes(label = species))

df <- penguins %>% 
  group_by(species) %>% 
  summarise(n = n(), med = median(body_mass_g, na.rm = T))

ggplot(data = penguins, aes(y = body_mass_g, x = species, fill = species)) +
  geom_boxplot() + 
  scale_fill_manual(values = cal_palette("fire")) +
  geom_text(data = df, aes(x=species, y = med+120, label = n))

#violin plot
ggplot(data = penguins, aes(y = body_mass_g, x = species, fill = species)) +
  geom_violin() + 
  scale_fill_manual(values = cal_palette("fire")) +
  geom_text(data = df, aes(x=species, y = med+120, label = n))

#Bar / column plots
#Basic bar plots: using geom_col or geom_jitter for categorical data
ggplot(data = penguins, aes(y = body_mass_g, x = species)) +
  geom_col(aes(fill = species))

ggplot(data = penguins, aes(y = body_mass_g, x = species)) +
  geom_jitter(aes(fill = species), width = .1)

#http://r-graph-gallery.com/

#bar plot with error bars
penguins %>% 
  group_by(species) %>% 
  summarize(mass = mean(body_mass_g, na.rm = T),
            sd = sd(body_mass_g, na.rm = T)) %>%
  ggplot(aes(y = mass, x = species, fill = species)) +
  geom_col() +
  geom_errorbar(aes(ymin = mass - sd, ymax = mass + sd), width = 0.1)




