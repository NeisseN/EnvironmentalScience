#### Setting stage ####
rm(list = ls())

### Packages
if(!require('tidyverse')){install.packages('tidyverse')}
if(!require('skimr')){install.packages('skimr')}
if(!require('GGally')){install.packages('GGally')}
if(!require("ggchicklet")){install.packages("ggchicklet", repos = "https://cinc.rud.is")}
if(!require("hrbrthemes")){devtools::install_github("hrbrmstr/hrbrthemes")}
if(!require("VennDiagram")){install.packages("VennDiagram")}
if(!require("tools")){install.packages("tools")}

### Data read-in
df <- read_csv("C:/Users/NeoN/Desktop/2020FreiburgEnvironmentalSciences/Semester/21SoSe/insect-communities-and-dynamics/data/insect-community-and-dyanmics_2021_datasheet_0722_0932.csv", 
               col_types = cols(date = col_date(format = "%d/%m/%y")))

### Data structuring
df_full <- df %>%
  mutate(order = factor(order), 
  trap = factor(trap), 
  hour = factor(hour), 
  pu = factor(pu), 
  trap = factor(trap),
  sub = factor(sub),
  n_pu = factor(n_pu),
  trap_nr = factor(trap_nr),
  activity = factor(activity),
  pan_color = factor(`Colored Pan`)) %>% 
  select(date, activity, trap, pan_color, trap_nr, hour, order, sub, pu, n_pu)

df <- df_full %>% 
  select(date, activity, trap, pan_color, trap_nr, hour, order, pu)

#### Data exploration ####

### Quick overview
df %>% skim() 
# missing values for pan_color (85%) and trap_nr (80%)
## balance
# activity: 580, 326

### Frequencies
## Counts per PU of respective order and trap
count_pu <- df %>%
  group_by(trap, order, pu) %>% 
  summarise(specimen_count = n()) 
count_pu

## PU diversity and abundance per order and respective trap
count_order <- count_pu %>% group_by(trap, order) %>% 
  summarise(pu_count = n(), pu_abundace = sum(specimen_count))
count_order
# Saved in spread sheet

## Order diversity per trap
count_order %>% 
  summarise(div_order = n(), div_pu = sum(pu_count))


## 
pudiv_trap <- df %>% 
  group_by(pu, trap) %>% 
  summarise(n = n()) %>% 
  spread(trap,n)


length(which(!is.na(pudiv_trap$`colored pan`) 
      & !is.na(pudiv_trap$malaise)
      & !is.na(pudiv_trap$sweep)))

length(which(!is.na(pudiv_trap$`colored pan`) 
             & !is.na(pudiv_trap$malaise)))

length(which(!is.na(pudiv_trap$`colored pan`)
             & !is.na(pudiv_trap$sweep)))

length(which(!is.na(pudiv_trap$malaise)
             & !is.na(pudiv_trap$sweep)))


#### Visualizations ####
#_________
## Overall order specific abundance
count(df, order, sort = T) %>%
  mutate(order = fct_reorder(order, n, sum, .desc=FALSE)) %>%
  mutate(order = fct_inorder(order) %>% fct_rev()) %>%
  ggplot(aes(order,n)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "count") +
  theme_classic()

#_________
## PU richness per trap
count_order %>% 
  ggplot(aes(trap, pu_count)) +
  geom_bar(stat = "identity")+
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(legend.position="none") + 
  theme_classic()
  


#_________
## PU richness by numbers of observations per trap and order 
# Calculate the future positions on the x axis of each bar (left border, central position, right border)
count_order$right <- cumsum(count_order$pu_abundace) + 30*c(0:(nrow(count_order)-1))
count_order$left <- count_order$right - count_order$pu_abundace 

#Plot
ggplot(count_order, aes(ymin = 0)) + 
  geom_rect(aes(xmin = left, xmax = right, ymax = pu_count, colour = trap, fill = order)) +
  xlab("number of obs") + 
  ylab("pu_count") +
  theme_classic()


#__________
# Facet wrap
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

# Graph
ggplot(count_order, aes(fill=order, y=pu_count, x=order)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~trap, ncol = 1) +
  coord_flip() +
  theme_bw() +
  theme(legend.position="none") +
  xlab("")

#_________
## PU richness per order and trap
#stacked
 ggplot(count_order, aes(fill=order, color=order, y=pu_count, x=trap)) + 
  geom_bar(position="stack", stat="identity") +
  scale_color_brewer(palette = 'Set3', direction = -1,
                     labels = c(toTitleCase(as.character(levels(count_order$order))))) +
  scale_fill_brewer(palette = 'Set3',
                    labels = c(toTitleCase(as.character(levels(count_order$order))))) +
  ylab("# phylogenetic unit's") +
  xlab("trap")+
  theme_classic()


#_________
## PU diversity Venn diagram
grid.newpage()                                        # Move to new plotting page
draw.triple.venn(area1 = 64,                          # Add name to each set
                 area2 = 109,
                 area3 = 191,
                 n12 = 17,
                 n23 = 12,
                 n13 = 10,
                 n123 = 6,
                 fill = c("orange", "red", "lightblue"),
                 lty = "blank",
                 category = c("Pan trap", "Malaise trap", "Sweep netting"))
