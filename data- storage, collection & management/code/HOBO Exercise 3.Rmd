---
title: "HOBO Exerciese 3"
author: "Neisse N"
date: "15/01/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### Setting stage

Packages in use

```{r packages, message=FALSE, warning=FALSE}
library(readr)
library(tidyverse)
library(lubridate)
library(rmarkdown)
library(rio)
library(zoo) # rollapply
```

Loading-in data

```{r data upload, message=FALSE, warning=FALSE}

rm(list=ls(all=TRUE))
df <- read_csv("C:/Users/neiss/Desktop/2020FreiburgEnvironmentalSciences/Semester/Semester1/Data- Storage Collection & Managment/data/raw-data/10347371.csv")


dfh_umg <- read_csv('C:/Users/neiss/Desktop/2020FreiburgEnvironmentalSciences/Semester/Semester1/Data- Storage Collection & Managment/data/10347371_TH.csv')

df <- as.tibble(df)
dfh_umg <- as.tibble(dfh_umg)

```

## 1. Temperature indices
#### 1.1 Mean temperatures

```{r means}

dfh_umg %>% 
  summarise(Tavg = round(mean(th), 3))

dfh_umg %>% 
  mutate(hour = hour(date_time)) %>% 
  mutate(dn = if_else(hour >= 6 & hour < 18,
                      'd', 'n',)) %>% 
  group_by(dn) %>% 
  summarise(Tdn = round(mean(th), 3)) %>% 
  ungroup %>% 
  summarise(diff(Tdn))

dfh_umg %>% 
  mutate(hour = hour(date_time)) %>% 
  mutate(dn = if_else(hour >= 6 & hour < 18,
                      'd', 'n',)) %>% 
  group_by(dn) %>% 
  summarise(Tdn = mean(th)) %>% 
  ungroup %>% 
  summarise(Td = round(diff(Tdn),3))

```
#### 1.2 Mean daily amplitude

```{r amplitude}

dfh_umg %>%
  mutate(date = date(date_time)) %>% 
  group_by(date) %>% 
  summarise(Tmin = min(th), Tmax = max(th)) %>% 
  ungroup %>% 
  mutate(Td = Tmax - Tmin) %>% 
  summarise(Tamp = round(mean(Td), 3))

```

#### 1.3 Most rapid temperature change

```{r}

t_range <- function(x) abs(diff(range(x)))

dfh_umg %>%
  mutate(t06 = rollapply(th,
                         width = 6,
                         FUN = t_range,
                         fill = 0,
                         partial = TRUE)) %>% 
  filter(t06 == max(t06))

```

#### 1.4 Fraction of NA-Values

```{r}

dfh_umg$origin <- as.factor(dfh_umg$origin)

dfh_umg %>% 
  group_by(origin) %>% 
  count() %>%
  ungroup %>% 
  summarise(Tfrc = n / sum(n))

```


## 2. Light intensity indices


```{r light indices}

df %>% 
  filter(lux > 0) %>% 
  summarise(Lavg = round(mean(lux),3))

df %>% 
  mutate(hm = format(date_time, format = '%H:%M')) %>% 
  group_by(hm) %>% 
  summarise(Lmean = mean(lux)) %>% 
  filter(Lmean == max(Lmean))

```

## 4. Comparison with a long-term average

```{r long-term average}



```

