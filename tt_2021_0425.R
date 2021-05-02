# load libraries
library(tidyverse)
library(tidytuesdayR)
library(dplyr)
library(png)
library(grid)
library(ggimage)
library(magick)

# load data
tuesdata <- tidytuesdayR::tt_load('2021-04-27')
tuesdata <- tidytuesdayR::tt_load(2021, week = 18)

departures <- tuesdata$departures

# find airlines
departures = departures %>% mutate(airline = ifelse(grepl("IRLINE", coname),"Yes", ifelse(grepl("AIR LINES",coname), "Yes", ifelse(grepl("AIRWAY",coname),"Yes","No"))))

depairted <- filter(departures, airline == "Yes")

#give 29 events. A quick bar plot shows no numbers for code 4. Should be able to remove this. Order of dismissal reasons from
#low to high is 6,7,5,3

# code departure reasons. Shows level 4 on its own and random order of totals. reorded to go from low to high.
depairted$newdepcode <- factor(depairted$departure_code, levels = c(6,7,5,3), labels = c("Voluntary - new opportunity","Other","Voluntary - retired", "Dismissed for performance"))

#tidy data to remove na
depairted2 <- select(depairted, newdepcode, coname, fyear) %>% na.omit()


# Trying to do a bar plot with flipped axes, yellowbackground,

#created a new datatable as had messed up depairted2 image referencing.
depairted3 <- depairted2

# imported airline logos as PNG from live sites (this is slow but struggled to sort from local files.)
depairted3 = depairted3 %>% mutate(logo = ifelse(grepl("AMERICAN", coname),"https://cdn.freebiesupply.com/logos/large/2x/aa-american-airlines-logo-png-transparent.png", ifelse(grepl("US AIRWAYS", coname), "https://cdn.freebiesupply.com/logos/large/2x/us-airways-3-logo-png-transparent.png", ifelse(grepl("DELTA", coname), "https://knowtion-inc.com/wp-content/uploads/2017/03/Delta-Air-Lines-Logo.png", ifelse(grepl("SOUTH", coname), "https://dwglogo.com/wp-content/uploads/2017/12/Southwest_Airlines_vector_logo.png", ifelse(grepl("NORTH", coname), "https://upload.wikimedia.org/wikipedia/commons/7/76/Northwest_Airlines_Logo.png", ifelse(grepl("FRONT", coname), "https://s3.amazonaws.com/company-photo.theladders.com/4725/77e083bf-2f43-49a9-8e1c-9cb9f4942f8a.png", ifelse(grepl("JET", coname),"https://clipartsworld.com/images/jetblue-logo-png-2.png", ifelse(grepl("REPUBLIC", coname),"https://marvel-b1-cdn.bc0a.com/f00000000035679/aviation.siu.edu/management/_common/images/partnerships/republic-logo.png","")))))))))

#theplot
p1 <- ggplot(depairted3, aes(x = newdepcode, y = fyear, image = logo)) + geom_point() + geom_image(aes(image = logo)) + xlab("Reason for departure") + ylab("Financial year") + coord_flip() + theme_classic() + theme(plot.background = element_rect(fill = "#ffd000")) + labs(title = "Final Boarding Call",subtitle = "Reasons for departure of CEOs of US S&P 1500 traded Airlines", caption = "Data from Gentry et al (https://doi.org/10.1002/smj.3278), chart by Matt Lee @wannabehawkeye") +  theme(panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF",size = 2, linetype = "solid"))
p1