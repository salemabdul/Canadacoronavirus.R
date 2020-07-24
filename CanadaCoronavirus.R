library(scales)
library(foreign)  
library(tidyverse)
library(dplyr) 
library(ggplot2)
library(lubridate)
data1 <- read_csv("C:/Users/SALEM/Downloads/covid19_1704.csv") 
View(data1)
data2 <- read_csv("C:/Users/SALEM/Downloads/Canada_events.csv") 
View(data2)
data3  <- read_csv("C:/Users/SALEM/Downloads/P_code.csv") 
View(data3)

## coronavirus datasets being cleaned up:)
## Keyboard shortcut for left arrow, alt + -

covid191 <- select(data1, prname:numconf, numdeaths:numtotal, numtoday, percentoday)  
View(covid191)
ggplot(data = covid191, mapping = aes(x = prname, y = numconf)) + theme(axis.text.x = element_text(angle = 100)) + geom_boxplot() 
ggplot(data = covid191, mapping = aes(x = prname, y = numdeaths)) + theme(axis.text.x = element_text(angle = 100)) + geom_boxplot() 
data5 <- data1 %>% filter(!is.na(numtested))
ggplot(data = data5, mapping = aes(x = prname, y = numtested)) + theme(axis.text.x = element_text(angle = 100)) + geom_boxplot() 

covid192 <- select(data2, Date:Death)
View(covid192)
data4 <-  covid192 %>% filter(!is.na(Death))
View(data4)
covid193 <- filter(covid191, prname == "Canada") 
View(covid193)
data5 <- mutate(covid192, dateformat = betterdates)
View(data5)
data7 <- select(data5, Confirmed:dateformat)
View(data7)
data8 <- arrange(data7, dateformat, Confirmed, Tested, Death)
View(data8)
##counts of variables
View(covid191 %>% count(numconf))
View(covid191 %>% count(numdeaths))
View(covid192 %>% count(Confirmed))
View(covid192 %>% count(Tested))
View(covid192 %>% count(Death))
dates <- c("12/31/19", "01/07/20", "01/15/20", "01/22/20", "01/25/20", "01/30/20", "02/09/20", "02/20/20", "03/09/20", "03/11/20", "03/12/20", "03/13/20", "03/16/20", "03/17/20", "03/18/20", "03/19/20", "03/20/20", "03/21/20", "03/26/20", "03/27/20", "03/29/20", "03/30/20", "03/31/20", "04/02/20")
betterdates <- as.Date(dates, format = "%m/%d/%y")
betterdates

##visualizations
ggplot(data = data3) + geom_bar(mapping = aes(x = prname, y = Population), stat = "identity") + theme(axis.text.x = element_text(angle = 100))
ggplot(data = data8, aes(x = dateformat, y = Confirmed, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 100)) + scale_x_date(date_breaks = "months", date_labels = "%m/%d/%y")
ggplot(data = data8, aes(x = dateformat, y = Death, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 100)) + scale_x_date(date_breaks = "months", date_labels = "%m/%d/%y")
ggplot(data = data8, aes(x = dateformat, y = Tested, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 100)) + scale_x_date(date_breaks = "months", date_labels = "%m/%d/%y")

##averages
summarise(covid192, mean1 = mean(Confirmed, na.rm = TRUE))
summarise(covid192, mean2 = mean(Tested, na.rm = TRUE))
summarise(covid192, mean3 = mean(Death, na.rm = TRUE)) 
average <- group_by(covid191, prname, prnameFR) 
summarise(average, mean4 = mean(numconf, na.rm = TRUE), mean5 = mean(numdeaths, na.rm = TRUE))
summarise(covid193, mean6 = mean(numconf,na.rm = TRUE))

##Filtering some datasets more and visualizations
View(filter1 <- filter(covid191,prname == "Ontario" )) 
View(filter2 <-  select(filter1, prname:numdeaths))
dates2 <- c("01/31/20", "02/08/20", "02/16/20", "02/21/20", "02/24/20", "02/25/20", "02/26/20", "02/27/20", "02/29/20", "03/01/20", "03/03/20", "03/05/20", "03/06/20", "03/07/20", "03/08/20", "03/09/20", "03/11/20", "03/12/20", "03/13/20", "03/14/20", "03/15/20", "03/16/20", "03/17/20", "03/18/20", "03/19/20", "03/20/20", "03/21/20", "03/22/20", "03/23/20", "03/24/20", "03/25/20", "03/26/20", "03/27/20", "03/28/20", "03/29/20", "03/30/20", "03/31/20", "04/01/20", "04/02/20", "04/03/20", "04/04/20", "04/05/20", "04/06/20", "04/07/20", "04/08/20", "04/09/20", "04/10/20", "04/11/20", "04/12/20", "04/13/20", "04/14/20", "04/15/20", "04/16/20", "04/17/20", "04/18/20")  
View(dates2)
View(betterdates2 <- as.Date(dates2, format = "%m/%d/%y"))
View(filter3  <-  mutate(filter2, dateformat2 = betterdates2))
View(filter4 <- select(filter3, prname, prnameFR, numconf:dateformat2 ))
ggplot(data = filter4, aes(x = dateformat2, y = numconf, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 100)) + scale_x_date(date_breaks = "months", date_labels = "%m/%d/%y")
ggplot(data = filter4, aes(x = dateformat2, y = numdeaths, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 100)) + scale_x_date(date_breaks = "months", date_labels = "%m/%d/%y")




## Quebec and ontario together
View(filter5 <- filter(covid191, prname == "Ontario"| prname == "Quebec")) 
View(filter6 <- select(filter5, prname:numdeaths))
View(dates3 <- c("01/31/20", "02/08/20", "02/16/20", "02/21/20", "02/24/20", "02/25/20", "02/26/20", "02/27/20", "02/29/20", "03/01/20", "03/01/20", "03/03/20", "03/03/20", "03/05/20", "03/05/20", "03/06/20", "03/06/20", "03/07/20", "03/07/20", "03/08/20", "03/08/20", "03/09/20", "03/09/20", "03/11/20", "03/11/20", "03/12/20", "03/12/20", "03/13/20", "03/13/20", "03/14/20", "03/14/20", "03/15/20", "03/15/20", "03/16/20", "03/16/20", "03/17/20", "03/17/20", "03/18/20", "03/18/20", "03/19/20", "03/19/20", "03/20/20", "03/20/20", "03/21/20", "03/21/20", "03/22/20", "03/22/20", "03/23/20", "03/23/20", "03/24/20", "03/24/20", "03/25/20", "03/25/20", "03/26/20", "03/26/20", "03/27/20", "03/27/20", "03/28/20", "03/28/20", "03/29/20", "03/29/20", "03/30/20", "03/30/20", "03/31/20", "03/31/20", "04/01/20", "04/01/20", "04/02/20", "04/02/20", "04/03/20", "04/03/20", "04/04/20", "04/04/20", "04/05/20", "04/05/20", "04/06/20", "04/06/20", "04/07/20", "04/07/20", "04/08/20", "04/08/20", "04/09/20", "04/09/20", "04/10/20", "04/10/20", "04/11/20", "04/11/20", "04/12/20", "04/12/20", "04/13/20", "04/13/20", "04/14/20", "04/14/20", "04/15/20", "04/15/20", "04/16/20", "04/16/20", "04/17/20", "04/17/20", "04/18/20", "04/18/20")) 
View(betterdates3 <- as.Date(dates3, format = "%m/%d/%y"))  
View(filter7 <- mutate(filter6, dateformat3 = betterdates3)) 
View(filter8 <- select(filter7, prname:prnameFR, numconf, numdeaths, dateformat3))
ggplot(filter8, aes(x = dateformat3, y = numconf, colour=prname, group=prname)) + geom_point(size=3.3)+  geom_line(size=1.3) + scale_x_date(date_breaks = "months", date_labels = "%m/%d/%y") + theme(axis.text.x = element_text(angle = 100))
ggplot(filter8, aes(x = dateformat3, y = numdeaths, colour=prname, group=prname)) + geom_point(size=3.3)+  geom_line(size=1.3) + scale_x_date(date_breaks = "months", date_labels = "%m/%d/%y") + theme(axis.text.x = element_text(angle = 100))
 
##Average confirmed cases
confirmedcases <- group_by(filter8, prname)
View(confirmedcases)
View(summarise(confirmedcases, confirmedmean = mean(numconf, na.rm = TRUE)))

