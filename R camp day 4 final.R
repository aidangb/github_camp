library(tidyverse)
load("~/R stuff/viz_data.RData")
load("~/R stuff/classdata.RData")

# dice roll ---------------------------------------------------------------

#dice roll
for (i in 1:10) {print(sample(1:6, 1, replace = T))} <-dice
if dice[1]=dice[2] no_doubles <-F
if (dice[1]=dice[2) no_doubles <-F
rm.dice
dice.rm
rm.dice
v<- c(4, 9, 16, 26)
map(v, sqrt)
map_dbl(v,sqrt)
library(tidyverse)
map(v, sqrt) [2]
map_dbl(v,sqrt) [2]
map_chr(v,sqrt)
v %>% map_dbl(sqrt) %>% round(1) #=round to the first decimal place
#can also write this as a for loop!
for(i in 1: length(v)) {print(sqrt(v[i]))}
v
rm.dice
rm(dice)
v %>% map_dbl
v %>% map_dbl(doubling)
library(tidyverse)
v %>% map_db1(doubling)
#map functions can be easily manipulated, ex:
v %>% map_dbl(doubling)
v %>% map_dbl(~ .x *2) #.x to return original value
v %>%  map_1g1(~.x ==9)
v %>%  map_1g1(~ .x ==9)
v %>%  map_lgl(~ .x ==9)
#turn into a file...
write_csv(v1, v2, v3)
#turn into a file...
paste("v_dataset"), 1:3, ".csv")
#turn into a file...
paste("v_dataset", 1:3, ".csv")
#turn into a file...
paste("v_dataset", 1:3, ".csv", sep = "")
path "./data/"
files <- dir(path = path, pattern = ".csv")
path <- "./data/"
files <- dir(path = path, pattern = ".csv")
?paste
paste(path, files, sep = "")
files <-paste(path, files, sep = "")
imported <- files %>% map(read_csv)
imported
imported
cd
cd %>% select_if(~is.numeric(.x))
cd %>% select_if(~is.numeric(.x)) %>% #excludes non-numeric values, so gender and hair colour!
  map_dbl(mean, na.rm= TRUE)
?map_dbl
ls
?ls
# sample dataset for missing values exercise
mv <- tribble(
  ~ qtr,
  ~ year,
  ~ name,
  1,
  2016,
  "Agnes",
  2,
  2016,
  "Bert",
  3,
  2016,
  "Choo",
  4,
  2016,
  "Divan",
  1,
  2017,
  "Erik",
  3,
  2017,
  "Gisele",
  1,
  2018,
  "Ines",
  2,
  2018,
  "Jed"
)
# create a simple example dataset for data cleaning
ds <- tribble(
  ~ row,  ~ c1,  ~ c2,  ~ `C 3`,
  "r1",
  "r1_c1",
  "r1c2",
  "r1 c3",
  "r2",
  "r2_c1",
  NA,
  "r2 c3",
  "r3",
  "r3_c1",
  "r3c2",
  "r3 c3"
)
jn <- tribble(
  ~qtr, ~year, ~turnover,
  2,    2018, 45600,
  2,    2016, 50000,
  1,    2016, 34500,
  1,    2018, 25000,
  3,    2018, 12300
)
jn1 <- tribble(
  ~quarter, ~year, ~turnover,
  2,    2018, 45600,
  2,    2016, 50000,
  1,    2016, 34500,
  1,    2018, 25000,
  3,    2018, 12300
)
mv1 <- tribble(
  ~qtr, ~year, ~name,
  1,   2015, "Agnes",
  2,   2016, "Bert",
  3,   2016, "Choo",
  4,   2016, "Divan",
  2,   2017, "Erik",
  3,   2017, "Gisele",
  1,   2018, "Xi",
  2,   2018, "Jed"
)
mvt <- tribble(
  ~qtr, ~year, ~turnover,
  1,   2016, 5.5,
  2,   2016, 1.2,
  3,   2016, 0.6,
  4,   2016, 8.9,
  1,   2017, 0.2,
  3,   2017, 10.3,
  1,   2018, 2.4,
  2,   2018, 5.0
)
# use of separate()
ds.separated <- separate(
  data = ds,
  col = c1,
  into = c("my.key1", "my.key2"),
  sep = "_"
)
cd
cd %>% count(hair.color)
cd %>% count(gender)
cd %>% mutate_all
cd %>% mutate_all(tolower)
cd %>% count(gender)
cd %>% mutate_all(tolower) %>%  count(hair.color)
cd %>% mutate_all(tolower) %>%  count(hair.color)  %>% count(gender)
cd %>% mutate_all(tolower) %>%  count(gender)
cd<- cd %>% mutate_all(tolower)
cd %>%mutate(hair.color = replace(hair.color, "blonde", "blond"))
cd %>%mutate(hair.color = str_replace(hair.color, "blonde", "blond"))
cd %>% count(hair.color)
cd %>%mutate(hair.color = str_replace(hair.color, "blonde", "blond")) %>% count(hair.color)
cd<- cd %>%mutate(hair.color = str_replace(hair.color, "blonde", "blond"))
))
)
))
#now to condense the categories
cd %>% mutate(hair.color=case_when(hair.color %in% c("blonde, "blond" ~ "blond",
#now to condense the categories
cd %>% mutate(hair.color=case_when(hair.color %in% c(hair.color "blonde, "blond" ~ "blond",
))
))
hair.color == "black" ~ "black",
TRUE ~ other,
na.omit(cd)
cd
cd %>% complete.cases
cd %>% complete.cases %>%  sum()
na.omit(cd) #67 rows
cd %>% complete.cases # can use this in our vactors...recall that true = 1 and false = 0
cd %>% complete.cases %>%  sum()
length(cd)
cd %>% nrow
nrow(cd) - sum(complete.cases(cd))
#now, which cases have missing values?
is.na(cd)
colSums(is.na(cd))
drop_na(cd, hair.length)
#can do this if you don't need the hair.length value in your calcs
#can drop everything at once (refer back to colsums results to see where dropping is needed):
drop_na(cd, hair.length, gender, shoe.size)
median(cd, height)
median(cd, height, na.rm=TRUE)
cd
median(cd, height, na.rm=TRUE)
median(cd$height, na.rm=TRUE)
mv2 <- spread(data=mv, key=year, value = name)
mv2
mvmv2
mv
mv2
#resolve this using the "fill" function
fill(mv2, `2017` : `2018`)
mv
complete(mv, year, qtr)
complete(mv, year, qtr, fill = "unknown") #looks for blanks and fills them in logically
complete(mv, year, qtr, fill = list(name="unknown"))
mv %>% complete(year, qtr) %>% mutate(name=replace_na(name, "unknown"))
mvt
mvt %>%
  complete(year, qtr) %>%
  mutate(turnover=replace_na(turnover, mean(turnover)))#replace with the mean of the turnover
mvt %>%
  complete(year, qtr) %>%
  mutate(turnover=replace_na(turnover, mean(turnover, na.rm = T)))#replace with the mean of the turnover
cd %>% filter(complete.cases(.))
cd %>% filter(!complete.cases(.))
## EXERCISE 1
data(Amelia)
load("~/R stuff/classdata.RData")
## EXERCISE 1
install.packages(Amelia)
## EXERCISE 1
install.packages("Amelia"")
library(Amelia)
## EXERCISE 1
install.packages("Amelia)
## EXERCISE 1
install.packages("Amelia")
library(Amelia)
data("freetrade")
freetrade
#1. proportion of cases with missign values
nrow(freetrade) - sum(complete.cases(freetrade))
na.omit(freetrade)
#1. proportion of cases with missing values
#first need number of overall cases
nrow(freetrade)
#express as a proportion...
(nrow(freetrade) - sum(complete.cases(freetrade)))/(nrow(freetrade))
#2. how many missing values does each variable have
freetrade %>%  is.na() %>% colSums()
#3. replace missing values in "tariff" with the average value
freetrade %>%  mutate(tariff = replace_na(tariff, na.rm = T))
#3. replace missing values in "tariff" with the average value
freetrade %>%  mutate(tariff = replace_na(mean(tariff), na.rm = T))
#3. replace missing values in "tariff" with the average value
freetrade %>%  mutate(tariff = replace_na(tariff, mean(tariff), na.rm = T))
summary(freetrade)
library(ggplot2)
install.packages(pacman)
install.packages(theskimr)
install.packages(broom)
install.packages(broom)
install.packages("broom")
install.packages("broom")
install.packages("theskimr")
library(tidyverse)
?fivenum
fivenum(cs$height, na.rm = T)
fivenum(cd$height, na.rm = T)
install.packages("Rtools")
cd %>%mutate_at(vars(height:hair.length, as.numeric))
range(cdheight, na.rm= T)
range(cd$height, na.rm= T)
diff(range(cd$height, na.rm= T))
sd(cd$height, na.rm = T)
hist(cd$height)
gplot(cd$height)
qplot(cd$height)
#can specify the geometry too
qplot(cd(height, geom = "boxplot"))
#can specify the geometry too
qplot(cd(height, geom = "boxplot"))
#can specify the geometry too
cd
qplot(cd(height, geom = "boxplot"))
qplot(1, cd(height, geom = "boxplot"))
qplot(1, cd$height, geom = c("boxplot", "jitter"))
qplot(cd$gender, cd$height, geom = c("boxplot", "jitter"))
skimr
?skimr
install.packages("theskimr")
library(skimr)
install.packages("installr")
updateR()
install.packages("installr"); require(installr)} #load / install+load installr
install.packages("installr"); require(installr)} #load / install+load installr
# using the package:
updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.
# using the package:
updateR(.) # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.
cor.test(cd$height, cd$shoe.size)
lm(shoe.size ~ height)#for doing regressions
lm(shoe.size ~ height, data = cd)#for doing regressions
summary(mod)
mod <- lm(shoe.size-height, data=cd)
summary(mod)
mod <- lm(shoe.size-height, data=cd)
mod <- lm(shoe.size ~ height, data=cd)
summary(mod)
cd %>% filter(complete.cases(.)) %>%
  group_by(gender) %>%
  group_map(~ lm(shoe.size ~ height, data = .x)) %>%
  map(summary)
cd %>% filter(complete.cases(.)) %>%
  group_by(gender) %>%
  group_map(~ lm(shoe.size ~ height, data = .x)) %>%
  map(summary) %>%
  map_dbl("r,squared")
cd %>% filter(complete.cases(.)) %>%
  group_by(gender) %>%
  group_map(~ lm(shoe.size ~ height, data = .x)) %>%
  map(summary) %>%
  map_dbl("r.squared")
map_dbl("height")
#and now onto graphs
cd %>% count(gender)
View(hdi)
View(hdi)
ls
ls(a)
ls(a)
#working with data
glimpse(hdi)
ls()
ggplot(data = hdi)
str(cd)
str(g)
g <- ggplot(data = hdi)
str(g)
g + geom_histogram(mapping = aes(x = Lil))
g + geom_histogram(mapping = aes(x = Li))
g + geom_histogram(mapping = aes(x = LifeExp))
g + geom_histogram(bins = 5) #means restrict to 5 columns, bins controls number of columns
g + geom_histogram(mapping = aes(x = LifeExp)) #x= to specify the thing you want to map out
g + geom_histogram(bins = 5) #means restrict to 5 columns, bins controls number of columns
g + geom_histogram(mapping = aes(x = LifeExp)) #x= to specify the thing you want to map out
g + geom_histogram(bins = 5) #means restrict to 5 columns, bins controls number of columns
g + geom_histogram() + geom_freqpoly()
g + geom_histogram(mapping = aes(x = LifeExp)) %>%    #x= to specify the thing you want to map out
  g + geom_histogram(bins = 5) #means restrict to 5 columns, bins controls number of columns
g + geom_histogram(mapping = aes(x = LifeExp))    #x= to specify the thing you want to map out
g + geom_histogram(bins = 5) #means restrict to 5 columns, bins controls number of columns
g + geom_histogram() + geom_freqpoly()
#working with data
glimpse(hdi),
ls(),
g <- ggplot(data = hdi)#we're getting a grey box, but this is setting the stage for our graph!
g+ geom_point
g <- ggplot(data = hdi)#we're getting a grey box, but this is setting the stage for our graph!
g+ geom_point
g + geom_histogram() + geom_freqpoly()
g + geom_histogram(bins = 5) #means restrict to 5 columns, bins controls number of columns
g+ geom_point() + geom_smooth(se=F) + geom_smooth(method = "lm")
g + geom_histogram(mapping = aes(x = LifeExp))    #x= to specify the thing you want to map out
g + geom_histogram(bins = 5) #means restrict to 5 columns, bins controls number of columns
g <- ggplot(data = hdi, aes(x = lifeExp))
g + geom_histogram() + geom_freqpoly()
g <- ggplot(data = hdi, aes(x = LifeExp))
g+ geom_point() + geom_smooth(se=F) + geom_smooth(method = "lm")
g + geom_histogram() + geom_freqpoly()
g <- ggplot(data = hdi, aes(x = LifeExp, y = GNIpCap))
g+ geom_point() + geom_smooth(se=F) + geom_smooth(method = "lm")
g + geom_point(mapping = aes(color = region)) + scale_y_log10(labels = scales::dollar)
g + geom_point(mapping = aes(color = "region")) + scale_y_log10(labels = scales::dollar)
hdi
g + geom_point(mapping = aes(color = region)) + scale_y_log10(labels = scales::dollar)
g + geom_point(mapping = aes(color = Region)) + scale_y_log10(labels = scales::dollar)
g + geom_point(mapping = aes(color = Region)) + scale_y_log10(labels = scales::dollar)
display.brewer.pal
g + geom_point(mapping = aes(color = Region)) + scale_y_log10(labels = scales::dollar) + scale_color_brewer(palette = yor)
g + geom_point(mapping = aes(color = Region)) + scale_y_log10(labels = scales::dollar) + scale_color_brewer(palette = "YlOrRd")
g + geom_point(mapping = aes(color = Region)) + scale_y_log10(labels = scales::dollar) + scale_color_brewer(palette = "YlOrRd") + labs(y= "Income per caital in PPP$", x = "Life expectancy at birth", title = "dependency of health on wealth")
g + geom_point(mapping = aes(color = Region)) + scale_y_log10(labels = scales::dollar) + scale_color_brewer(palette = "YlOrRd") + labs(y= "Income per caital in PPP$", x = "Life expectancy at birth", title = "Dependency of Health on Wealth")
ggsave()
1+1
ggsave("myveryfirstgraph")
ggsave("myveryfirstgraph.")
ggsave("myveryfirstgraph.pdf")
ggsave("myveryfirstgraph.pdf")
getwd()
#LAB
#1.
sample(x=1:6, replace = TRUE)
#LAB
#1.
2(sample(x=1:6, replace = TRUE)
  #LAB
  #1.
  sample(x=1:6, replace = TRUE)
  #LAB
  #1.
  sample(x=1:6, replace = TRUE)
  #LAB
  #1.
  sample(x=1:6, size = 1, replace = TRUE)
  #LAB
  #1.
  sample(x=1:6, size = 1, replace = TRUE)
  while
  #LAB
  #1.
  sample(x=1:6, size = 1, replace = TRUE)
  #LAB
  #1.
  sample(x=1:6, size = 1, replace = TRUE)
  #LAB
  #1.
  sample(x=[1:6], size = 1, replace = TRUE)
  #LAB
  #1.
  sample(x=1:6, size = 1, replace = TRUE)
  #LAB
  #1.
  sample(x=1:6, size = 1, replace = TRUE)
  #LAB
  #1.
  sample(x=1:6, size = 1, replace = TRUE)
  ?doubles
  ??doubles
  ?while
  ?while
  ?while
  while?
    while?
    die2 <- (sample(x=1:6, size = 1, replace = TRUE))
  #LAB
  #1.
  die1 <- (sample(x=1:6, size = 1, replace = TRUE))
  die1, die2
  die1 die2
  die1 die2
  die1
  die2
  #LAB
  #1.
  no_doubles=TRUE
  #LAB
  #1.
  no_doubles==TRUE
  c(die1, die2)
  while(no_doubles == TRUE) {die1, die2}
  while(no_doubles == TRUE) {c(die1, die2)}
  while(no_doubles == TRUE) {c(die1, die2)}
  #LAB
  #1.
  no_doubles==TRUE
  #LAB
  #1.
  no_doubles==TRUE
  die1 <- (sample(x=1:6, size = 1, replace = TRUE))
  die2 <- (sample(x=1:6, size = 1, replace = TRUE))
  while(no_doubles == TRUE) {c(die1, die2)}
  while(doubles == FALSE) {c(die1, die2)}
  c(die1, die 2) %>%  while(doubles == FALSE) {c(die1, die2)}
  c(die1, die 2) %>%  while(doubles == FALSE) {c(die1, die2)}
  c(die1, die2)
  c(die1, die2)
  die1 <- (sample(x=1:6, size = 1, replace = TRUE))
  die2 <- (sample(x=1:6, size = 1, replace = TRUE))
  die1
  c(die1, die2)
  die1 <- (sample(x=1:6, size = 1, replace = TRUE)
           g<-6
           g
           g
           g <- 6
           g
           j <- 6
           j
           1+1
           1+1
           1+1
           1+1
           1+1
           1+1
           i
           j
           j <- 6
           x <- 0
           #LAB
           #1.
           no_doubles==TRUE
           x <- 0
           while(no_doubles) {
             die1 <- (sample(1:6, size = 1))
             die2 <- (sample(1:6, size = 1),
             )
             if (die1 == die2) no_doubles = FALSE
             x <- x + 1
           }
           #LAB
           #1.
           no_doubles==TRUE
           x <- 0
           while(no_doubles) {
             #LAB
             #1.
             no_doubles <- TRUE
             x <- 0
             while(no_doubles) {
               while(no_doubles) {
                 die1 <- (sample(1:6, size = 1)
                          die2 <- (sample(1:6, size = 1)
                                   if (die1 == die2) no_doubles = FALSE
                                   x <- x + 1
               }
               while(no_doubles)
                 die1 <- (sample(1:6, size = 1)
                          die2 <- (sample(1:6, size = 1)
                                   if (die1 == die2) no_doubles = FALSE
                                   x <- x + 1

                                   

                                                                                                         

                                   
                                   

                                   
                                
#day 4 stuff
#back to graphing, making nice looking graphs  
1+1
library(tidyverse)
g <- ggplot(data = hdi)
g <- ggplot(data=hdi, mapping=aes(x=LifeExp, y=GNIpCap))
g + geom_point(color="blue")  #specify colour on the inside of brackets --> colour is an argument of the function       
g + geom_point(color="blue", shape = 1) #can play around with shapes lol 
#vary by shape instead of colour 
g <- ggplot(data=hdi, mapping = aes(x=LifeExp, y=GNIpCap, shape = Region))
g + geom_point(color="red")
#select custom shapes for each variable 
g + geom_point(color="red") + scale_shape_manual(values= c(1,3,4,5,6:8))
#recall: "aes =" to specify aethetics, and data details should be specified outside this

g <- ggplot(hdi, aes(x=LifeExp, y = GNIpCap))
g + geom_point(aes(color = Region)) + geom_smooth(color = "black") #apply a second function where we don't want anything to vary
#now let's combine this...
g + geom_point(aes(color = Region, alpha = .3)) + (aes(color = Status, alpha = .3)) #overlay of the 2...shown by darker-coloured sections 
#alpha value = transparency value

hdi$MnYrsSchool

g + geom_point(aes(color=HDI)) #now intensity of color shows HDI variations 
g + geom_point(aes(size=HDI, color = MnYrsSchool), alpha = 0.5) # now HDI is shown by differrntly-sized dots and mean years of school by variations in colour intensity-- show lots of data all at once!
g + geom_point(aes(size=HDI, color = MnYrsSchool), alpha = 0.5) + scale_color_gradient(low = "slategrey", high = "royalblue2")
g <- ggplot(data = hdi, mapping = aes(x=Year, y=LifeExp))
g + geom_line() #not a great graph...needs to group lines by country to be more readable and useful!
g + geom_line(aes(group = Country)) + facet_wrap("Region")
?facet_grid
#facet_wrap--> produces facets for each unique value of Region! so we end up getting a nice graph of life expectency changes per region 
g + geom_line(aes(group = Country)) + facet_grid(Region ~.) #difference in how facet_wrap and grid are expressed is about the coding instructions of each...one needs brackets and one doesn't 
g + geom_line(aes(group = Country)) + facet_grid(Status ~ Region)
#filtering out particular regions...
#change the dataset first 
g <- ggplot(data = hdi %>% filter(Region != "North America"), mapping = aes(x=Year, y=LifeExp))
#then repeat the operations
g + geom_line(aes(group = Country)) + facet_grid(Status ~ Region)

g + geom_line(aes(group = Country)) + facet_wrap("Region") + scale_x_discrete(breaks= seq(1990, 2015, 10)) #display breaks every 10 years
g + geom_line(aes(group = Country)) + facet_wrap("Region") + scale_x_discrete(breaks= seq(1990, 2015, 5)) +
  theme(axis.text.x=element_text(angle=285)) #adjusting text position for readability 
g + geom_line(aes(group = Country)) + facet_wrap(Region ~ Status) + scale_x_discrete(breaks= seq(1990, 2015, 5)) +
  theme(axis.text.x=element_text(angle=285)) +
  labs (x="Year", y = "Life Expectancy in years", title="Life Expectancy at Birth by World Region")
#add themes!
g + geom_line(aes(group = Country)) + facet_wrap(Region ~ Status) + scale_x_discrete(breaks= seq(1990, 2015, 5)) +
  theme(axis.text.x=element_text(angle=285)) +
  labs (x="Year", y = "Life Expectancy in years", title="Life Expectancy at Birth by World Region") +
  theme_minimal()
install.packages("ggthemes")
library(ggthemes)
g + geom_line(aes(group = Country)) 
g + geom_point(aes(color= Region)) + geom_smooth() + scale_color_stata()+ theme_stata()


g <- 

# Stats with ggplot -------------------------------------------------------
g <- ggplot(data=hdi, mapping=aes(x=Region))
g + geom_bar() #gives absolute numbers 
#\n linebreak in text 
#convert to relative numbers 

g+ geom_bar(aes(y)) #there is no y, y is just the count of region...so must do y = 
g+ geom_bar(aes(y = ..prop..)) #but now every bar is showing 1.00
g+ geom_bar(aes(y = ..prop.., group =1)) #instead of having each variable be relative to itself, group = 1 to make it relative to the whole dataset! 
g <- ggplot(hdi %>% 
              filter(Year %in% c(1990, 2000, 2015)),
              aes(x=HDI, fill = Year))
g + geom_density(aes(alpha = .3, y = ..scaled..))
guides(alpha = FALSE)            
install.packages("wesanderson")
library("wesanderson")
library(tidyverse)

g + geom_density(aes(alpha = .3, y = ..scaled..)) + 
  guides(alpha = FALSE) +
  scale_fill_manual(values = wes_palette(n = 3, name = "GrandBudapest1")) #n = 3 because we have 3 values

g <- ggplot(hdi, aes(x=Region, fill=Status))  
g + geom_bar()
g + geom_bar(position = "dodge")
g + geom_bar(position = "fill") + #fils bars up to 100%...divides the whole of each category into constituent parts 
scale_y_continuous(labels = scales::percent) +
  coord_flip() + #flips graph on its side 
  theme(legend.position = "top")
hdi

# Labeling graphs ---------------------------------------------------------
load("GIC-workshop/GIC-workshop/data/viz_data.RData")
data("hdi")
g <- ggplot(data= hdi %>%  
              filter(Year %in% seq(2000, 2014, 2),
              Region %in% c("Europe and Central Asia", "North America", "East Asia and the Pacific")) %>% #g+ ets every second year, between 2000 and 2014--> applies a filter
              group_by(Region, Year) %>%  #bc we want to show HDI by region and year
              summarize(HDI.avg=mean(HDI, na.rm = TRUE)), mapping = aes(x = Year, y = HDI.avg))
               
g + geom_point()
install.packages("tinytex")

tinytex::install_tinytex()
library(tinytex)
install.packages("stargazer")
library(stargazer)



# R tips and tricks -------------------------------------------------------
funky <- function(x,y) { # fun + tab to get a code snippet for functions 

# Thu Aug 29 15:19:42 2019 ------------------------------ #ts + tab tab
  
install.packages("colourpicker")
library(colourpicker)    

max(hdi$HDI, na.rm = TRUE) #some kind of shortcut giving us some important info, but don't recall specifics 
hdi  

#look into installing datapasta too, if going to be working with different programs and moving data between them

