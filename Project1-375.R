# Names: Tilak Ghorashainee, Edgar Lara, Allen Rivas
# CPSC 375-01
# Project 1

# Call tidyverse library since it is required for tidying our data.
library(tidyverse)

# Read the corresponding CVS files and store them into covid19, gdp, and demographics.
covid19  <- read.csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv")
gdp <- read_csv("C:/Users/allen/Downloads/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_3011433.csv")
demographics <- read_csv("C:/Users/allen/Downloads/demographics.csv")

# Use the view function to give a better understanding of the rows and columns of each CSV file.
view(covid19)
view(gdp)
view(demographics)

######### Data Wrangling #########

# Tidy up covid19 and store it into a new variable called newcovid19. 
newcovid19 <- covid19 %>% pivot_longer(cols = starts_with("X"), names_to = "Date", values_to = "shots",  values_drop_na = TRUE) %>% select(c("iso3","Country_Region","Population","shots" )) %>% filter(!if_any(starts_with("shots"), ~ . == 0)) %>% group_by(iso3) %>% mutate(vacRate =(shots/Population), daysSinceStart=row_number())
newcovid19

# Tidy up gdp and store it into a new variable called newgdp.
newgdp <- gdp %>% select(c("Country Code","2020")) %>% rename(GDP = "2020")
newgdp

# Tidy up demographics and store it into a new variable called newdemographics.
newdemographics <- demographics %>% pivot_wider(names_from = 'Series Code', values_from = 'YR2015', -c('Series Name')) %>% mutate(SP.POP.80UP=SP.POP.80UP.FE+SP.POP.80UP.MA, SP.POP.1564.IN=SP.POP.1564.MA.IN+SP.POP.1564.FE.IN, SP.POP.0014.IN=SP.POP.0014.MA.IN+SP.POP.0014.FE.IN, SP.DYN.AMRT=SP.DYN.AMRT.FE+SP.DYN.AMRT.MA, SP.POP.TOTL.IN=SP.POP.TOTL.FE.IN+SP.POP.TOTL.MA.IN, SP.POP.65UP.IN=SP.POP.65UP.FE.IN+SP.POP.65UP.MA.IN) %>% pivot_wider(-c(SP.POP.80UP.FE, SP.POP.80UP.MA, SP.POP.1564.MA.IN, SP.POP.1564.FE.IN, SP.POP.0014.MA.IN, SP.POP.0014.FE.IN, SP.DYN.AMRT.FE, SP.DYN.AMRT.MA, SP.POP.TOTL.FE.IN, SP.POP.TOTL.MA.IN, SP.POP.65UP.FE.IN, SP.POP.65UP.MA.IN)) %>% select(c("Country Code", "SP.DYN.LE00.IN", "SP.URB.TOTL", "SP.POP.TOTL", "SP.POP.80UP", "SP.POP.1564.IN", "SP.POP.0014.IN", "SP.DYN.AMRT", "SP.POP.TOTL.IN", "SP.POP.65UP.IN"))
newdemographics

# To combine the three files as need, a full join was first done with newgdp and newdemographics by 
# their respective Country Code which was stored into the new variable called GDPdemographics.
GDPdemographics <- newgdp %>% full_join(newdemographics, by=c("Country Code" = "Country Code"))
GDPdemographics

# A second full join was done using the GDPdemopgrahics with newcovid19 by Country Code and iso3
# which was finally stored into the new variable called tidycovid19.
tidycovid19 <- newcovid19 %>% full_join(GDPdemographics, by=c("iso3" = "Country Code"))
tidycovid19

# Use the view function to give a better understanding of the rows and columns for tidycovid19.
view(tidycovid19)

# Since for the scatter plot the most recent vaccination date was required, we modified tidycovid19 and
# stored that into a new variable called tidycovid19.2
# tidycovid19.2 was only used for the scatter plot portion of the project. 
tidycovid19.2 <- tidycovid19 %>% top_n(1,daysSinceStart)
# Using ggplot2 we created a scatter plot of the most recent vaccination rate. 
ggplot(data=tidycovid19.2) + geom_point(mapping=aes(x=daysSinceStart,y=vacRate,color=iso3), show.legend = F) + scale_x_continuous(limits=c(226,320)) + scale_y_continuous(limits = c(0.0,2.5))


######### Modeling #########

# lmDaysSinceStart, lmPopulation, lmGDP, lmSPDYNLEOOIN, lmPopTotl each store a summary of their 
# corresponding linear model.
lmDaysSinceStart <- summary(lm(data = tidycovid19, vacRate~daysSinceStart))
lmDaysSinceStart
lmPopulation <- summary(lm(data = tidycovid19, vacRate~daysSinceStart + Population))
lmPopulation
lmGDP <- summary(lm(data = tidycovid19, vacRate~daysSinceStart + Population + GDP))
lmGDP
lmSPDYNLEOOIN <- summary(lm(data = tidycovid19, vacRate~daysSinceStart + Population + GDP + SP.DYN.LE00.IN))
lmSPDYNLEOOIN
lmPopTotl <- summary(lm(data = tidycovid19, vacRate~daysSinceStart + Population + GDP + SP.DYN.LE00.IN + SP.POP.TOTL))
lmPopTotl

# Transform tidycovid19 variable so we could create our following linear models below.
tidycovid19 <- tidycovid19 %>% mutate(Population.Proportion = Population/SP.POP.TOTL)

# lmPopulation.Prop, lm.all.predictor each store a summary of their 
# corresponding linear model.
lmPopulation.Prop <- summary(lm(data=tidycovid19,vacRate~daysSinceStart + Population + GDP+ SP.DYN.LE00.IN + SP.POP.TOTL+Population.Proportion))
lmPopulation.Prop
lm.all.predictor <- summary(lm(data=tidycovid19,vacRate~iso3+daysSinceStart + Population + GDP+ SP.DYN.LE00.IN + SP.URB.TOTL + SP.POP.TOTL + SP.POP.80UP+ SP.POP.1564.IN+ SP.POP.0014.IN + SP.DYN.AMRT + SP.POP.65UP.IN + Population.Proportion, na.action="na.omit"))
lm.all.predictor

# Create two vectors lmModels and R2values so that we later use to create a data frame.
lmModels <- c("lmDaysSinceStart", "lmPopulation", "lmGDP", "lmSPDYNLEOOIN","lmSP.POP.TOTL","lmPopulation.Proportion","lm.all.predictors")
R2values <- c(lmDaysSinceStart$adj.r.squared,lmPopulation$adj.r.squared,lmGDP$adj.r.squared,lmSPDYNLEOOIN$adj.r.squared,lmPopTotl$adj.r.squared,lmPopulation.Prop$adj.r.squared,lm.all.predictor$adj.r.squared)

# Verifying if hey are the same length.
length(lmModels)
length(R2values)

# Create a data frame using the variables lmModels and R2values so, we can later use them to plot our bar graph. 
lmDF <- data.frame(lmModels, R2values)
lmDF

# Create a bar graph using lmDF which was create beforeas our data. 
# lmModels is our x value while R2values is our y values. 
ggplot(data=lmDF) + geom_bar(mapping=aes(x=lmModels, y=R2values), stat='identity') + labs(y="Adjust R-Squared value",title="Comparing our Linear Model Regression") + theme(plot.title = element_text(hjust=0.5))

