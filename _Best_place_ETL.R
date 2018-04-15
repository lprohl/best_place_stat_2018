### I. INSTALLATION OF PACKAGES REQUIRED ##############################
### Uncomment the following lines and run them once. ##################
#install.packages("XLConnect") 
#install.packages("reshape")

### II. PREPARING ENVIRONMENT #########################################
### Insert the working path to your working directory here ############

setwd("/media/poroh/Data/EDX Data Science/R visualization/best place") 

### This function will be used for loading the two-dimensional XLS file
### into the one-dimensional data frame. ##############################
### Parameters: #######################################################
###  fileName - the path to the file in the working directory #########
###  valueName - the name of column for the resulting data frame ######

library("XLConnect")

loadTransponseXL <- function(fileName, valueName = "") {
  #1. loading from XLS
  df <- readWorksheetFromFile(fileName, sheet = 1)

  #2. transforming years into one column
  library(reshape)
  mdf <- melt(df, id=c(names(df)[1]), na.rm=TRUE) 
  if (valueName == "") {valueName <- names(mdf)[1]}
  
  names(mdf)[1] <- "country"
  names(mdf)[2] <- "year"
  names(mdf)[3] <- valueName
  return (mdf)
}

### III. LOADING DATA #################################################
# III.1. loading key dimensions #######################################

# UNSD — Methodology.xlsx - https://unstats.un.org/unsd/methodology/m49/overview/ 
df_UN_region <- readWorksheetFromFile("dimensions/UNSD — Methodology.xlsx", sheet = 1) 

df_countries_of_interest <- readWorksheetFromFile("dimensions/Countries of interest.xlsx", sheet = 1)
df_region <- merge(x=df_UN_region, y=df_countries_of_interest, by.x = c("country"), by.y = c("country"), all = TRUE) 
df_decades <- readWorksheetFromFile("dimensions/decades.xlsx", sheet = 1)

# III.2. loading economical data ######################################
# https://www.gapminder.org/data/

#df_gdp_per_employee <- loadTransponseXL("economics/indicator_gdp per employee.xlsx", "gdp_per_employee")
df_gdp_per_capita <- loadTransponseXL("economics/indicator gapminder gdp_per_capita_ppp.xlsx", "gdp_per_capita")
df_tax_revenue    <- loadTransponseXL("economics/Tax revenue (p of GDP).xlsx", "tax")
#df_foreign_invest <- loadTransponseXL("economics/Foreign investment inflow.xlsx", "foreign_investment")
#df_investment <- loadTransponseXL("economics/Investment.xlsx", "investment")
df_military       <- loadTransponseXL("economics/military expenditure.xlsx", "military")
df_government_health_spending_per_capita <- loadTransponseXL("economics/indicator_per capita government expenditure on health (ppp int. $).xlsx", "government_health_spend_per_capita")
#df_total_health_spending_per_capita <- loadTransponseXL("economics/indicator_per capita total expenditure on health (ppp int. $).xlsx", "total_health_spend_per_capita")

# III.3. loading health data ##########################################
# https://www.gapminder.org/data/

df_alcohol <- loadTransponseXL("health/indicator alcohol consumption  20100830.xlsx", "alcohol")
df_tobacco <- loadTransponseXL("health/indicator_prevalence of current tobacco use among adults (%) both sexes.xlsx" , "tobacco")
df_body_mass_female <- loadTransponseXL("health/Indicator_BMI female ASM.xlsx", "body_mass_female")
df_body_mass_male   <- loadTransponseXL("health/Indicator_BMI male ASM.xlsx", "body_mass_male")
df_kids_mortality   <- loadTransponseXL("health/indicator dead kids 35.xlsx", "kids_mortality")
df_life_expectancy  <- loadTransponseXL("health/indicator life_expectancy_at_birth.xlsx", "life_expectancy")

### IV. MERGING DATA ##################################################
# IV.1. MERGING economics into single table ###########################

library(dplyr)
df <- merge(x=df_region, y=df_decades, all = TRUE) 
#df <- merge(x=df, y=df_gdp_per_employee, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
#df <- merge(x=df_region, y=, by.x = c("country"), by.y = c("country"), all = TRUE) 
#df <- merge(x=df_decades, y=df, by.x = c("xyear"), by.y = c("year"), all = TRUE) 
df <- merge(x=df, y=df_gdp_per_capita, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
df <- merge(x=df, y=df_tax_revenue, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
df <- merge(x=df, y=df_military, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
df <- merge(x=df, y=df_government_health_spending_per_capita, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
#df <- merge(x=df, y=df_total_health_spending_per_capita, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
#df <- merge(x=df, y=df_investment, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
#df <- merge(x=df, y=df_foreign_invest, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
df_economics <- df

# CHECK FOR FALSE YEAR MERGING - the result should have zero rows
df %>% filter(year > "X1990" ) %>% head()

# IV.2. MERGING health into single table ##############################

df <- merge(x=df_region, y=df_decades, all = TRUE) 
#df <- merge(x=df_region, y=df_body_mass_female, by.x = c("country"), by.y = c("country"), all = TRUE) 
#df <- merge(x=df_decades, y=df, by.x = c("xyear"), by.y = c("year"), all = TRUE) 
df <- merge(x=df, y=df_body_mass_female, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
df <- merge(x=df, y=df_body_mass_male, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
df <- merge(x=df, y=df_alcohol, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
df <- merge(x=df, y=df_tobacco, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
df <- merge(x=df, y=df_life_expectancy, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
df <- merge(x=df, y=df_kids_mortality, by.x = c("country", "xyear"), by.y = c("country", "year"), all = FALSE) 
df_health <- df

# CHECK FOR FALSE YEAR MERGING - the result should have zero rows
df %>% filter(year > "X1990" ) %>% head()

### V. PLOTTING GRAPHS ################################################
library(ggplot2)
library(ggrepel)

### V.1. PLOTTING GRAPHS ################################################
#1. DEVELOPMENT OF EASTERN EUROPE FROM 1980 TO 2015
plot_caption <- "GDP of eastern europe countries in 1980 - 2015"
df <- df_economics %>% filter(
  decade %in% c("1980X", "1990X", "2000X", "2010X")
  , Region_Name %in% c("Europe")
  #, every_5_year == "true"
  , Sub_region_Name %in% c("Eastern Europe")#, "Northern Europe")
  , gdp_per_capita > 0
) %>% mutate (country = reorder(country, -gdp_per_capita))
df_labels <- df %>% filter(year == 2014)
df %>% ggplot(aes(year, gdp_per_capita, colour = country, label = ISO_alpha3_Code))+
  geom_point() + 
  geom_label_repel(data=df_labels, 
                   aes(year, gdp_per_capita, nooverlap = TRUE), 
                   size=3, direction = "x", vjust = -0.8) + 
  facet_grid(Sub_region_Name~.) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle(plot_caption)


#********************************************************************************
#2. GDP IN 1980 - 2015
df <- df_economics %>% filter(
  year >= 1980 & year <= 2015
  #year %in% c(1990, 2000, 2009)
  #, Region_Name %in% c("Europe")
  , every_5_year == "true"
  #, Sub_region_Name %in% c("Eastern Europe")#, "Northern Europe")
  , gdp_per_capita > 0 
) %>% mutate (Region_Name = reorder(Region_Name, -gdp_per_capita))
#df_labels <- df %>% filter((year == 2015 | year == 1980) & (best_place_to_live == "check" | gdp_per_capita + (2000 - year) * 200 > 60000))
df_labels_left  <- df %>% filter(year == 2015 & best_place_to_live == "check")
df_labels_right <- df %>% filter(year == 1980 & best_place_to_live == "check")
df_lines <- df %>% filter(best_place_to_live == "check")
df %>% ggplot(aes(jitter(as.numeric(year),factor=1), gdp_per_capita, colour = Region_Name, label = ISO_alpha3_Code)
)+
  geom_point() + 
  #geom_label(data=df_labels_left, aes(year, gdp_per_capita), 
  #          size=3, hjust = -1.1, vjust = -0.1)+ #, check_overlap = TRUE) + 
  #geom_text(data=df_labels_right, aes(year, gdp_per_capita), 
  #          size=3, hjust = 2,  vjust = -0.1)+ #, check_overlap = TRUE) + 
  geom_line(data = df_lines, aes(year, gdp_per_capita, group = country))+
  geom_label_repel(data=df_labels_left, aes(year, gdp_per_capita), label.padding = 0.1, 
                   size=4, direction = "x", label.size = 0.1)+ #, check_overlap = TRUE) + 
  geom_label_repel(data=df_labels_right, aes(year, gdp_per_capita), label.padding = 0.1, 
                   size=4, direction = "x", label.size = 0.1)+ #, check_overlap = TRUE) + 
  #facet_grid(.~year) + 
  scale_y_continuous(trans="sqrt") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#3. MILITARY VS HEALTH EXPENDITURE IN 2009
df <- df_economics %>% filter(best_place_to_live == "check", year == 2009) %>% mutate(government_health_spend = government_health_spend_per_capita / gdp_per_capita * 100) 
df %>% ggplot(aes(military, tax, label = country, colour = Region_Name))+
  geom_point(shape = 18, size = 4) + 
  #geom_point(aes(shape = factor(country))) + 
  geom_label_repel(size=4, direction = "x", box.padding = 0.5)+ 
  geom_point(shape = 20, data = df, 
             aes(government_health_spend, tax, size = 4, label = country, colour = Region_Name)) + 
  geom_label_repel(data = df, direction = "x", box.padding = 0.5, 
                   aes(government_health_spend, tax, label = country, colour = Region_Name)) 
#+ facet_grid(Region~.)

#PLOTTING Health
p_health <- df_health %>% filter(best_place_to_live == "check", every_5_year == "true", decade %in% c("1980X", "1990X", "2000X")
    ) %>% ggplot(aes(body_mass_male, body_mass_female, label = ISO_alpha3_Code, colour = five_year))

#BODY MASS DYNAMICS
df <- df_health %>% 
  filter(best_place_to_live == "check", 
         #Region_Name == "Europe",
         decade %in% c("1980X", "1990X", "2000X"))
df %>%  ggplot(aes(body_mass_male, body_mass_female, 
                   label = ISO_alpha3_Code, colour = five_year))+
  geom_point() + 
  geom_text(vjust=0, hjust=0, check_overlap = TRUE) 
#+facet_grid(Region~.)

#TOBACCO_VS_ALCOHOL IN 2005
df <- df_health %>% filter(best_place_to_live == "check", 
                     year == 2005,
                     alcohol > 0 & tobacco > 0) 
df %>% ggplot(aes(tobacco, alcohol, label = ISO_alpha3_Code, color = country))+
  geom_point() +
  geom_label_repel(direction = "x")

  #facet_grid(Region~.)

#KIDS_MORTALITY
plot_name <- "Kids mortality"
df <- df_health %>% filter(best_place_to_live == "check", 
                           decade %in% c("1980X", "1990X", "2000X", "2010X"),
                           kids_mortality > 0
) %>% mutate (country = reorder(country, -kids_mortality))
df %>% ggplot(aes(year, country, fill = kids_mortality, color = Region_Name))+
  geom_tile(color = "grey50")+
 # scale_x_continuous(expand = c(0,0))+
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  #geom_vline(xintercept = 1963, col = "blue")+
  theme_minimal() + theme(panel.grid = element_blank())+
  ggtitle(plot_name)+
  ylab("")+
  xlab("")

#LIFE_EXPECTANCY
plot_name <- "Life expectancy"
df <- df_health %>% filter(best_place_to_live == "check", 
                           decade %in% c("1980X", "1990X", "2000X", "2010X"),
                           life_expectancy > 0
) %>% mutate (country = reorder(country, life_expectancy))
df %>% ggplot(aes(year, country, fill = life_expectancy, color = Region_Name))+
  geom_tile(color = "grey50")+
  # scale_x_continuous(expand = c(0,0))+
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  #geom_vline(xintercept = 1963, col = "blue")+
  theme_minimal() + theme(panel.grid = element_blank())+
  ggtitle(plot_name)+
  ylab("")+
  xlab("")


#  geom_point(aes(shape = factor(decade))) + 
#  scale_x_continuous(trans="log2")+
#  #geom_text(vjust=0, hjust=0, check_overlap = TRUE) + 
#  geom_label_repel(data=df_labels_left, aes(kids_mortality, life_expectancy), label.padding = 0.1, 
#                   size=4, direction = "x", label.size = 0.1) +  #, check_overlap = TRUE) + 
#  geom_label_repel(data=df_labels_rigth, aes(kids_mortality, life_expectancy), label.padding = 0.1, 
#                   size=4, direction = "x", label.size = 0.1) #, check_overlap = TRUE) + 
##facet_grid(Region~.)

#PLOTTING economics
#value_columns_economics <- c("gdp_per_employee", "gdp_per_capita", "tax", "investment", "military", "government_health_spend_per_capita", "total_health_spend_per_capita")
#"gdp_per_employee", "tax", ""
#best_place_to_live == "check"
#%>% reorder(df_economics.$country, df_economics.$gdp_per_capita, FUN=mean)


#xlab, ylab, ggtitle


#xlab, ylab, ggtitle
#c("gdp_per_employee", "gdp_per_capita", "tax", "investment", "military", "government_health_spend_per_capita", "total_health_spend_per_capita")

#df_economics %>% filter(decade %in% c("1980X", "1990X", "2000X")
#) %>% ggplot(aes(gdp_per_employee, tax, check_overlap = TRUE, colour = five_year))+geom_point() + facet_grid(Region~.)


#df_economics %>% filter(best_place_to_live == "check",decade %in% c("1980X", "1990X", "2000X", "2010X")
#) %>% ggplot(aes(year, investment, colour = country))+geom_line(aes(shape = factor(country))) #+ facet_grid(Region~.)

#df_economics %>% filter(best_place_to_live == "check",decade %in% c("1980X", "1990X", "2000X", "2010X")
#) %>% ggplot(aes(year, military, colour = country))+geom_line(aes(shape = factor(country))) 


#df_invest
df_tobacco %>% head()

#read.csv("Investment.csv", header=TRUE)
#?read.csv

library(RColorBrewer)
display.brewer.all(type = "seq")
display.brewer.all(type = "div")
