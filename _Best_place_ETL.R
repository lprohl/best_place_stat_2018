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

# https://data.worldbank.org/indicator/GC.TAX.TOTL.GD.ZS
df_tax_revenue    <- loadTransponseXL("economics/API_GC.TAX.TOTL.GD.ZS_DS2_en_excel_v2.xls", "tax")
#df_tax_revenue    <- loadTransponseXL("economics/Tax revenue (p of GDP).xlsx", "tax")
#df_foreign_invest <- loadTransponseXL("economics/Foreign investment inflow.xlsx", "foreign_investment")
#df_investment <- loadTransponseXL("economics/Investment.xlsx", "investment")

# https://data.worldbank.org/indicator/MS.MIL.XPND.GD.ZS?page
df_military       <- loadTransponseXL("economics/API_MS.MIL.XPND.GD.ZS_DS2_en_excel_v2.xls", "military")
#df_military       <- loadTransponseXL("economics/military expenditure.xlsx", "military")

# https://data.worldbank.org/indicator/SH.XPD.TOTL.ZS
df_health_spending <- loadTransponseXL("economics/API_SH.XPD.TOTL.ZS_DS2_en_excel_v.xls", "health_spending")
#df_government_health_spending_per_capita <- loadTransponseXL("economics/indicator_per capita government expenditure on health (ppp int. $).xlsx", "government_health_spend_per_capita")
#df_total_health_spending_per_capita <- loadTransponseXL("economics/indicator_per capita total expenditure on health (ppp int. $).xlsx", "total_health_spend_per_capita")

# III.3. loading health data ##########################################
# https://www.gapminder.org/data/

library(dplyr)
#http://apps.who.int/gho/data/  
df_alcohol <- loadTransponseXL("health/Alcohol 2013.xlsx", "alcohol") %>% mutate(alcohol = as.numeric(alcohol))
df_tobacco <- readWorksheetFromFile("health/daily-smoking-prevalence.xlsx" , sheet = 1)
#https://www.gapminder.org/data/
df_sugar <- loadTransponseXL("health/indicator sugar_consumption.xlsx" , "sugar")
df_body_mass_female <- loadTransponseXL("health/Indicator_BMI female ASM.xlsx", "body_mass_female")
df_body_mass_male   <- loadTransponseXL("health/Indicator_BMI male ASM.xlsx", "body_mass_male")
df_kids_mortality   <- loadTransponseXL("health/indicator dead kids 35.xlsx", "kids_mortality")
df_life_expectancy  <- loadTransponseXL("health/indicator life_expectancy_at_birth.xlsx", "life_expectancy")

### IV. MERGING DATA ##################################################
# IV.1. MERGING economics into single table ###########################

df <- merge(x=df_region, y=df_decades, all = TRUE) 
#df <- merge(x=df, y=df_gdp_per_employee, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
#df <- merge(x=df_region, y=, by.x = c("country"), by.y = c("country"), all = TRUE) 
#df <- merge(x=df_decades, y=df, by.x = c("xyear"), by.y = c("year"), all = TRUE) 
df <- merge(x=df, y=df_gdp_per_capita, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
df <- merge(x=df, y=df_tax_revenue, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
df <- merge(x=df, y=df_military, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
df <- merge(x=df, y=df_health_spending, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
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
df <- merge(x=df, y=df_tobacco, by.x = c("country", "year"), by.y = c("country", "year"), all = TRUE) 
df <- merge(x=df, y=df_sugar, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
df <- merge(x=df, y=df_life_expectancy, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
df <- merge(x=df, y=df_kids_mortality, by.x = c("country", "xyear"), by.y = c("country", "year"), all = TRUE) 
df_health <- df

# CHECK FOR FALSE YEAR MERGING - the result should have zero rows
df %>% filter(year > "X1990" ) %>% head()
#df %>% filter(tobacco > 0 & alcohol > 0, year == 2010) %>% head()

### V. PLOTTING GRAPHS ################################################
library(ggplot2)
library(ggrepel)
library(RColorBrewer)

### V.1. DEVELOPMENT EASTERN EUROPE FROM 1980 TO 2015 #################
plot_caption <- "1. GDP of eastern europe countries in 1980 - 2015"
df <- df_economics %>% filter(
  decade %in% c("1980X", "1990X", "2000X", "2010X")
  #, Region_Name %in% c("Europe")
  #, every_5_year == "true"
  , Sub_region_Name %in% c("Eastern Europe") 
  , gdp_per_capita > 0
) %>% mutate (country = reorder(country, -gdp_per_capita))
df_labels <- df %>% filter(year == 2015)
df %>% ggplot(aes(year, gdp_per_capita / 1000, colour = country, label = ISO_alpha3_Code))+
  geom_point() + 
  geom_line() + 
  scale_color_discrete(name ="Country")+
  geom_label_repel(data=df_labels, 
                   aes(year, gdp_per_capita / 1000), 
                   size=3, direction = "x") + 
  facet_grid(.~Region_Name) + 
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle(plot_caption)+
  ylab("GDP per capita, 1000 $") +
  xlab("Year")
ggsave(paste(plot_caption, "png", sep="."),height = 5, width = 8, dpi = 144, units = "in", device='png')

### V.2. DEVELOPMENT OF POST SOVIET COUNTRIES FROM 1980 TO 2015 #################
plot_caption <- "2. GDP of post soviet countries in 1980 - 2015"
df <- df_economics %>% filter(
  decade %in% c("1980X", "1990X", "2000X", "2010X")
  #, Region_Name %in% c("Europe")
  #, every_5_year == "true"
  #, Sub_region_Name %in% c("Eastern Europe") |
  ,  ISO_alpha3_Code %in% c("RUS", "BLR", "UKR", "ROU", "EST", "LVA", "LTU", "KAZ", "KGZ", "TJK", "TKM", "UZB", "GEO", "ARM", "AZE") #, "Northern Europe")
  , gdp_per_capita > 0
) %>% mutate (country = reorder(country, -gdp_per_capita))
df_labels <- df %>% filter(year == 2015)
df %>% ggplot(aes(year, gdp_per_capita / 1000, colour = country, label = ISO_alpha3_Code))+
  geom_point() + 
  geom_line() + 
  scale_color_discrete(name ="Country")+
  geom_label_repel(data=df_labels, 
                   aes(year, gdp_per_capita / 1000), 
                   size=3, direction = "x") + 
  facet_grid(.~Region_Name) + 
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle(plot_caption)+
  ylab("GDP per capita, 1000 $") +
  xlab("Year") 
ggsave(paste(plot_caption, "png", sep="."),height = 5, width = 8, dpi = 144, units = "in", device='png')

### V.3. GDP IN ALL COUNTRIES 1980 - 2015 ##############################################
plot_caption <- "3. GDP of all countries in 1980 - 2015"
df <- df_economics %>% filter(
  year >= 1980 & year <= 2015
  #year %in% c(1990, 2000, 2009)
  #, Region_Name %in% c("Europe")
  , every_5_year == "true"
  #, Sub_region_Name %in% c("Eastern Europe")#, "Northern Europe")
  , gdp_per_capita > 0 
) %>% mutate (Region_Name = reorder(Region_Name, -gdp_per_capita), gdp_per_capita = gdp_per_capita / 1000)

df %>% select(country, Region_Name)

#df_labels <- df %>% filter((year == 2015 | year == 1980) & (best_place_to_live == "check" | gdp_per_capita + (2000 - year) * 200 > 60000))
df_labels_left  <- df %>% filter(year == 2015 & best_place_to_live == "check")
df_labels_right <- df %>% filter(year == 1980 & best_place_to_live == "check")
df_lines <- df %>% filter(best_place_to_live == "check")
df %>% ggplot(aes(jitter(as.numeric(year),factor=1.5), gdp_per_capita, colour = Region_Name, label = ISO_alpha3_Code)
)+
  geom_point() + 
  #geom_label(data=df_labels_left, aes(year, gdp_per_capita), 
  #          size=3, hjust = -1.1, vjust = -0.1)+ #, check_overlap = TRUE) + 
  #geom_text(data=df_labels_right, aes(year, gdp_per_capita), 
  #          size=3, hjust = 2,  vjust = -0.1)+ #, check_overlap = TRUE) + 
  geom_line(data = df_lines, aes(year, gdp_per_capita, group = country))+
  scale_color_discrete(name ="Region")+
  geom_label_repel(data=df_labels_left, aes(year, gdp_per_capita), label.padding = 0.1, 
                   size=2.5, direction = "x", label.size = 0.1)+ #, check_overlap = TRUE) + 
  geom_label_repel(data=df_labels_right, aes(year, gdp_per_capita), label.padding = 0.1, 
                   size=2.5, direction = "x", label.size = 0.1)+ #, check_overlap = TRUE) + 
  #facet_grid(.~year) + 
  scale_y_continuous(trans="sqrt") + 
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle(plot_caption)+
  ylab("GDP per capita, 1000 $") +
  xlab("Year")
ggsave(paste(plot_caption, "png", sep="."),height = 5, width = 8, dpi = 144, units = "in", device='png')

### V.4. TAX, MILITARY VS HEALTH EXPENDITURE IN 2014 ###########################################
plot_caption <- "4. Tax income, military, and health expenditure of selected countries in 2014"
df <- df_economics %>% filter(best_place_to_live == "check", year == 2014)# %>% mutate(health_spending = health_spending / gdp_per_capita * 100) 
df1 <- df %>% mutate(type = "military", value = military) %>% 
  select(type, value, tax, country, year, Region_Name)
df2 <- df %>% mutate(type = "health", value = health_spending) %>% 
  select(type, value, tax, country, year, Region_Name)
df <- union(df1, df2)
df %>% ggplot(aes(value, tax, label = country, colour = Region_Name, group = type))+
  #geom_point(shape = 23, size = 4) + 
  #geom_point() + 
  geom_point(aes(shape = factor(type)), size = 4) +
  scale_color_discrete(name ="Region")+
  scale_shape_discrete(name ="Type")+
  geom_label_repel(size=4, box.padding = 0.5)+ #, direction = "x")+ 
  #geom_point(shape = 20, data = df, 
  #           aes(government_health_spend, tax, size = 5, colour = Region_Name)) + 
  #geom_label_repel(data = df, box.padding = 0.5, #direction = "x", 
  #                 aes(government_health_spend, tax, label = country, colour = Region_Name)) +
  ggtitle(plot_caption)+
  #theme(legend.position="none")+
  ylab("Tax income, % of GDP")+
  xlab("Govrenment expenditure % of GDP")
ggsave(paste(plot_caption, "png", sep="."),height = 5, width = 8, dpi = 144, units = "in", device='png')

#+ facet_grid(Region~.)

#PLOTTING Health #########################################################
### V.5. BODY MASS DYNAMICS ###########################################
plot_caption <- "5. Body mass indexes  in 1980-2008 for selected countries, kg per m2"
df <- df_health %>% 
  filter(best_place_to_live == "check", 
         #Region_Name == "Europe",
         decade %in% c("1980X", "1990X", "2000X", "2010X")
         & body_mass_male > 0
        )
df_labels_left  <- df %>% filter(year == 2008 & best_place_to_live == "check")
df_labels_right <- df %>% filter(year == 1980 & best_place_to_live == "check")

df %>%  ggplot(aes(body_mass_male, body_mass_female, 
                   label = ISO_alpha3_Code, colour = five_year))+
  geom_point() + 
  scale_color_discrete(name ="Year")+
  #geom_text(vjust=0, hjust=0, check_overlap = TRUE)+
  geom_label_repel(data=df_labels_left, aes(body_mass_male, body_mass_female), label.padding = 0.1, 
                   size=4, direction = "x", label.size = 0.1)+ #, check_overlap = TRUE) + 
  geom_label_repel(data=df_labels_right, aes(body_mass_male, body_mass_female), label.padding = 0.1, 
                   size=4, direction = "x", label.size = 0.1)+ #, check_overlap = TRUE)  
  xlab("Body mass index male")+
  ylab("Body mass index female")+
  #theme(legend.title =element_text(legend.title = "Year", colour="black", size=16))+
  ggtitle(plot_caption)
  #+facet_grid(Region~.)
ggsave(paste(plot_caption, "png", sep="."),height = 5, width = 8, dpi = 144, units = "in", device='png')

### V.5.1. SUGAR CONSUMPTION ###########################################
plot_caption <- "5.1 Sugar consumption per person, g per day and BMI avg"
df <- df_health %>% 
  filter(best_place_to_live == "check", 
         #Region_Name == "Europe",
         decade %in% c("1990X", "2000X", "2010X"),
         sugar > 0
         ) %>% mutate (BMI = (body_mass_male + body_mass_female) / 2)
df_labels_left  <- df %>% filter(year == 1992 & best_place_to_live == "check")
df_labels_right <- df %>% filter(year == 2004 & best_place_to_live == "check")
#df %>% select(country, Region_Name)

df %>%  ggplot(aes(BMI, sugar, 
                   label = ISO_alpha3_Code, colour = year, group = country, shape = Region_Name, order = year))+
  geom_point() + 
  geom_path() + 
  scale_color_continuous(name ="Year")+
  scale_shape_discrete(name ="Region")+
  #geom_text(vjust=0, hjust=0, check_overlap = TRUE)+
  geom_label_repel(data=df_labels_left, aes(BMI, sugar), label.padding = 0.1, 
                   size=4, direction = "x", label.size = 0.1)+ #, check_overlap = TRUE) + 
  geom_label_repel(data=df_labels_right, aes(BMI, sugar), label.padding = 0.1, 
                   size=4, direction = "x", label.size = 0.1)+ #, check_overlap = TRUE)  
  #xlab("Body mass index male")+
  #ylab("Body mass index female")+
  #theme(legend.title =element_text(legend.title = "Year", colour="black", size=16))+
  ggtitle(plot_caption)#+
  #facet_grid(.~Region_Name)
ggsave(paste(plot_caption, "png", sep="."),height = 5, width = 8, dpi = 144, units = "in", device='png')

### V.6. TOBACCO VS ALCOHOL ###########################################
plot_caption <- "6. Tobacco and alcohol consumption in 2000-2010"
df <- df_health %>% filter(best_place_to_live == "check", 
                           decade %in% c("1990X", "2000X", "2010X"),
                           alcohol > 0 & tobacco > 0
    )
df_labels_left  <- df %>% filter(year == 2010 & best_place_to_live == "check")
df_labels_right <- df %>% filter(year == 2000 & best_place_to_live == "check")

#%>% mutate (country = reorder(country, -alcohol))
df %>% ggplot(aes(tobacco, alcohol, label = ISO_alpha3_Code, group = country, color = year, shape = Region_Name, order = year))+
  #scale_fill_discrete(name="year")+
  geom_point(size = 3) +
  geom_path() +
  scale_color_continuous(name ="Year")+
  scale_shape_discrete(name ="Region \n name")+
  geom_label_repel(data = df_labels_left, aes(tobacco, alcohol, label = ISO_alpha3_Code, color = year), label.padding = 0.1)+ #, direction = "x")+

  geom_label_repel(data = df_labels_right, aes(tobacco, alcohol, label = ISO_alpha3_Code, color = year), label.padding = 0.1)+ #, direction = "x")+
  xlab("Tobacco use, % of adults (>= 15 years)")+
  ylab("Alcohol consumption per adult, liters (>= 15 years)")+
  ggtitle(plot_caption)
  #theme(legend.position = "none")+
  #facet_grid(.~Region_Name)
ggsave(paste(plot_caption, "png", sep="."),height = 5, width = 8, dpi = 144, units = "in", device='png')

### V.6.1 ALCOHOL LEADERS OVERALL ###########################################
plot_caption <- "6.1. Alcohol highest consuming countries (avg > 12 or max > 14 l)"
highest_alcohol_consumption <- df_health %>% filter(decade %in% c("1990X", "2000X", "2010X")) %>% 
  select(country, alcohol) %>% group_by(country) %>% summarise(mean_alcohol = mean(alcohol, na.rm = TRUE), max_alcohol = max(alcohol, na.rm = TRUE)) %>% 
  filter(mean_alcohol > 12 | max_alcohol > 14 | country == "Russia")

df <- df_health %>% filter(country %in% as.vector(highest_alcohol_consumption$country), 
                           decade %in% c("1990X", "2000X", "2010X"),
                           alcohol > 0 #& tobacco > 0
                           #& country %in% c("Russia", "Austria", "United Kingdom", "Poland", "Spain")
)
df_labels_left  <- df %>% filter(year == 2014 | country == "Estonia" & year == "2010"| country == "Cyprus" & year == "2013")
df_labels_right <- df %>% filter(year == 2000 | country == "Estonia" & year == "2002")

#%>% mutate (country = reorder(country, -alcohol))
df %>% ggplot(aes(year, alcohol, label = ISO_alpha3_Code, group = country, color = country, shape = Region_Name, order = year))+
  #scale_fill_discrete(name="year")+
  geom_point(size = 3) +
  geom_path() +
  scale_color_discrete(name ="Country")+
  scale_shape_discrete(name ="Region \n name")+
  geom_label_repel(data = df_labels_left, aes(year, alcohol, label = ISO_alpha3_Code, color = country), label.padding = 0.1, direction = "x")+
  
  geom_label_repel(data = df_labels_right, aes(year, alcohol, label = ISO_alpha3_Code, color = country), label.padding = 0.1, direction = "x")+
  xlab("Year")+
  ylab("Alcohol consumption per adult, liters (>= 15 years)")+
  ggtitle(plot_caption)
#theme(legend.position = "none")+
#facet_grid(.~Region_Name)
ggsave(paste(plot_caption, "png", sep="."),height = 5, width = 8, dpi = 144, units = "in", device='png')


### V.6.2 TOBACCO LEADERS OVERALL ###########################################
plot_caption <- "6.2. Tobacco highest consuming countries (avg > 30 or max > 37)"
highest_tobacco_consumption <- df_health %>% filter(decade %in% c("1990X", "2000X", "2010X")) %>%
  select(country, tobacco) %>% group_by(country) %>% summarise(mean_tobacco = mean(tobacco, na.rm = TRUE), max_tobacco = max(tobacco, na.rm = TRUE)) %>% 
  filter(mean_tobacco > 30 | max_tobacco > 37 | country == "Russia"| country == "Austria")

df <- df_health %>% filter(country %in% as.vector(highest_tobacco_consumption$country), 
                           decade %in% c("1990X", "2000X", "2010X"),
                           tobacco > 0)
df_labels_left  <- df %>% filter(year == 2012 | country == "Estonia" & year == "2010"| country == "Cyprus" & year == "2013")
df_labels_right <- df %>% filter(year == 1990 | country == "Estonia" & year == "2002")

df %>% ggplot(aes(year, tobacco, label = ISO_alpha3_Code, group = country, color = country, shape = Region_Name, order = year))+
  #scale_fill_discrete(name="year")+
  geom_point(size = 3) +
  geom_path() +
  scale_color_discrete(name ="Country")+
  scale_shape_discrete(name ="Region \n name")+
  geom_label_repel(data = df_labels_left, aes(year, tobacco, label = ISO_alpha3_Code, color = country), label.padding = 0.1, direction = "x")+
  
  geom_label_repel(data = df_labels_right, aes(year, tobacco, label = ISO_alpha3_Code, color = country), label.padding = 0.1, direction = "x")+
  xlab("Year")+
  ylab("Tobacco use, % of adults (>= 15 years)")+
  ggtitle(plot_caption)
#theme(legend.position = "none")+
#facet_grid(.~Region_Name)
ggsave(paste(plot_caption, "png", sep="."),height = 5, width = 8, dpi = 144, units = "in", device='png')

### V.7. KIDS MORTALITY ###########################################
plot_caption <- "7. Kids mortality"
df <- df_health %>% filter(best_place_to_live == "check", 
                           decade %in% c("1980X", "1990X", "2000X", "2010X"),
                           kids_mortality > 0
) %>% mutate (country = reorder(country, -kids_mortality))
df %>% ggplot(aes(year, country, fill = kids_mortality, color = Region_Name))+
  geom_tile(color = "grey50")+
  # scale_x_continuous(expand = c(0,0))+
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt", name ="%") + 
  #geom_vline(xintercept = 1963, col = "blue")+
  theme_minimal() + theme(panel.grid = element_blank())+
  ggtitle(plot_caption)+
  ylab("")+
  xlab("")
ggsave(paste(plot_caption, "png", sep="."),height = 5, width = 8, dpi = 144, units = "in", device='png')

### V.8. LIFE EXPECTANCY ###########################################
plot_caption <- "8. Life expectancy"
df <- df_health %>% filter(best_place_to_live == "check", 
                           decade %in% c("1980X", "1990X", "2000X", "2010X"),
                           life_expectancy > 0
) %>% mutate (country = reorder(country, life_expectancy))
df %>% ggplot(aes(year, country, fill = life_expectancy, color = Region_Name))+
  geom_tile(color = "grey50")+
  # scale_x_continuous(expand = c(0,0))+
  scale_fill_gradientn(colors = brewer.pal(9, "Greens"), trans = "sqrt", name ="Years") + 
  #geom_vline(xintercept = 1963, col = "blue")+
  theme_minimal() + theme(panel.grid = element_blank())+
  #scale_fill_continuous(name ="Years")+
  ggtitle(plot_caption)+
  ylab("")+
  xlab("")
ggsave(paste(plot_caption, "png", sep="."),height = 5, width = 8, dpi = 144, units = "in", device='png')

