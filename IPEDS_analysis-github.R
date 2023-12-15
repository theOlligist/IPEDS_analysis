options(scipen=999)
rm(list = ls())
setwd("~/Dropbox/R_general_working_directory/R-workshop/")
library(tidyverse);library(reshape2);library(waffle);library(readxl)

#Questions:
#0) What is the racial demographic of the united states? (done)
#0.2) What is the ratio of the different awards given: (done)
#1) What is the racial breakdown of people earning the different types of degrees: B.S., MS, PhD, Certificates? (done)
#I think that if we hypothetically saw that blacks and whites are 50 50 in total degrees, we 
# may see a difference in the types of degrees and institutions. So.
#2) What types of degrees (cips) and institutions (carnigie) do whites, asians, hispanics and blacks earn and attend? Top 10. 

#3) Black male vs female degree attainment?
#4) Is there a normalizing factor? If 300 of 700 whites go to high carnegie and major in stem, what do i do for the 17 blacks that earn a degree? Is it just a ratio or is there a normalizing factor to better compare?
#5) Analyze the followng from the Top ten institutions with black attendance:
#Composition of degree CIPS and top 10, male vs female degree attainment at these institutions and vs !these institutions.
#6) STEM demographic breakdown:
# Do the top black institutions have STEM cips?
# What is the racial breadkwon of BS. and PHD and MS stem CIPs
# Black male vs female STEM CIPs

## Color palete
pal = c("salmon", "turquoise1", "purple", "olivedrab4","red","deeppink","grey","black","blue","green","yellow")
# Later, in order to filter this analysis to only degrees in STEM,
# Make a vector containing STEM CIP codes used for filtering.
STEM_CIPs = read.csv("STEM_CIP_codes.csv") %>% pull(CIP_Code)
# 0-Census demographics ---------------------------------------------------

# In order to compare degree demographics to what we see in the US census, create numbers from the US census. 
#Later, Add the us Census as an outgroupping to compare
US_Census_df = read_xlsx("~/Dropbox/R_general_working_directory/R-workshop/CensusDemographics_2020.xlsx") %>% #read in table
  mutate(var_total = value * 335000000) %>% # use population total to add a raw count
  mutate(Award = "Census") %>% 
  select(Award, IPEDS_variable, var_total, value) %>% #reorder
  rename("var_frac" = value, #rename
         "variable" = IPEDS_variable)
US_Census_df %>% 
  ggplot(., aes(x = Award, y= var_total, fill = variable)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "", x = "") +
  theme_minimal()


# 0.1 Intro data wrangling -------------------

# Read in the Degree count data
degrees <- read_csv("" ) %>% # imput a file that should end with something like c2019_a.csv from IPEDS
  select(-starts_with("XC")) # load in the data, keep only the counts for each race.
#head(degrees) # degrees contains the universities unique identifier (UNITID) and the numbers of degrees awarded to different demographics (starts_with(C)) per major (CIPCODE), and the degree awarded (AWLEVEL).

# The first question is the racial breakdown of people earning degrees. The first thing I need to look at is what are the different types of degrees
# use the distinct function to determine the different award levels
# What are the different award levels?
degrees %>% 
  distinct(AWLEVEL)
#Because the award levels are coded as digits, not actual words like "bachelor's degree" I'll need to join it to a key.
#Read in the excel format AWARD and CIP code key.
award_code = readxl::read_excel("", sheet = "Frequencies") #Imput a file that should end with c2019_a.xlsx from IPEDS
#Make a key strictly for converting the award codes
awlevel_Key = award_code %>% 
  filter(str_detect(varname, "AWLEV")) %>% #filte the varname for awardlevel
  mutate(codevalue = str_pad(codevalue, width = 2, side = c("left"), pad = "0")) %>% #add a pad to the numbers so they will match with the degrees dataframe
  select(codevalue, valuelabel) %>% 
  mutate(Award = case_when(str_detect(valuelabel, "less than 1") ~"< 1 year",
                         str_detect(valuelabel, "at least 1") ~"1 year",
                         str_detect(valuelabel, "Associate's") ~"Associates",
                         str_detect(valuelabel, "at least 2") ~"2+ years, no degree", #either low information people or people like me
                         str_detect(valuelabel, "Bachelor's") ~"Bachelors",
                         str_detect(valuelabel, "Postbacc") ~"Postbac",
                         str_detect(valuelabel, "Master") ~"Masters",
                         str_detect(valuelabel, "Post-mast") ~"Post-Masters cert",
                         str_detect(valuelabel, "research") ~"PhD",
                         str_detect(valuelabel, "professional") ~"Doctor(MD, JD, etc.)",
                         str_detect(valuelabel, "other") ~"Doctor_other",
                         TRUE ~valuelabel)) %>% 
  select(-valuelabel)
# Looking at the award level key, you can see the various award levels recorded in IPEDS

## Back to our question: What is the demographic breakdown for each of the award levels?
# First, I will assess qualitatively using three plots: Stacked bar plot, Waffle plot, Heatmap. Pie chart? 
# Second, I will compare quantitqtively using Cluster analysis, NMDS, and PCA.

# To plot using ggplot, variables (races) need to be in long format.
degrees_long = degrees %>% 
  filter(CIPCODE %in% STEM_CIPs,
         MAJORNUM == "1") %>% 
  pivot_longer(cols = matches("C.+[WTM]"), names_to = "variable", values_to = "values") %>% #matches() is new for me. Relative to contains() (starts|ends)_with() but for columns
  filter(!str_detect(variable, "TOTAL")) #use regular expressions to remove the totals

degrees_long %>% 
  filter(str_detect(variable, "^C.+T$")) %>% 
  pull(values) %>% sum() #there are about 5,000,000 people in this dataset
#Make an attribute key for race and sex
Attribute_key = degrees_long %>% 
  distinct(variable) %>% 
  mutate(race = case_when(str_detect(variable, "2MOR") ~"MultiRacial",
                          str_detect(variable, "AIAN") ~"NativeAmerican",
                          str_detect(variable, "ASIA") ~"Asian",
                          str_detect(variable, "BK") ~"Black",
                          str_detect(variable, "HISP") ~"Hispanic",
                          str_detect(variable, "NHP") ~"PacificIslander",
                          str_detect(variable, "NRA") ~"NonResident",
                          str_detect(variable, "UNK") ~"UnknownRace",
                          str_detect(variable, "WHIT") ~"White",
                          TRUE ~variable),
         sex = case_when(str_detect(variable, ".+W") ~ "Woman",
                         str_detect(variable, ".+M") ~ "Male",
                         str_detect(variable, ".+T") ~ "Total",
                         TRUE ~variable)) %>%
  unite(combined, race, sex, sep = "_", remove = FALSE)

# 0.2) Number of different awards -----------------------------------------

## Create the dataframe from which I will make my plots and tables.
#Summarize the counts for all races (the variables) across all institutions (UNITID) and areas of study (CIPCODE)
df_toplot = degrees_long %>% 
  filter(str_detect(variable, regex(".+T$"))) %>% #Filter for the totals for each variable, excludig the Male vs Female counts
  group_by(AWLEVEL, variable) %>% # group by unique award level - race pairs
  summarise(var_total = sum(values)) %>% # summarize by tallying across the institutions and areas of study
  mutate(var_frac = var_total/sum(var_total)) %>% # use a separate mutate statement to get the fraction
  left_join(., awlevel_Key, by = c("AWLEVEL" = "codevalue")) %>% # Join with the award level key.
  ungroup() %>% # ungroup the data
  select(Award, matches(regex("VAR", ignore_case = TRUE))) %>% #the matches function allows the use of regular expression searches in a column, not an exact match such as does the contains() function.
  mutate(Award = factor(Award, levels = rev(c("Doctor(MD, JD, etc.)", "Doctor_other", "PhD", "Post-Masters cert", "Masters", "Postbac", "Bachelors", "2+ years, no degree", "Associates", "1 year", "< 1 year")))) %>% #set a specific order for the plot
  left_join(., Attribute_key)

## How many degrees were given for each of the carnegie categories?
df_toplot %>% 
  group_by(Award) %>% 
  summarise(total = sum(var_total)) %>% 
  arrange(desc(total))

## the first plot shows the overall numbers at the differnt award levels
df_toplot %>% 
  filter(Award != "Doctor_other") %>% 
  mutate(Award = factor(Award, levels = c("Bachelors", "Associates", "Masters", "< 1 year", "1 year", "Doctor(MD, JD, etc.)", "PhD", "Postbac", "2+ years, no degree", "Post-Masters cert", "Doctor_other"))) %>% 
  ggplot(., aes(y = Award, x = var_total)) + 
  geom_bar(stat = "identity") +
  labs(y = "",
       x = "Individuals") +
  theme_minimal()


# The first plot: Stacked barplot illustrating the number of awards earned at each level and the proportion of 9 "racial" categories at each award level.
df_toplot %>% 
  #bind_rows(US_Census %>% select(Award, IPEDS_variable, value) %>% rename("var_frac" = value, "variable" = IPEDS_variable)) %>% 
  #mutate(Award = factor(Award, levels = rev(c("Doctor(MD, JD, etc.)", "Doctor_other", "PhD", "Post-Masters cert", "Masters", "Postbac", "Bachelors", "2+ years, no degree", "Associates", "1 year", "< 1 year", "Census")))) %>% 
  ggplot(., aes(y = Award, x = var_total, fill = race)) + 
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(name = "",
                    values = turbo(10)) +
  labs(y = "",
       x = "Individuals") +
  theme_minimal()

## Summarize: Although everything in interesing, distill the large dataset by filtering out rare awards and races

DF = df_toplot %>% 
  #bind_rows(US_Census %>% select(Award, IPEDS_variable, value) %>% rename("var_frac" = value, "variable" = IPEDS_variable)) %>% 
  #mutate(Award = factor(Award, levels = rev(c("Doctor(MD, JD, etc.)", "Doctor_other", "PhD", "Post-Masters cert", "Masters", "Postbac", "Bachelors", "2+ years, no degree", "Associates", "1 year", "< 1 year", "Census")))) %>% 
  #mutate(data.agg = ifelse(str_detect(Award, "year"), "1-2 years no degree", Award)) %>% distinct(data.agg)
  filter(Award %in% c("PhD", "Masters", "Bachelors", "Associates")) %>% 
  group_by(race) %>% 
  summarise(val = sum(var_total)) %>% 
  filter(!race %in% c("MultiRacial", "PacificIslander","UnknownRace")) %>% 
  mutate(RACE = factor(race, levels = c("NativeAmerican","Black","Asian","Hispanic","NonResident","White"))) %>% 
  ungroup() %>% 
  arrange(desc(val)) 

color_pal = c("#8c6510","#324E2A","#0F261F","#E6ADCF","#DE4500","#65A98F")
names(color_pal) = DF %>% pull(RACE)

DF %>% 
  ggplot(., aes(values = val, fill = RACE)) +
  geom_waffle(make_proportional = T) +
  scale_fill_manual(name = "",
                    values = color_pal) +
  labs(y = "",
       x = "Proportion") +
  theme_void()


  
#Looking at this plot it seems that In order to compare via multivariate analysis, I should find some way to normalize.
#There is a huge disparity of the number of awardees at each level. I will need to normalize.
# Lets look at the numbers via table
df_toplot %>% 
  group_by(Award) %>% 
  summarise(Award_total = sum(var_total)) %>% 
  ungroup() %>% 
  mutate(Award_frac = Award_total/sum(Award_total)) %>% 
  arrange(desc(Award_total)) #%>% 
  pull(Award_total) %>% summary()
# The raw numbers say that I will need to remove some things: Doctor_other for starters, and can aggregate others after they are independently analyzed for a supplimental.

ASV_style_df = df_toplot %>% 
  dcast(variable~Award, value.var = "var_total") %>% 
  select(-Doctor_other) %>% 
  column_to_rownames("variable")

colSums(ASV_style_df)

cutoff = 31382
## I want to leave this in to show there is always a better way to do things. The bruit force vs other.

# #1) What is the racial breakdown of people earning the different --------

#Here I rarify but dont rotate
rare_df = rrarefy(t(ASV_style_df %>% select("Associates", "Bachelors", "Masters", "PhD")), cutoff) %>%
  data.frame(.)
#Note it would be easy to add a row for the Census if necessary after selecting out the races not present in the census

rare_to_plot = rare_df %>% 
  rownames_to_column("Award") %>% 
  pivot_longer(cols = starts_with("C"), names_to = "var", values_to = "val")

#Plot the distributions of awards. Note after rarifying, we lose the information that there are more bachelors. This is why it will be reflected in a simple barplot prior to normalizing.
rare_to_plot %>% 
  mutate(data.agg = ifelse(str_detect(Award, "year"), "1-2 years no degree", Award)) %>% 
  group_by(data.agg, var) %>% 
  summarise(val = sum(val)) %>% 
  filter(!str_detect(data.agg, "Postbac")) %>% 
  left_join(., Attribute_key, by = c("var" = "variable")) %>% 
  mutate(data.agg = factor(data.agg, levels = rev(c("Doctor(MD, JD, etc.)", "PhD", "Post-Masters cert", "Masters", "Bachelors", "Associates", "1-2 years no degree")))) %>% 
  filter(data.agg %in% c("PhD", "Masters", "Bachelors", "Associates"),
         !race %in% c("MultiRacial", "PacificIslander","UnknownRace")) %>% 
  ggplot(., aes(y = data.agg, x = val, fill = race)) +
  geom_bar(stat = "identity", position = "fill") +
  #scale_fill_brewer(type = "qual", palette = "Set3") +
  #scale_fill_manual(name = "", values = rev(c("#7A0403FF", "#CB2A04FF", "#F66B19FF", "#C7EF34FF", "grey",  "#72FE5EFF", "#1AE4B6FF", "#36AAF9FF", "#4662D7FF", "#30123BFF"))) +
  scale_fill_manual(name = "",
                    values = jisho_picker("vol2.030")) +
  labs(y = "",
       x = "Proportion") +
  theme_minimal() 

#End.