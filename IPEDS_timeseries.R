rm(list = ls())
options(scipen = 999)
library(tidyverse)

#function to download a timeseries range of IPEDS data
Download_Degree_Data = function(Beginning,End,Folder_Name) {
  #This function will download raw data from IPEDS within a specified date range.
  Year_range = as.character(Beginning:End)
  temp_staging = tempfile()
  for(i in Year_range){
    URL = paste("https://nces.ed.gov/ipeds/datacenter/data/C",i,"_A.zip", sep = "")
    print(URL)
    target_file = paste("c",i,"_a.csv", sep = "")
    download.file(url = URL, destfile = temp_staging)
    unzip(zipfile = temp_staging, files = target_file, exdir = Folder_Name)
    Sys.sleep(5.0)
  }
  unlink(temp_staging)
}

Process_Degree_DF = function(FILE) {
  #This function will load IPEDS data into R and quality trim it for the data of interest before returning a Dataframe.
  YEAR = FILE %>% str_replace("DownloadedDegrees/c", "") %>% str_replace("_a.csv", "")
  degrees <- read_csv(FILE) %>% 
    mutate(Year = YEAR) %>% 
    select(-starts_with("XC")) %>% 
    filter(CIPCODE != 99,
           MAJORNUM == 1,
           AWLEVEL %in% c("03", "05", "07", "17")) %>% 
    pivot_longer(cols = matches("C.+[WTM]"), names_to = "variable", values_to = "values") %>% #matches() is new for me. Relative to contains() (starts|ends)_with() but for columns
    filter(!str_detect(variable, "TOTAL"))
  return(degrees)
}

plot_series = function(DF, RACE){
  DF %>% 
    group_by(AWLEVEL, Year) %>% 
    mutate(TOTAL = sum(values)) %>% #Calculate the total number of degrees for each award level for each year. This will serve as the number to get a fraction.
    ungroup() %>% 
    group_by(AWLEVEL, Year, variable) %>% #print(n=25)
    summarise(TTl = sum(values),
              sum_frac = sum(values/TOTAL)) %>% #think this through. Shoul it be the mean?
    ungroup() %>%
    filter(variable %in% RACE) %>% #return()
    #filter(variable == "CBKAAT")
  left_join(., Level_codes, by = c("AWLEVEL"="codevalue")) %>% 
    mutate(valuelabel = case_when(str_detect(valuelabel, "Doctor") ~"PhD",
                                  TRUE ~valuelabel),
           valuelabel = factor(valuelabel, levels = c("Associate's degree", "Bachelor's degree", "Master's degree", "PhD"))) %>% 
    ggplot(., aes(x = as.numeric(Year), y = sum_frac, linetype = valuelabel, color = variable, shape = variable)) +
    geom_point(size = 1, show.legend = F) +
    geom_line(size = .6) +
    scale_x_continuous(breaks = Year_range) +
    scale_color_manual(name = "Race",
                       values = rev(jisho_picker("vol2.064", randomize = F))) +
    labs(y = "Fraction of degrees",
         x = "Year") +
    theme_minimal()
}



  Year_range = 2010:2022 #make a vector containing the years
  Data_directory = "DownloadedDegrees" #create a name for the directory the data will be saved
  filearray = paste(Data_directory,"/","c",Year_range,"_a.csv", sep = "") #Make an array containing the filenames (that will be used to read into R) using this informaton 
  Download_Degree_Data(Year_range[1],Year_range[length(Year_range)], Data_directory) #Run the function to download the datatables from IPEDS
  degree_df = map_dfr(filearray[2:length(filearray)], Process_Degree_DF) %>% filter(!str_detect(variable, "C.+T$")) #use a map statement to import and compile all downloaded data.

  
  #example of what you can do with the degree_df
  degree_df %>% 
    filter(CIPCODE %in% STEM_CIPs) %>% 
    select(AWLEVEL, Year, variable, values) %>% 
    group_by(AWLEVEL, Year) %>% 
    mutate(total = sum(values),
           frac = values/ sum(values)) %>% 
    group_by(AWLEVEL, Year, variable) %>% 
    summarise(sum_frac = sum(frac)) %>% 
    filter(variable == "CBKAAT") %>% 
    left_join(., Level_codes, by = c("AWLEVEL"="codevalue")) %>% 
    left_join(., Attribute_key) %>% 
    mutate(valuelabel = case_when(str_detect(valuelabel, "Doctor") ~"PhD",
                                  TRUE ~valuelabel),
           valuelabel = factor(valuelabel, levels = c("Associate's degree", "Bachelor's degree", "Master's degree", "PhD"))) %>% 
    ggplot(., aes(x = as.numeric(Year), y = sum_frac, color = valuelabel)) +
    geom_point(size = 1) +
    geom_line(size = .4) +
    scale_x_continuous(breaks = Year_range) +
    scale_color_manual(name = "",
                       values = rev(jisho_picker("vol2.064", randomize = F))) +
    labs(y = "Fraction of U.S. Degrees",
         x = "Year") +
    theme_minimal() +
    #facet_wrap(~race) +
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1, vjust = 1))
