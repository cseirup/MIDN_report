#Testing out forestTrend package loess functions for use generating 2022 MIDN report

#devtools::install_github("katemmiller/forestMIDN") 
#install.packages("fANCOVA")
library(tidyverse)
library(forestTrends)
library(forestMIDN)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

#Generate practice dataset: stocking index
#importData()
#exportCSV(zip = TRUE)
importCSV(path = 'C:/01_NETN/Forest_Health/R_Dev/MIDN_report', zip_name = 'MIDN_Forest_20211215.zip')
joinLocEvent()
rgn <- joinRegenData(speciesType = "native", canopyForm = "canopy", units = "sq.m", QAQC = FALSE)

names(rgn)
sort(unique(rgn$StartYear))
unique(rgn$ParkSubUnit)
length(unique(rgn$PlotCode)) # 375 total MIDN plots

#by year
rgn_sum <- rgn %>% group_by(ParkSubUnit, Plot_Name, PlotCode, StartYear) %>% 
  summarise(sd15_30 = sum(seed_15_30cm, na.rm = TRUE), 
            sd30_100 = sum(seed_30_100cm, na.rm = TRUE), 
            sd100_150 = sum(seed_100_150cm, na.rm = TRUE), 
            sd150p = sum(seed_p150cm, na.rm = TRUE),
            sap = sum(sap_den, na.rm = TRUE),
            stock = sum(stock, na.rm = TRUE))

#by cycle
rgn_sumC <- rgn %>% group_by(ParkSubUnit, Plot_Name, PlotCode, cycle) %>% 
  summarise(sd15_30 = sum(seed_15_30cm, na.rm = TRUE), 
            sd30_100 = sum(seed_30_100cm, na.rm = TRUE), 
            sd100_150 = sum(seed_100_150cm, na.rm = TRUE), 
            sd150p = sum(seed_p150cm, na.rm = TRUE),
            sap = sum(sap_den, na.rm = TRUE),
            stock = sum(stock, na.rm = TRUE))
table(rgn_sumC$cycle)

#attempt loess by year
span <- 8/diff(range(rgn_sum$StartYear))
span #.5714

nest <- rgn_sum %>% arrange(Plot_Name, StartYear) %>%
  mutate(grp = ParkSubUnit) %>% group_by(ParkSubUnit) %>% nest()

boot_all <- nest %>% mutate(
  model = map(data, ~case_boot_loess(., x = "StartYear", y = "stock", ID = "Plot_Name",
                                     span = 0.5714, group = "grp", #degree = 2,
                                     num_reps = 1000, chatty = TRUE)))

boot_results <- boot_all %>% select(ParkSubUnit, model) %>% unnest(model) %>% na.omit() #drop ASIS and SAHI

plot_trend_response(boot_results, xlab = "StartYear", ylab = "Stocking Index",
                    model_type = 'loess', ribbon = TRUE, group = 'ParkSubUnit', facet_scales = "fixed",
                    sign_color = c("#D3D3D3", "#696969", "#228B22", "#CD5C5C")) #+
 # scale_x_continuous(breaks = c(2008, 2012, 2016, 2020), labels = c("2008", "2012", "2016", "2020"))

#attempt loess by cycle
span <- 8/diff(range(rgn_sumC$cycle))
span #2.6667
nestC <- rgn_sumC %>% arrange(Plot_Name, cycle) %>%
  mutate(grp = ParkSubUnit) %>% group_by(ParkSubUnit) %>% nest()

boot_all <- nestC %>% mutate(
  model = map(data, ~case_boot_loess(., x = "cycle", y = "stock", ID = "Plot_Name",
                                     span = 2.6667, group = "grp", #degree = 2,
                                     num_reps = 1000, chatty = TRUE)))

boot_resultsC <- boot_all %>% select(ParkSubUnit, model) %>% unnest(model) %>% na.omit() #drop ASIS and SAHI

plot_trend_response(boot_resultsC, xlab = "cycle", ylab = "Stocking Index",
                    model_type = 'loess', ribbon = TRUE, group = 'ParkSubUnit', facet_scales = "fixed",
                    sign_color = c("#D3D3D3", "#696969", "#228B22", "#CD5C5C")) #+
# scale_x_continuous(breaks = c(2008, 2012, 2016, 2020), labels = c("2008", "2012", "2016", "2020"))


