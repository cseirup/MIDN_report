#Testing out best way to compile data by cycle for MIDN 2022 report. Would like to set correct last4yrs as fourth cycle

#install.packages("htmltools")
library(tidyverse)
library(forestTrends)
library(forestMIDN)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

# Import Data -------------------------------------------------------------
#Using .csvs while views change
importCSV(path = 'C:/01_NETN/Forest_Health/R_Dev/MIDN_report', zip_name = 'MIDN_Forest_20211215.zip')

# Cycle and last 4 year information
Group1 <- c("FRSP", "RICH", "PETE", "GEWA", "THST")# last four years 2018 - 2021
Group2 <- c("VAFO", "HOFU", "GETT", "APCO", "BOWA", "COLO", "SAHI", "ASIS")# last four years 2016-2019
G1_last4 <- 2018:2021
G2_last4 <- 2016:2019
cycle_names <- c("Cycle 1", "Cycle 2", "Cycle 3", "Last4yrs")

# Regen metrics: join data and calculate last 4 years ---------------------
#join parks and all years for C1, C2, and C3 data
rgn <- joinRegenData(speciesType = "native", canopyForm = "canopy", units = "sq.m", QAQC = FALSE)

#need to remove years from the incomplete last cycle: C4 for everyone but COLO - C3
colo <- rgn %>% filter(ParkUnit == "COLO") %>% filter(cycle == 3)
rgn2 <- rgn %>% filter(cycle != 4) 
rgn3 <- anti_join(rgn2, colo)

#join subset that will be the last 4 years
rgn_g1 <- joinRegenData(park = c("FRSP", "RICH", "PETE", "GEWA", "THST"),
                        from = 2018, to = 2021, speciesType = "native", 
                        canopyForm = "canopy", units = "sq.m", QAQC = FALSE) 

rgn_g2<- joinRegenData(park = c("VAFO", "HOFU", "GETT", "APCO", "BOWA", "COLO", "SAHI", "ASIS"),
                        from = 2016, to = 2019, speciesType = "native", 
                        canopyForm = "canopy", units = "sq.m", QAQC = FALSE)

rgn_g1$cycle[rgn_g1$ParkUnit %in% Group1 & rgn_g1$StartYear %in% G1_last4] <- 4# had to use 4 because loess needs numeric?
rgn_g2$cycle <- 4 #should be last4yr

#combine all datasets together
rgn4 <- rbind(rgn3, rgn_g1, rgn_g2)

#Prep regen data by cycle with last4yr
rgn_sumC <- rgn4 %>% group_by(ParkSubUnit, Plot_Name, PlotCode, cycle) %>% 
  summarise(sd15_30 = sum(seed_15_30cm, na.rm = TRUE), 
            sd30_100 = sum(seed_30_100cm, na.rm = TRUE), 
            sd100_150 = sum(seed_100_150cm, na.rm = TRUE), 
            sd150p = sum(seed_p150cm, na.rm = TRUE),
            sap = sum(sap_den, na.rm = TRUE),
            seed = sd15_30 + sd30_100 + sd100_150 + sd150p,
            stock = sum(stock, na.rm = TRUE))
table(rgn_sumC$cycle)

span <- 8/diff(range(rgn_sumC$cycle))
span #2.6667
nestC <- rgn_sumC %>% arrange(Plot_Name, cycle) %>%
  mutate(grp = ParkSubUnit) %>% group_by(ParkSubUnit) %>% nest()

#Prep regen data by year
rgn_sum <- rgn %>% group_by(ParkSubUnit, Plot_Name, PlotCode, StartYear) %>% 
  summarise(sd15_30 = sum(seed_15_30cm, na.rm = TRUE), 
            sd30_100 = sum(seed_30_100cm, na.rm = TRUE), 
            sd100_150 = sum(seed_100_150cm, na.rm = TRUE), 
            sd150p = sum(seed_p150cm, na.rm = TRUE),
            seed = sd15_30 + sd30_100 + sd100_150 + sd150p,
            sap = sum(sap_den, na.rm = TRUE),
            stock = sum(stock, na.rm = TRUE))

span <- 8/diff(range(rgn_sum$StartYear))
span #.5714

nest <- rgn_sum %>% arrange(Plot_Name, StartYear) %>%
  mutate(grp = ParkSubUnit) %>% group_by(ParkSubUnit) %>% nest()

# Stocking index by cycle -------------------------------------------------
boot_all <- nestC %>% mutate(
  model = map(data, ~case_boot_loess(., x = "cycle", y = "stock", ID = "Plot_Name",
                                     span = 2.6667, group = "grp", #degree = 2,
                                     num_reps = 1000, chatty = TRUE)))

boot_resultsC <- boot_all %>% select(ParkSubUnit, model) %>% unnest(model) %>% na.omit() #drop ASIS and SAHI

stockC <- plot_trend_response(boot_resultsC, xlab = "cycle", ylab = "Stocking Index",
                              model_type = 'loess', ribbon = TRUE, group = 'ParkSubUnit', 
                              facet_scales = "fixed", sign_color = c("#D3D3D3", "#696969", "#228B22", "#CD5C5C"))+
                              scale_x_continuous(breaks = c(1, 2, 3, 4), labels = cycle_names)+
                              theme(axis.text.x = element_text(angle = 45, hjust = 1))

stockC

# Stocking index by year --------------------------------------------------
boot_all <- nest %>% mutate(
  model = map(data, ~case_boot_loess(., x = "StartYear", y = "stock", ID = "Plot_Name",
                                     span = 0.5714, group = "grp", #degree = 2,
                                     num_reps = 1000, chatty = TRUE)))

boot_results <- boot_all %>% select(ParkSubUnit, model) %>% unnest(model) %>% na.omit() #drop ASIS and SAHI

stockY <- plot_trend_response(boot_results, xlab = "StartYear", ylab = "Stocking Index",
                    model_type = 'loess', ribbon = TRUE, group = 'ParkSubUnit', facet_scales = "fixed",
                    sign_color = c("#D3D3D3", "#696969", "#228B22", "#CD5C5C")) +
                    scale_x_continuous(breaks = c(2007, 2011, 2015, 2019), labels = c("2007", "2011", "2015", "2019"))
stockY

# Sapling density by cycle -----------------------------------------------

boot_all <- nestC %>% mutate(
  model = map(data, ~case_boot_loess(., x = "cycle", y = "sap", ID = "Plot_Name",
                                     span = 2.6667, group = "grp", #degree = 2,
                                     num_reps = 1000, chatty = TRUE)))

boot_resultsC <- boot_all %>% select(ParkSubUnit, model) %>% unnest(model) %>% na.omit() #drop ASIS and SAHI

sapC <- plot_trend_response(boot_resultsC, xlab = "cycle", ylab = "Sapling Density (stems/sq.m)",
                              model_type = 'loess', ribbon = TRUE, group = 'ParkSubUnit', 
                              facet_scales = "fixed", sign_color = c("#D3D3D3", "#696969", "#228B22", "#CD5C5C"))+
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = cycle_names)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sapC

# Sapling density by year --------------------------------------------------
boot_all <- nest %>% mutate(
  model = map(data, ~case_boot_loess(., x = "StartYear", y = "sap", ID = "Plot_Name",
                                     span = 0.5714, group = "grp", #degree = 2,
                                     num_reps = 1000, chatty = TRUE)))

boot_results <- boot_all %>% select(ParkSubUnit, model) %>% unnest(model) %>% na.omit() #drop ASIS and SAHI

sapY <- plot_trend_response(boot_results, xlab = "StartYear", ylab = "Sapling Density (stems/sq.m)",
                              model_type = 'loess', ribbon = TRUE, group = 'ParkSubUnit', facet_scales = "fixed",
                              sign_color = c("#D3D3D3", "#696969", "#228B22", "#CD5C5C")) +
  scale_x_continuous(breaks = c(2007, 2011, 2015, 2019), labels = c("2007", "2011", "2015", "2019"))
sapY

# Seedling density by cycle -----------------------------------------------

boot_all <- nestC %>% mutate(
  model = map(data, ~case_boot_loess(., x = "cycle", y = "seed", ID = "Plot_Name",
                                     span = 2.6667, group = "grp", #degree = 2,
                                     num_reps = 1000, chatty = TRUE)))

boot_resultsC <- boot_all %>% select(ParkSubUnit, model) %>% unnest(model) %>% na.omit() #drop ASIS and SAHI

seedC <- plot_trend_response(boot_resultsC, xlab = "cycle", ylab = "Seedling Density (stems/sq.m)",
                            model_type = 'loess', ribbon = TRUE, group = 'ParkSubUnit', 
                            facet_scales = "fixed", sign_color = c("#D3D3D3", "#696969", "#228B22", "#CD5C5C"))+
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = cycle_names)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

seedC

# Seedling density by year --------------------------------------------------
boot_all <- nest %>% mutate(
  model = map(data, ~case_boot_loess(., x = "StartYear", y = "seed", ID = "Plot_Name",
                                     span = 0.5714, group = "grp", #degree = 2,
                                     num_reps = 1000, chatty = TRUE)))

boot_results <- boot_all %>% select(ParkSubUnit, model) %>% unnest(model) %>% na.omit() #drop ASIS and SAHI

seedY <- plot_trend_response(boot_results, xlab = "StartYear", ylab = "Seedling Density (stems/sq.m)",
                            model_type = 'loess', ribbon = TRUE, group = 'ParkSubUnit', facet_scales = "fixed",
                            sign_color = c("#D3D3D3", "#696969", "#228B22", "#CD5C5C")) +
  scale_x_continuous(breaks = c(2007, 2011, 2015, 2019), labels = c("2007", "2011", "2015", "2019"))
seedY

# Seedling distribution by size class -------------------------------------
#only status aka last 4 years
names(rgn_sumC)
rgnC_long <- rgn_sumC %>% filter(cycle == 4) %>%
                select(ParkSubUnit, Plot_Name, PlotCode, cycle, sd15_30:sap) %>% 
                pivot_longer(cols = c(sd15_30:sap), names_to = "size_class",
                values_to = "density")

rgnC_size_class <- rgnC_long %>% group_by(ParkSubUnit, size_class) %>% 
                                 summarize(avg_dens = mean(density, na.rm = TRUE),
                                 se_dens = sd(density, na.rm = TRUE)/sqrt(sum(!is.na(density))),
                                 num_plots = sum(!is.na(density)))

rgnC_size_class$size_class <- ordered(rgnC_size_class$size_class,
                                     levels = c("sd15_30", "sd30_100", "sd100_150", "sd150p", "sap"))

rgnC_size_class <- rgnC_size_class %>% arrange(ParkSubUnit, size_class)

# Make ggplot graph
regen_plot <- ggplot(data = rgnC_size_class, aes(x = size_class, y = avg_dens))+
  geom_bar(stat = 'identity', fill = '#8FA7BE')+ #revised hexcode 7-30-20
  geom_errorbar(aes(ymin = avg_dens - se_dens, 
                    ymax = avg_dens + se_dens, x = size_class),
                color = "#696969", 
                width = 0.3, 
                size = 0.3, #added 7/13
                position = position_dodge(0.9))+
  facet_wrap(~ParkSubUnit, ncol = 5)+
  labs(x = "Regeneration Size Class (cm)", y = "stems/sq.m")+ #revised 7-30-20
  theme(axis.text = element_text(size = 6), 
        strip.text = element_text(size = 8), 
        axis.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "#696969", size = 0.01),
        axis.ticks = element_line(color = "#696969", size = 0.5),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        axis.title.y = element_text(margin = margin(r = 5)))+ #added 8/3/20
  scale_x_discrete(labels= c('15  \U2013 30cm', '30  \U2013 100cm', '100  \U2013 150cm','>150cm',
                             '1 \U2013 10cm DBH'))+ #revised 7/30/20
  theme_FVM()
print(regen_plot)

# Sapling diameter distribution -------------------------------------------
sumSapDBHDist <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE, locType = c('VS', 'all'), panels = 1:4,
                          speciesType = c('all', 'native','exotic', 'invasive'),
                          canopyForm = c("all", "canopy"), eventType = c('complete', 'all'),
                          units = c('density', 'BA', 'both'), ...){
  
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2007)
  stopifnot(class(to) == "numeric", to >= 2007)
  locType <- match.arg(locType)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  speciesType <- match.arg(speciesType)
  canopyForm <- match.arg(canopyForm)
  units <- match.arg(units)
  
  plot_events <- joinLocEvent(park = park, from = from, to = to, QAQC = QAQC, locType = locType,
                              eventType = eventType, panels = panels)
  
  sap_evs <- joinMicroSaplings(park = park, from = from, to = to, QAQC = QAQC, locType = locType,
                               eventType = eventType, panels = panels, speciesType = speciesType,
                               canopyForm = canopyForm, status = 'live')
  
  
  sap_evs <- sap_evs %>% mutate(size_class = case_when(between(DBHcm, 1, 1.9)~ 'd1_1.9',
                                                       between(DBHcm, 2, 2.9)~ 'd2_2.9',
                                                       between(DBHcm, 3, 3.9)~ 'd3_3.9',
                                                       between(DBHcm, 4, 4.9)~ 'd4_4.9',
                                                       between(DBHcm, 5, 5.9)~ 'd5_5.9',
                                                       between(DBHcm, 6, 6.9)~ 'd6_6.9',
                                                       between(DBHcm, 7, 7.9)~ 'd7_7.9',
                                                       between(DBHcm, 8, 8.9)~ 'd8_8.9',
                                                       between(DBHcm, 9, 9.9)~ 'd9_9.9',
                                                       TRUE ~ 'unknown'),
                                stem = ifelse(!is.na(DBHcm), 1, 0),
                                #unit_conv = (pi*3^2)*3,
                                BA_cm2 = round(pi*((DBHcm/2)^2),4))
  
  # Check for NA DBHcm
  sap_check <- sap_evs %>% filter(size_class == "unknown" & !is.na(TagCode))
  
  if(nrow(sap_check)>0){
    warning(paste("The", nrow(sap_check), "records below are missing DBH measurements and will be removed from summaries."),
            "\n",
            paste(capture.output(data.frame(sap_check[, c("Plot_Name", "StartYear", "TagCode")])), collapse = "\n"))
  }
  
  #sap_evs2 <- sap_evs %>% arrange(Plot_Name, StartYear, IsQAQC, size_class) %>% filter(!size_class %in% "unknown")
  
  sap_dist <- sap_evs %>% group_by(Plot_Name, ParkUnit, PlotID, EventID, StartYear, IsQAQC,
                                   size_class) %>%
    summarize(dens = ((sum(stem))*10000)/((pi*3^2)*3), #stems/ha
              BA = sum(BA_cm2)/((pi*3^2)*3), #m2/ha
              .groups = 'drop')  # BA already corrected for Deer Ex. in joinTreeData
  
  sap_dist_wide <- switch(units,
                          'density' = sap_dist %>% select(-BA) %>%
                            pivot_wider(names_from = size_class,
                                        values_from = dens,
                                        values_fill = 0,
                                        names_glue = "dens_{str_sub(size_class, 2)}"),
                          'BA' = sap_dist %>% select(-dens) %>%
                            pivot_wider(names_from = size_class,
                                        values_from = BA,
                                        values_fill = 0,
                                        names_glue = "BA_{str_sub(size_class, 2)}"),
                          'both' = sap_dist %>%
                            pivot_wider(names_from = size_class,
                                        values_from = c(dens, BA),
                                        values_fill = 0,
                                        names_glue = "{.value}_{str_sub(size_class, 2)}")
  )
  
  # next few lines find if a size class is missing, and adds it later
  sizes = switch(units,
                 'density' = c("dens_1_1.9", "dens_2_2.9", "dens_3_3.9", "dens_4_4.9",
                               "dens_5_5.9", "dens_6_6.9", "dens_7_7.9", "dens_8_8.9",
                               "dens_9_9.9"),
                 'BA' = c("BA_1_1.9", "BA_2_2.9", "BA_3_3.9", "BA_4_4.9",
                          "BA_5_5.9", "BA_6_6.9", "BA_7_7.9", "BA_8_8.9",
                          "BA_9_9.9"),
                 'both' = c("dens_1_1.9", "dens_2_2.9", "dens_3_3.9", "dens_4_4.9",
                            "dens_5_5.9", "dens_6_6.9", "dens_7_7.9", "dens_8_8.9",
                            "dens_9_9.9",
                            "BA_1_1.9", "BA_2_2.9", "BA_3_3.9", "BA_4_4.9",
                            "BA_5_5.9", "BA_6_6.9", "BA_7_7.9", "BA_8_8.9",
                            "BA_9_9.9")
  )
  
  
  missing_sizes <- setdiff(sizes, names(sap_dist_wide))
  
  sap_dist_wide[missing_sizes] <- 0
  
  sap_dist_final <- left_join(plot_events, sap_dist_wide,
                              by = intersect(names(plot_events), names(sap_dist_wide))) %>%
    select(Plot_Name, ParkUnit, ParkSubUnit, PlotID, EventID, StartYear, IsQAQC, cycle,
           all_of(sizes))
  
  
  return(data.frame(sap_dist_final))
  
} # end of function


sap_dist <- sumSapDBHDist(park = "all", status = 'live', speciesType = "native", 
                          canopyForm = c("canopy"), units = 'density', 
                          from = 2007, to = 2021)

#need to remove years from the incomplete last cycle: C4 for everyone but COLO - C3
colo <- sap_dist %>% filter(ParkUnit == "COLO") %>% filter(cycle == 3)
sap_dist2 <- sap_dist %>% filter(cycle != 4) 
sap_dist3 <- anti_join(sap_dist2, colo)

#join subset that will be the last 4 years
sap_dist_g1 <- sumSapDBHDist(park =  c("FRSP", "RICH", "PETE", "GEWA", "THST"), 
                             status = 'live', speciesType = "native", 
                             canopyForm = "canopy", units = 'density', 
                             from = 2018, to = 2021)


sap_dist_g2<- sumSapDBHDist(park =  c("VAFO", "HOFU", "GETT", "APCO", "BOWA", "COLO", "SAHI", "ASIS"), 
                            status = 'live', speciesType = "native", 
                            canopyForm = "canopy", units = 'density', 
                            from = 2016, to = 2019)

sap_dist_g1$cycle <- 4# had to use 4 because loess needs numeric?
sap_dist_g2$cycle <- 4 #should be last4yr

#combine all datasets together
sap_dist4 <- rbind(sap_dist3, sap_dist_g1, sap_dist_g2)

sap_dist_park <- sap_dist4 %>% select(ParkSubUnit, cycle, dens_1_1.9:dens_9_9.9) %>% 
                              group_by(ParkSubUnit, cycle) %>% 
                              summarise(across(starts_with("dens_"),
                                               ~mean(.x, na.rm = TRUE),
                                        .groups = 'drop'))

sap_dist_long <- sap_dist_park %>% select(ParkSubUnit, cycle, dens_1_1.9:dens_9_9.9) %>% 
                              pivot_longer(cols = starts_with("dens_"), names_to = "size_class",
                                           values_to = "density") 

sap_dist_long$cycle <- as.factor(sap_dist_long$cycle)                             
str(sap_dist_long)

sap_dist_plot <- ggplot(data = sap_dist_long, aes(color = cycle, x = size_class, y = density))+
                     #geom_point()+ 
                     geom_line(aes(group = cycle), size = 1)+
                     facet_wrap(~ParkSubUnit, ncol = 5)+
                     labs(x = "Sapling Diameter Class (cm)", y = "stems/ha")+ 
                     theme(axis.text = element_text(size = 9), # change axis label size
                           strip.text = element_text(size = 10), # change facet text size
                           axis.title = element_text(size = 12), # change axis title size
                           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                           axis.title.y = element_text(margin = margin(r = 5)),
                           legend.text = element_text(size = 12),
                           legend.title = element_text(size = 12),
                           legend.position = "right",
                           legend.justification = c(1,0))+
                     scale_color_manual(name = "cycle", labels = cycle_names, 
                                        values = c("#bae4b3", "#74c476", "#31a354", "#006d2c"))+
                     scale_x_discrete(labels = c("1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10"))+
                     theme_FVM() 

sap_dist_plot

# Deer Browse Index -------------------------------------------------------

std <- joinStandData(eventType = "complete")

#need to remove years from the incomplete last cycle: C4 for everyone but COLO - C3
colo <- std %>% filter(ParkUnit == "COLO") %>% filter(cycle == 3)
std2 <- std %>% filter(cycle != 4) 
std3 <- anti_join(std2, colo)

#join subset that will be the last 4 years
std_g1 <- joinStandData(park = c("FRSP", "RICH", "PETE", "GEWA", "THST"),
                        from = 2018, to = 2021, eventType = "complete") 

std_g2<- joinStandData(park = c("VAFO", "HOFU", "GETT", "APCO", "BOWA", "COLO", "SAHI", "ASIS"),
                       from = 2016, to = 2019, eventType = "complete")

std_g1$cycle <- 4 #had to use 4 because loess needs numeric?
std_g2$cycle <- 4 #should be last4yr

#combine all datasets together
std4 <- rbind(std3, std_g1, std_g2)
names(std4)
dbi <- std4 %>% select(ParkSubUnit, Plot_Name, cycle, Deer_Browse_Index, Deer_Browse_Label) %>% 
                group_by(ParkSubUnit, cycle, Deer_Browse_Index, Deer_Browse_Label) %>% 
                summarise(num_plots = n()) %>% drop_na() #DBI not collected until 2009
dbi2 <- dbi
str(dbi)
dbi$Deer_Browse_Index <- as.factor(dbi$Deer_Browse_Index)
dbi$cycle <- as.factor(dbi$cycle)

#stacked bar chart
unique(dbi$Deer_Browse_Label)
dbi_names <- c("3" = "Moderate", "4" ="High", "5" = "Very High", "2" = "Low Impact")
DBI_plot <- ggplot(dbi, aes(fill = Deer_Browse_Index, x=cycle, y=num_plots))+
                   geom_bar(stat='identity', position="fill")+
                   labs(x='Cycle', y='Percent of Plots')+ 
                   facet_wrap(~ParkSubUnit)+
                   scale_fill_manual(name = "Deer Browse Impact", labels = dbi_names,
                                     values = c( "#91cf60","#fee090","#d73027","#b2182b"))+
                   scale_x_discrete(labels = cycle_names)+
                   scale_y_continuous(labels = scales::percent)+
                   theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                   axis.text.y = element_text(size = 9),
                   axis.title.y = element_text(size = 12))+
                   theme_FVM()

DBI_plot

# Stocking Index Status ---------------------------------------------------
stk <- rgn_sumC %>% filter(cycle == 4) %>% 
                    group_by(ParkSubUnit, cycle) %>% 
                    summarise(mean_stock = mean(stock, na.rm = TRUE),
                              se_stock = sd(stock, na.rm = TRUE)/sqrt(sum(!is.na(stock))),
                              num_plots = n())

stkStatus <- ggplot(data = stk, aes(x = reorder(ParkSubUnit, -mean_stock), y = mean_stock))+
  geom_bar(stat = 'identity', fill = '#82B07A')+ 
  geom_errorbar(aes(ymin = mean_stock - se_stock, 
                    ymax = mean_stock + se_stock, x = ParkSubUnit),
                color = "#696969", 
                width = 0.3,
                size = 0.3,
                position = position_dodge(0.9))+
  labs(y = "Mean Stocking Index", x = "Park Unit")+ 
  theme(axis.text = element_text(size = 9), # change axis label size
        strip.text = element_text(size = 9), # change facet text size
        axis.title = element_text(size = 12), # change axis title size
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(margin = margin(r = 5)))+  
  geom_hline(yintercept = 1.989, lty = 2, lwd = 1, color = '#CD5C5C')+ 
  geom_hline(yintercept = 7.958, lty = 3, lwd = 1, color = '#0E5D2C')+ 
  annotate(geom="text", x = "ASIS", y = 2.6, label = "Severely Understocked", 
           colour='black')+ 
  annotate(geom="text", x = "ASIS", y = 8.5, label = "Sufficiently Stocked", 
                                      colour='black')+
  theme_FVM()

print(stkStatus)


# Stocking Index: percent of plots stocked (status) --------------------------------
Mdbi <- std4 %>% select(ParkSubUnit, Plot_Name, cycle, Deer_Browse_Index, Deer_Browse_Label) %>% 
                 filter(cycle == 4) %>% 
                 group_by(ParkSubUnit, cycle) %>% 
                 summarise(mean_DBI = mean(Deer_Browse_Index, na.rm = TRUE),
                           se_DBI = sd(Deer_Browse_Index, na.rm = TRUE)/sqrt(sum(!is.na(Deer_Browse_Index))),
                           num_plots = n()) %>% drop_na()
#all parks average >3 DBI so need stock = 7.958 (or 100) to be stocked

rgn_g1$cycle[rgn_g1$ParkUnit %in% Group1 & rgn_g1$StartYear %in% G1_last4] <- 4# had to use 4 because loess needs numeric?
rgn_g2$cycle <- 4 #should be last4yr

pStock <- rgn_sumC %>% select("ParkSubUnit", "Plot_Name", "PlotCode", "cycle", "stock") %>% 
                       filter(cycle == 4) %>% 
                       mutate(stk_YN = NA)
intersect(names(pStock), names(Mdbi))

pStkDBI <- left_join(pStock, Mdbi, by = c("ParkSubUnit", "cycle"))

pStkDBI$stk_YN[pStkDBI$stock >= 7.958] <- 1
pStkDBI$stk_YN[pStkDBI$stock >= 3.979 & pStkDBI$mean_DBI <= 3] <- 1
pStkDBI$stk_YN[pStkDBI$stock < 7.958 & pStkDBI$mean_DBI > 3] <- 0
pStkDBI$stk_YN[pStkDBI$stock < 3.979 & pStkDBI$mean_DBI <= 3] <- 0
table(pStkDBI$stk_YN)

names(pStkDBI)
pStkDBI2 <- pStkDBI %>% group_by(ParkSubUnit) %>% 
                        summarise(pStk = mean(stk_YN)*100,
                                  num_plots = n())
#plot
pStk_plot <- ggplot(data = pStkDBI2, aes(x = reorder(ParkSubUnit, -pStk), y = pStk))+
  geom_bar(stat = 'identity', fill = '#82B07A')+ 
  labs(y = "% Stocked Plots", x = "Park Unit")+ 
  theme(axis.text = element_text(size = 9), # change axis label size
        strip.text = element_text(size = 9), # change facet text size
        axis.title = element_text(size = 12), # change axis title size
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(margin = margin(r = 5)))+  
  geom_hline(yintercept = 25, lty = 2, lwd = 1, color = '#CD5C5C')+ 
  annotate(geom="text", x = "ASIS", y = 26.2, label = "Critical Threshold: 25% Stocked", 
           colour='black')+ 
  theme_FVM()

print(pStk_plot)

