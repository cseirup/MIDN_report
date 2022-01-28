#Testing out best way to compile data by cycle for MIDN 2022 report. Would like to set correct last4yrs as fourth cycle
#testing branch
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

# Live Native Tree Metrics: join data and generate last 4 years--------------------------------------------
#join parks and all years for C1, C2, and C3 data
joinTreeData <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE, locType = c('VS', 'all'), panels = 1:4,
                         status = c('all', 'active', 'live', 'dead'), speciesType = c('all', 'native','exotic', 'invasive'),
                         canopyPosition = c("all", "canopy"), dist_m = NA, eventType = c('complete', 'all'),
                         output = 'short', ...){
  
  # Match args and class
  status <- match.arg(status)
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2007)
  stopifnot(class(to) == "numeric", to >= 2007)
  locType <- match.arg(locType)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  output <- match.arg(output, c("short", "verbose"))
  eventType <- match.arg(eventType)
  status <- match.arg(status)
  canopyPosition <- match.arg(canopyPosition)
  speciesType <- match.arg(speciesType)
  
  
  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}
  
  # Prepare the tree data
  tryCatch(tree_vw <- get("COMN_TreesByEvent", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC, TreeLegacyID,
                    TagCode, TaxonID, TSN, ScientificName, Fork, Azimuth, Distance, DBHcm, IsDBHVerified,
                    IsDBHUnusual, TreeStatusCode, TreeStatusLabel, CrownClassCode, CrownClassLabel,
                    DecayClassCode, HWACode, HWALabel, BBDCode, BBDLabel, TreeEventNote),
           
           error = function(e){stop("COMN_TreesByEvent view not found. Please import view.")}
  )
  
  tryCatch(foliage_vw <- get("COMN_TreesFoliageCond", envir = env) %>%
             select(PlotID, EventID, ParkUnit, ParkSubUnit, PlotCode, StartYear, IsQAQC,
                    TreeLegacyID, TagCode, TotalFoliageConditionCode, TotalFoliageConditionLabel) %>%
             unique(),
           error = function(e){stop("COMN_TreeFoliageCond view not found. Please import view.")})
  
  taxa_wide <- prepTaxa()
  # subset with EventID from plot_events to make tree data as small as possible to speed up function
  plot_events <- force(joinLocEvent(park = park, from = from , to = to, QAQC = QAQC,
                                    panels = panels, locType = locType, eventType = eventType,
                                    abandoned = FALSE, output = 'short')) %>%
    select(Plot_Name, Network, ParkUnit, ParkSubUnit, PlotTypeCode, PanelCode, PlotCode, PlotID,
           EventID, StartDate, StartYear, cycle, IsQAQC)
  
  if(nrow(plot_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}
  
  pe_list <- unique(plot_events$EventID)
  
  tree_evs <- filter(tree_vw, EventID %in% pe_list)
  
  # Drop unwanted status
  alive <- c("1", "AB", "AF", "AL" ,"AM" ,"AS", "RB", "RF", "RL", "RS")
  dead <- c("2","DB" ,"DF" ,"DL", "DM","DS")
  active <- c(alive, dead, "DC") #inactive-old: 0, ES, EX, inactive-current: NL, PM, XO, XP, XS
  
  tree_stat <- switch(status,
                      'all' = tree_evs,
                      'active' = filter(tree_evs, TreeStatusCode %in% active),
                      'live' = filter(tree_evs, TreeStatusCode %in% alive),
                      'dead' = filter(tree_evs, TreeStatusCode %in% dead))
  
  # Drop unwanted events before merging
  tree_fol1 <- filter(foliage_vw, EventID %in% pe_list)
  tree_fol <- left_join(tree_stat, tree_fol1, by = intersect(names(tree_vw), names(foliage_vw)))
  
  tree_taxa <- left_join(tree_fol,
                         taxa_wide[,c('TSN','ScientificName','CommonName','Family', 'Genus', 'Exotic', "InvasiveMIDN")],
                         by = c("TSN", "ScientificName"))
  
  tree_taxa$BA_cm2 <- round(pi*((tree_taxa$DBHcm/2)^2),4)# basal area (cm^2)
  
  tree_taxa$BBDCode <- suppressWarnings(as.numeric(tree_taxa$BBDCode)) # drops PMs from column
  tree_taxa$HWACode <- suppressWarnings(as.numeric(tree_taxa$HWACode)) # drops PMs from column
  tree_taxa$CrownClassCode <- suppressWarnings(as.numeric(tree_taxa$CrownClassCode)) # drops PM/NC
  
  tree_taxa$DecayClassLabel <- ifelse(is.na(tree_taxa$DecayClassCode) |
                                        tree_taxa$DecayClassCode %in% c("PM", "NC"),
                                      paste0(tree_taxa$DecayClassLabel),
                                      paste0("Decay Class ", tree_taxa$DecayClassCode))
  tree_taxa$DecayClassCode <- suppressWarnings(as.numeric(tree_taxa$DecayClassCode))
  
  tree_taxa <- tree_taxa %>% mutate(Pct_Tot_Foliage_Cond = as.numeric(
    case_when(TotalFoliageConditionCode == "0" ~ 0,
              TotalFoliageConditionCode == "1" ~ 5.5,
              TotalFoliageConditionCode == "2" ~ 30,
              TotalFoliageConditionCode == "3" ~ 70,
              TotalFoliageConditionCode == "4" ~ 95,
              TotalFoliageConditionCode == "NC" ~ NA_real_,
              TRUE ~ NA_real_)),
    Txt_Tot_Foliage_Cond = TotalFoliageConditionLabel) %>%
    select(-TotalFoliageConditionCode, -TotalFoliageConditionLabel) # fix . after next release
  
  tree_nat <- switch(speciesType,
                     'all' = tree_taxa,
                     'native' = filter(tree_taxa, Exotic == FALSE),
                     'exotic' = filter(tree_taxa, Exotic == TRUE),
                     'invasive' = filter(tree_taxa, InvasiveMIDN == TRUE))
  
  tree_can <- switch(canopyPosition,
                     'all' = tree_nat,
                     'canopy' = tree_nat %>% filter(CrownClassCode %in% c(2, 3, 4)))
  
  tree_dist <- if(!is.na(dist_m)){filter(tree_can, Distance <= dist_m)
  } else {tree_can}
  
  tree_merge <- left_join(plot_events, tree_dist,
                          by = intersect(names(plot_events), names(tree_dist))) %>%
    arrange(Plot_Name, StartYear, IsQAQC, TagCode)
  
  # Handling plots with missing status or species specified.
  tree_merge$ScientificName <- ifelse(is.na(tree_merge$ScientificName), "None present", tree_merge$ScientificName)
  tree_merge$num_stems <- ifelse(is.na(tree_merge$TagCode), 0, 1) # for plots missing live or dead trees
  tree_merge$BA_cm2[is.na(tree_merge$TagCode)] <- 0 # for plots missing live or dead trees
  # Plots will have a record, but species, condition, DBH info will be NA.
  
  tree_final <- if(output == 'short'){
    tree_merge[, c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode", "cycle", "PlotCode",
                   "PlotID", "EventID", "IsQAQC", "StartYear", "StartDate", "TSN", "ScientificName",
                   "TagCode", "Fork", "Azimuth", "Distance", "DBHcm", "IsDBHVerified", "TreeStatusCode",
                   "CrownClassCode", "DecayClassCode", "Pct_Tot_Foliage_Cond",
                   "HWACode", "BBDCode", "BA_cm2", "num_stems", "TreeEventNote")]
  } else {tree_merge}
  #table(complete.cases(tree_merge[,intersect(names(plot_events), names(tree_dist))])) #All T
  
  return(data.frame(tree_final))
} # end of function



tree <- joinTreeData(status = "live", speciesType = "native")

#need to remove years from the incomplete last cycle: C4 for everyone but COLO - C3
colo <- tree %>% filter(ParkUnit == "COLO") %>% filter(cycle == 3)
tree2 <- tree %>% filter(cycle != 4) 
tree3 <- anti_join(tree2, colo)

#join subset that will be the last 4 years
tree_g1 <- joinTreeData(park = c("FRSP", "RICH", "PETE", "GEWA", "THST"),
                        from = 2018, to = 2021, speciesType = "native", 
                        QAQC = FALSE) 

tree_g2<- joinTreeData(park = c("VAFO", "HOFU", "GETT", "APCO", "BOWA", "COLO", "SAHI", "ASIS"),
                       from = 2016, to = 2019, speciesType = "native", 
                       QAQC = FALSE)

tree_g1$cycle <- 4# had to use 4 because loess needs numeric?
tree_g2$cycle <- 4 #should be last4yr

#combine all datasets together
tree4 <- rbind(tree3, tree_g1, tree_g2)
names(tree4)
#Prep tree basal area and density data by cycle with last4yr
tree_sumC <- tree4 %>% group_by(ParkSubUnit, Plot_Name, PlotCode, cycle) %>% 
                       summarise(sum_stems = sum(num_stems),
                                 sum_BA_cm2 = sum(BA_cm2))%>% 
                       mutate(num_stems_ha = sum_stems * 25, 
                              BA_m2ha = sum_BA_cm2/400)

table(tree_sumC$cycle)

span <- 8/diff(range(tree_sumC$cycle))
span #2.6667
nestC <- tree_sumC %>% arrange(Plot_Name, cycle) %>%
  mutate(grp = ParkSubUnit) %>% group_by(ParkSubUnit) %>% nest()

#Prep regen data by year
tree_sum <- tree %>% group_by(ParkSubUnit, Plot_Name, PlotCode, StartYear) %>% 
                     summarise(sum_stems = sum(num_stems),
                               sum_BA_cm2 = sum(BA_cm2))%>% 
                     mutate(num_stems_ha = sum_stems * 25, 
                            BA_m2ha = sum_BA_cm2/400)

span <- 8/diff(range(tree_sum$StartYear))
span #.5714

nest <- tree_sum %>% arrange(Plot_Name, StartYear) %>%
  mutate(grp = ParkSubUnit) %>% group_by(ParkSubUnit) %>% nest()

# Basal area by cycle -------------------------------------------------
boot_all <- nestC %>% mutate(
  model = map(data, ~case_boot_loess(., x = "cycle", y = "BA_m2ha", ID = "Plot_Name",
                                     span = 2.6667, group = "grp", #degree = 2,
                                     num_reps = 1000, chatty = TRUE)))

boot_resultsC <- boot_all %>% select(ParkSubUnit, model) %>% unnest(model) %>% na.omit() #drop ASIS and SAHI

TreeBA_C <- plot_trend_response(boot_resultsC, xlab = "cycle", ylab = "Live Tree Basal Area",
                              model_type = 'loess', ribbon = TRUE, group = 'ParkSubUnit', 
                              facet_scales = "fixed", sign_color = c("#D3D3D3", "#696969", "#228B22", "#CD5C5C"))+
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = cycle_names)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

TreeBA_C

# Basal area by year --------------------------------------------------
boot_all <- nest %>% mutate(
  model = map(data, ~case_boot_loess(., x = "StartYear", y = "BA_m2ha", ID = "Plot_Name",
                                     span = 0.5714, group = "grp", #degree = 2,
                                     num_reps = 1000, chatty = TRUE)))

boot_results <- boot_all %>% select(ParkSubUnit, model) %>% unnest(model) %>% na.omit() #drop ASIS and SAHI

TreeBA_Y <- plot_trend_response(boot_results, xlab = "StartYear", ylab = "Live Tree Basal Area",
                              model_type = 'loess', ribbon = TRUE, group = 'ParkSubUnit', facet_scales = "fixed",
                              sign_color = c("#D3D3D3", "#696969", "#228B22", "#CD5C5C")) +
  scale_x_continuous(breaks = c(2007, 2011, 2015, 2019), labels = c("2007", "2011", "2015", "2019"))
TreeBA_Y

# Tree Density by cycle -------------------------------------------------
boot_all <- nestC %>% mutate(
  model = map(data, ~case_boot_loess(., x = "cycle", y = "num_stems_ha", ID = "Plot_Name",
                                     span = 2.6667, group = "grp", #degree = 2,
                                     num_reps = 1000, chatty = TRUE)))

boot_resultsC <- boot_all %>% select(ParkSubUnit, model) %>% unnest(model) %>% na.omit() #drop ASIS and SAHI

TreeDen_C <- plot_trend_response(boot_resultsC, xlab = "cycle", ylab = "Live Tree Density (stems/ha)",
                              model_type = 'loess', ribbon = TRUE, group = 'ParkSubUnit', 
                              facet_scales = "fixed", sign_color = c("#D3D3D3", "#696969", "#228B22", "#CD5C5C"))+
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = cycle_names)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

TreeDen_C

# Tree Density by year --------------------------------------------------
boot_all <- nest %>% mutate(
  model = map(data, ~case_boot_loess(., x = "StartYear", y = "num_stems_ha", ID = "Plot_Name",
                                     span = 0.5714, group = "grp", #degree = 2,
                                     num_reps = 1000, chatty = TRUE)))

boot_results <- boot_all %>% select(ParkSubUnit, model) %>% unnest(model) %>% na.omit() #drop ASIS and SAHI

TreeDen_Y <- plot_trend_response(boot_results, xlab = "StartYear", ylab = "Live Tree Density (stems/ha)",
                              model_type = 'loess', ribbon = TRUE, group = 'ParkSubUnit', facet_scales = "fixed",
                              sign_color = c("#D3D3D3", "#696969", "#228B22", "#CD5C5C")) +
  scale_x_continuous(breaks = c(2007, 2011, 2015, 2019), labels = c("2007", "2011", "2015", "2019"))
TreeDen_Y

# Live Native Tree Diameter Distribution ----------------------------------------------
sumTreeDBHDist <- function(park = 'all', from = 2007, to = 2021, QAQC = FALSE, locType = c('VS', 'all'), panels = 1:4,
                           status = c('all', 'active', 'live', 'dead'), speciesType = c('all', 'native','exotic', 'invasive'),
                           canopyPosition = c("all", "canopy"), dist_m = NA, eventType = c('complete', 'all'),
                           units = c('density', 'BA', 'both'), ...){
  
  # Match args and class
  status <- match.arg(status)
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", "HOFU", "PETE",
                      "RICH", "SAHI", "THST", "VAFO"))
  stopifnot(class(from) == "numeric", from >= 2007)
  stopifnot(class(to) == "numeric", to >= 2007)
  locType <- match.arg(locType)
  stopifnot(class(QAQC) == 'logical')
  stopifnot(panels %in% c(1, 2, 3, 4))
  speciesType <- match.arg(speciesType)
  canopyPosition <- match.arg(canopyPosition)
  units <- match.arg(units)
  
  arglist <- list(park = park, from = from, to = to, QAQC = QAQC, panels = panels,
                  locType = locType, eventType = eventType)
  
  plot_events <- do.call(joinLocEvent, arglist) %>% select(Plot_Name, ParkUnit, ParkSubUnit, PlotID, EventID, StartYear, IsQAQC, cycle)
  
  tree_df <- do.call(joinTreeData, c(arglist, list(status = status, speciesType = speciesType, dist_m = dist_m,
                                                   canopyPosition = canopyPosition))) %>%
    filter(!TreeStatusCode %in% c('DF', 'DC', '0','EX', 'ES', 'XS', 'XP', 'XO', 'NL', 'PM')) %>%
    select(Plot_Name, ParkUnit, PlotID, EventID, StartYear, IsQAQC, TagCode, TreeStatusCode, DBHcm, BA_cm2)
  
  tree_df <- tree_df %>% mutate(size_class = case_when(between(DBHcm, 10, 19.9) ~ 'd10_19.9',
                                                       between(DBHcm, 20, 29.9) ~ 'd20_29.9',
                                                       between(DBHcm, 30, 39.9) ~ 'd30_39.9',
                                                       between(DBHcm, 40, 49.9) ~ 'd40_49.9',
                                                       between(DBHcm, 50, 59.9) ~ 'd50_59.9',
                                                       between(DBHcm, 60, 69.9) ~ 'd60_69.9',
                                                       between(DBHcm, 70, 79.9) ~ 'd70_79.9',
                                                       between(DBHcm, 80, 89.9) ~ 'd80_89.9',
                                                       between(DBHcm, 90, 99.9) ~ 'd90_99.9',
                                                       DBHcm >= 100 ~ 'd100p',
                                                       TRUE ~ 'unknown'),
                                stem = ifelse(!is.na(DBHcm), 1, 0),
                                unit_conv = ifelse(ParkUnit == "ACAD", 225, 400))
  
  tree_check <- tree_df %>% filter(size_class == "unknown" & !is.na(TagCode))
  
  if(nrow(tree_check)>0){
    warning(paste("The", nrow(tree_check), "records below are missing DBH measurements and will be removed from summaries."),
            "\n",
            paste(capture.output(data.frame(tree_check[, c("Plot_Name", "StartYear", "TagCode")])), collapse = "\n"))
  }
  
  tree_df$size_class <- ordered(tree_df$size_class,
                                levels = c('d10_19.9', 'd20_29.9', 'd30_39.9', 'd40_49.9',
                                           'd50_59.9', 'd60_69.9', 'd70_79.9', 'd80_89.9',
                                           'd90_99.9', 'd100p', 'unknown'))
  
  tree_df2 <- tree_df %>% arrange(Plot_Name, StartYear, IsQAQC, size_class) %>% filter(size_class != "unknown")
  
  # Summarize stems to size class and pivot wide
  tree_dist <- tree_df2 %>% group_by(Plot_Name, ParkUnit, PlotID, EventID, StartYear, IsQAQC,
                                     size_class, unit_conv) %>%
    summarize(dens = sum(stem) * 10000/first(unit_conv), #stems/ha
              BA = sum(BA_cm2)/first(unit_conv), #m2/ha
              .groups = 'drop')
  
  tree_dist_wide <- switch(units,
                           'density' = tree_dist %>% select(-BA) %>%
                             pivot_wider(names_from = size_class,
                                         values_from = dens,
                                         values_fill = 0,
                                         names_glue = "dens_{str_sub(size_class, 2)}"),
                           'BA' = tree_dist %>% select(-dens) %>%
                             pivot_wider(names_from = size_class,
                                         values_from = BA,
                                         values_fill = 0,
                                         names_glue = "BA_{str_sub(size_class, 2)}"),
                           'both' = tree_dist %>% pivot_wider(names_from = size_class,
                                                              values_from = c(dens, BA),
                                                              values_fill = 0,
                                                              names_glue = "{.value}_{str_sub(size_class, 2)}")
  )
  
  # next few lines find if a size class is missing, and adds it later
  sizes = switch(units,
                 'density' = c('dens_10_19.9', 'dens_20_29.9', 'dens_30_39.9', 'dens_40_49.9',
                               'dens_50_59.9', 'dens_60_69.9', 'dens_70_79.9', 'dens_80_89.9',
                               'dens_90_99.9', 'dens_100p'),
                 'BA' = c('BA_10_19.9', 'BA_20_29.9', 'BA_30_39.9', 'BA_40_49.9',
                          'BA_50_59.9', 'BA_60_69.9', 'BA_70_79.9', 'BA_80_89.9',
                          'BA_90_99.9', 'BA_100p'),
                 'both' = c('dens_10_19.9', 'dens_20_29.9', 'dens_30_39.9', 'dens_40_49.9',
                            'dens_50_59.9', 'dens_60_69.9', 'dens_70_79.9', 'dens_80_89.9',
                            'dens_90_99.9', 'dens_100p',
                            'BA_10_19.9', 'BA_20_29.9', 'BA_30_39.9', 'BA_40_49.9',
                            'BA_50_59.9', 'BA_60_69.9', 'BA_70_79.9', 'BA_80_89.9',
                            'BA_90_99.9', 'BA_100p')
  )
  
  missing_sizes <- setdiff(sizes, names(tree_dist_wide))
  
  tree_dist_wide[missing_sizes] <- 0
  
  tree_dist_final <- left_join(plot_events, tree_dist_wide, by = intersect(names(plot_events), names(tree_dist_wide))) %>%
    select(Plot_Name, ParkUnit, ParkSubUnit, PlotID, EventID, StartYear, IsQAQC, cycle,
           all_of(sizes))
  
  
  return(data.frame(tree_dist_final))
  
} # end of function

tree_dist <- sumTreeDBHDist(park = "all", status = 'live', speciesType = "native", 
                            units = 'density', 
                            from = 2007, to = 2021)

#need to remove years from the incomplete last cycle: C4 for everyone but COLO - C3
colo <- tree_dist %>% filter(ParkUnit == "COLO") %>% filter(cycle == 3)
tree_dist2 <- tree_dist %>% filter(cycle != 4) 
tree_dist3 <- anti_join(tree_dist2, colo)

#join subset that will be the last 4 years
tree_dist_g1 <- sumTreeDBHDist(park =  c("FRSP", "RICH", "PETE", "GEWA", "THST"), 
                               status = 'live', speciesType = "native", 
                               units = 'density', 
                               from = 2018, to = 2021)


tree_dist_g2<- sumTreeDBHDist(park =  c("VAFO", "HOFU", "GETT", "APCO", "BOWA", "COLO", "SAHI", "ASIS"), 
                              status = 'live', speciesType = "native", 
                              units = 'density', 
                              from = 2016, to = 2019)

tree_dist_g1$cycle <- 4# had to use 4 because loess needs numeric?
tree_dist_g2$cycle <- 4 #should be last4yr

#combine all datasets together
tree_dist4 <- rbind(tree_dist3, tree_dist_g1, tree_dist_g2)

names(tree_dist4)
tree_dist_park <- tree_dist4 %>% select(ParkSubUnit, cycle, dens_10_19.9:dens_100p) %>% 
  group_by(ParkSubUnit, cycle) %>% 
  summarise(across(starts_with("dens_"),
                   ~mean(.x, na.rm = TRUE),
                   .groups = 'drop'))

tree_dist_long <- tree_dist_park %>% select(ParkSubUnit, cycle, dens_10_19.9:dens_100p) %>% 
                                     pivot_longer(cols = starts_with("dens_"), names_to = "size_class",
                                                  values_to = "density") 

tree_dist_long$cycle <- as.factor(tree_dist_long$cycle)                             
str(tree_dist_long)

sort(unique(tree_dist_long$size_class))
tree_dist_long$size_class <- ordered(tree_dist_long$size_class,
                                         levels = c("dens_10_19.9", "dens_20_29.9", "dens_30_39.9",
                                                    "dens_40_49.9", "dens_50_59.9", "dens_60_69.9",
                                                    "dens_70_79.9", "dens_80_89.9", "dens_90_99.9",
                                                    "dens_100p"))

tree_dist_plot <- ggplot(data = tree_dist_long, aes(color = cycle, x = size_class, y = density))+
  #geom_point()+ 
  geom_line(aes(group = cycle), size = 1)+
  facet_wrap(~ParkSubUnit, ncol = 5)+
  labs(x = "Live Native Diameter Class (cm)", y = "stems/ha")+ 
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
  scale_x_discrete(labels = c("10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "100+"))+
  theme_FVM() 

tree_dist_plot

# Invasives quadrat cover ---------------------------------------------

quadsI <- joinQuadSpecies(locType = "VS", speciesType = "invasive", valueType = "all")
seedsI <- joinQuadSeedlings(locType = "VS", speciesType = "invasive", valueType = "all")

