rm(list = ls())
gc()
library(tidyverse)
# Functions ---------------------------------------------------------------

# Two arguments : files_name_list and treatment_names.
# files_name_list contains the FACs data which are exported as CSV files, each CSV files 
# represented as a sample acquired by FACs Guava, the two color acquired value are 
# extracted from the CSV file.
# Treatment_names are used as the samples names which were used in the data manipulation
# and plot procedure.

c_data <- function(files = file_name_list, treatment_names = tnames){
  nm <- vector('list',length(files))
  migb <- vector('list',length(files))
  for (i in seq_along(files)) {
    nm[[i]] <- read_csv(files[[i]])
    nm[[i]] <- nm[[i]] %>% 
      filter(P01.R1.R2 == 1) %>%
      mutate(treatment = `GRN-B-HLog`/`RED-B-HLog`) %>% 
      select(treatment)
    names(nm[[i]]) <- treatment_names[i]
    migb <- bind_cols(migb, nm[[i]])
  }
  migb
}
# FACS_plot has five arguments data, plot_treat_names, plot_label_names, 
# plot_title, xmin, xmax.
# c_data function produced the data, which contains the relative color 
# value in each column, column names are the sample names(treatment_names).
# Plot_treat_names refer to the samples which are used to plot in the 
# final figure are subset of treatment_names. Plot_label_names are 
#the figure caption labels according to plot_treat_names. Plot_title 
# is the figure title. Xmin or xmax is the minimum or maximum value of 
# the x-axis.


FACS_plot <- function(data = migbc, 
                      plot_treatment_names = ptn, 
                      plot_label_names = pln,
                      plot_title = title, 
                      xmin = xmin, 
                      xmax = xmax){
  total <- select(data, plot_treatment_names) %>% 
    gather(plot_treatment_names,
           key = "treatment",value = "cases") %>% 
    ggplot(aes(cases,color = treatment)) +
    geom_density() + 
    xlim(xmin,xmax) + 
    labs(x = "EGFP/mCherry",title = plot_title ) + 
    theme(axis.title = element_text(family = "Helvetica"), 
          plot.title = element_text(family = "Helvetica", hjust = 0.5),
          legend.text = element_text(family = "Helvetica"), 
          legend.title = element_text(family = "Helvetica")) +
    scale_color_hue(limits = plot_treatment_names,
                    label = plot_label_names)
  
  
  ggsave(filename = str_c(plot_title,".tiff"),width = 10, height = 6.18)
}

get_treatment_label_names <- function(t_treatment_names = t_treatment_names,
                                      t_label_names = t_label_names,
                                      x = x,
                                      y = y){
  tn <- vector('character', (y - x + 1))
  ln <- vector('character', (y - x + 1))
  for(i in c(x:y)){
    tn[i] <- t_treatment_names[i]
    ln[i] <- t_label_names[i]
  }
  tn <- tn[!tn=='']
  ln <- ln[!ln=='']
  t_l <- list(tn, ln)
  t_l
}

get_title <- function(title_1 = title_1, title_2 = title_2){
  titlet <- vector('character',length(title_2))
  for (i in seq_along(titlet)) {
    titlet[i] <- str_c(title_1,' ', title_2[i])
  }
  titlet
}

re_c <- function(t_l = t_l, o_t_l = o_t_l, s_t_l = s_t_l, titlet = titlet){
  t_l[3] <- titlet[1]
  o_t_l[3] <- titlet[2]
  s_t_l[3] <- titlet[3]
  
  treatment_names <- list(t_l[[1]],o_t_l[[1]],s_t_l[[1]])
  label_names <- list(t_l[[2]],o_t_l[[2]],s_t_l[[2]])
  title_names <- list(t_l[[3]],o_t_l[[3]],s_t_l[[3]])
  tibble(treatment_names, label_names, title_names)
}
# Arguments ---------------------------------------------------------------


file_name_list <- dir(pattern = "\\.CSV")

t_treatment_names <- c("migb",
                       "migb.UBE3C.sgRNA1.6",
                       "migb.UBE3C.sgRNA1.10",
                       "migb.RAB3A.sgRNA1.10",
                       "migb.RAB3A.sgRNA2.11",
                       "migb.RAB3B.sgRNA")
t_label_names <- c("MIGBc + vector",
                   "MIGBc + UBE3C sgRNA1-6",
                   "MIGBc + UBE3C sgRNA1-10",
                   "MIGBc + RAB3A sgRNA1-10",
                   "MIGBc + RAB3A sgRNA2-11",
                   "MIGBc + RAB3A sgRNA")

title_1 <- c("the mCherry IRES EGFPBOKcyto(MIGBc) affected by")

title_2 <- c('UBE3C sgRNA monoclony','RAB3A sgRNA monoclony')
# plot --------------------------------------------------------------------


data <- c_data(files = file_name_list, treatment_names = t_treatment_names)

titlet <- get_title(title_1, title_2)

t_l <- get_treatment_label_names(t_treatment_names, t_label_names, 1, 6)
o_t_l <- get_treatment_label_names(t_treatment_names,t_label_names, 1, 3)
s_t_l <- get_treatment_label_names(t_treatment_names,t_label_names, 4, 6)

treatment_label_title_names <- re_c(t_l,o_t_l,s_t_l,titlet)

for (i in (1:ncol(treatment_label_title_names))) {
  FACS_plot(data, 
            treatment_label_title_names$treatment_names[[i]], 
            treatment_label_title_names$label_names[[i]],
            treatment_label_title_names$title_names[[i]], 
            xmin = 0.4, xmax = 1)
  print(i)
}

rm(list = ls())
gc()




