library(tidyverse)
# Functions ---------------------------------------------------------------

# Two arguments : files_name_list and treatment_names.
# files_name_list contains the FACs data which are exported as CSV files, each CSV files 
# represented as a sample acquired by FACs Guava, the two color acquired value are 
# extracted from the CSV file.
# Treatment_names are used as the samples names which were used in the data manipulation
# and plot procedure.

c_data <- function(files = file_name_list, 
                   treatment_names = tnames){
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

