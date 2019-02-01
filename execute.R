source('FACs-data-plot/functions.R')

# Arguments ---------------------------------------------------------------

file_name_list <- dir(pattern = "\\.CSV")

file_name_list

treat_names <- c()



label_names <- c()

# plot --------------------------------------------------------------------

data <- c_data(files = file_name_list, treatment_names = treat_names)

FACS_plot(data = data,
          plot_treatment_names = treat_names,
          plot_label_names = label_names,
          plot_title = '',
          xmin = 0.35,
          xmax = 0.9
)

FACS_plot(data = data,
          plot_treatment_names = c(treat_names[1:2],treat_names[3:6]),
          plot_label_names = c(label_names[1:2],label_names[3:6]),
          plot_title = '',
          xmin = 0.35,
          xmax = 0.9
)

FACS_plot(data = data,
          plot_treatment_names = c(treat_names[1:2],treat_names[7:10]),
          plot_label_names = c(label_names[1:2],label_names[7:10]),
          plot_title = '',
          xmin = 0.35,
          xmax = 0.9
)

fntn <-  C_filename_treatname(file_name_list,treat_names)

unlist(fntn)
