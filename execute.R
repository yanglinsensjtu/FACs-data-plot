source('FACs-data-plot/functions.R')

# Arguments ---------------------------------------------------------------

file_name_list <- dir(pattern = "\\.CSV")
treat_names <- c()
label_names <-  c()

# save sample labels to disk ----------------------------------------------


fntn <-  C_filename_treatname(file_name_list,treat_names)
unlistfntn <- unlist(fntn)
write.csv(unlistfntn,file = 'lable.txt')

# plot --------------------------------------------------------------------

data <- c_data(files = file_name_list, treatment_names = treat_names)

FACS_plot(data = data,
          plot_treatment_names = treat_names,
          plot_label_names = label_names,
          plot_title = 'total',
          xmin = 0.35,
          xmax = 0.9
)

