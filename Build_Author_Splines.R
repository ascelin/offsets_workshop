# load package
library(googlesheets)
library(plyr)
library(abind)

source('Build_Author_Splines_functions.R')



# gs_auth(new_user = TRUE)

# author_col = c('darkblue',
#                'red',
#                'pink',
#                'lightblue',
#                'orange',
#                'green',
#                'darkgreen')
# 
# author_names = c("Elicitation_CP_Workshop_cmorris",
#                  "Elicitation_CP_Workshop_dkeith",
#                  "Elicitation_CP_Workshop_dkirk" ,     
#                  "Elicitation_CP_Workshop_gsteenbeeke",
#                  "Elicitation_CP_Workshop_jsanders",
#                  "Elicitation_CP_Workshop_pprice",     
#                  "Elicitation_CP_Workshop_pridgeway")

author_col = c('darkblue',
               'red',
               'pink',
               'lightblue',
               'orange',
               'green')

author_names = c("REVISED_Elicitation_CP_Workshop_cmorris",
                 "REVISED_Elicitation_CP_Workshop_dkeith",
                 "REVISED_Elicitation_CP_Workshop_dkirk" ,     
                 "REVISED_Elicitation_CP_Workshop_gsteenbeeke",
                 "Elicitation_CP_Workshop_jsanders",
                 "REVISED_Elicitation_CP_Workshop_pprice")

worksheet_names = c("Instructions", "1. TG-Low" , "2. TG-Med1", "3. TG-Med2", "4. TG-High","5. GG-Low","6. GG-Med1","7. GG-Med2",                           
                    "8. GG-High","9. FG-Low","10. FG-Med1","11. FG-Med2" ,                         
                    "12. FG-High","Cover Benchmarks","13. TG-Cover-Low" ,"14. TG-Cover-Med1" ,                   
                    "15. TG-Cover-Med2","16. TG-Cover-High", "17. GG-Cover-Low", "18. GG-Cover-Med1" ,                   
                    "19. GG-Cover-Med2","20. GG-Cover-High", "Further information", "Details of management actions" ,       
                    "Growth form categorization of species")

column_names = c('Year', 'Lower Bound',	'Upper Bound',	'Best Estimate',	'Confidence 50-100 (%)',	
                 '90% CI (LB)',	'90% CI (UB)', 'Uppermost Bound')

calc_y_lims = FALSE
write_pdf = FALSE

# set to TRUE to pull google sheets, setting to FALSE uses local data
pull_data = FALSE

plot_selection_type = 'by_plot'
plot_means = FALSE
plot_splines = FALSE
plot_sheets = TRUE
plot_mean = FALSE
plot_author = TRUE

#output_pdf_filename = 'CP_elicitation_workshop_1.pdf'
output_pdf_filename = 'Splines_v2.pdf'
file_prefix = 'REVISED_Elicitation_CP_Workshop'
strings_to_exclude = FALSE
worksheets_to_pull = c(2:13, 15:22)  # worksheets to pull down from google sheets- only for pull_data TRUE
worksheets_to_collate = c(2:13, 15:22) # what data to work with - Note sheet 1 is instructions, 14 is benchmark 
#worksheets_to_collate = c(2:13, 15:22)

sheet_num = length(worksheets_to_pull)

authors_to_pull = c(1)
authors_to_plot = 1:5
time_vec_interpolated = 0:80
fit_type = 'by_mean'

# 2 - lower bound
# 3 - upper bound
# 4 - best estimate
column_to_use = 4
plot_sheet_num = length(worksheets_to_collate)


author_ind = 1

plot_lwd = 2
plot_lty = 1
mean_plot_lwd = 3
mean_plot_lty = 2

cols_to_plot = c(2, 3, 4)
time_vec = c(0, 20, 40, 60, 80)
lty_vec = c(1, 1, 2)

# set the spacing between each of the plots
plot_x_space =  2
plot_y_space =  2.8

if (pull_data == TRUE){
  pull_worksheets(file_prefix, strings_to_exclude, sheet_characteristics, authors_to_pull, worksheets_to_pull)
}


sheet_data <- collate_sheet_data(worksheets_to_collate, authors_to_plot)

comment_indx = lapply(seq_along(sheet_data), 
                      function(i) grep("Comment", sheet_data[[i]][[1]][, 1]))

comments = lapply(seq_along(sheet_data), 
                  function(i) lapply(seq_along(sheet_data[[i]]), function(j) sheet_data[[i]][[j]][comment_indx[[i]], 2]))

worksheet_names_to_use = worksheet_names[worksheets_to_collate]

numerical_data_matrix = lapply(seq_along(sheet_data), function(i) lapply(seq_along(sheet_data[[i]]),
                                                                         function(j) data.matrix(sheet_data[[i]][[j]])))

numerical_data_matrix = lapply(seq_along(numerical_data_matrix), function(i) abind(numerical_data_matrix[[i]], along=3))

sheet_mins = lapply(seq_along(numerical_data_matrix), function(i) apply(numerical_data_matrix[[i]], c(1, 2), min, na.rm = TRUE))
sheet_maxs = lapply(seq_along(numerical_data_matrix), function(i) apply(numerical_data_matrix[[i]], c(1, 2), max, na.rm = TRUE))
sheet_means = lapply(seq_along(numerical_data_matrix), function(i) apply(numerical_data_matrix[[i]], c(1, 2), mean, na.rm = TRUE))

plot_starts = lapply(seq_along(sheet_means), function(i) as.numeric(which(sheet_means[[i]][, 1] == 0)))

plot_nums = lapply(seq_along(plot_starts), function(i) length(plot_starts[[i]]))

worksheet_comments = lapply(seq_along(comments), 
                            function(i) lapply(seq(plot_nums[[i]]), 
                                               function(j) lapply(seq_along(authors_to_plot), 
                                                                  function(k) comments[[i]][[k]][j])))


if (calc_y_lims == TRUE){
  sheet_y_lims = lapply(seq_along(sheet_maxs), function(i) c(0, max(sheet_maxs[[i]][, 3], na.rm = TRUE)))
} else {
  sheet_y_lims = rep(list(array(0, 2)), 20)
  
  sheet_y_lims[[1]][2] <- 10.1
  sheet_y_lims[[2]][2] <- 10.1
  sheet_y_lims[[3]][2] <- 10.1
  sheet_y_lims[[4]][2] <- 10.1
  
  sheet_y_lims[[5]][2] <- 22
  sheet_y_lims[[6]][2] <- 22
  sheet_y_lims[[7]][2] <- 22
  sheet_y_lims[[8]][2] <- 22
  
  sheet_y_lims[[9]][2]  <- 26
  sheet_y_lims[[10]][2] <- 26
  sheet_y_lims[[11]][2] <- 26
  sheet_y_lims[[12]][2] <- 26
  
  sheet_y_lims[[13]][2] <- 80
  sheet_y_lims[[14]][2] <- 80
  sheet_y_lims[[15]][2] <- 80
  sheet_y_lims[[16]][2] <- 80
  
  sheet_y_lims[[17]][2] <- 80
  sheet_y_lims[[18]][2] <- 80
  sheet_y_lims[[19]][2] <- 80
  sheet_y_lims[[20]][2] <- 80
  
}


# Note: the object author_spline_fits has the following structure 
#   author_spline_fits[[1:20]][[1:3]][[1:3]]
#    where the 1:20 is the sheet number from the google sheets for each expert
#    the first 1:3 the management class (eg typical activities, low intensity mgmt, high intensity mgmt) 
#    the second 1:3 is specifies the lower, upper and best estimates from the experts


for (author_ind in authors_to_plot){
    author_spline_fits = fit_splines(data_to_fit = numerical_data_matrix, plot_sheet_num, time_vec, fit_type = 'by_author',  plot_starts, columns_to_use = c(2, 3, 4), author_ind)
    saveRDS(object = author_spline_fits, paste0(author_names[author_ind], '_splines.rds'))
}

mean_spline_fits = fit_splines(data_to_fit = sheet_means, plot_sheet_num, time_vec, fit_type = 'by_mean',  plot_starts, columns_to_use = c(2, 3, 4), author_ind = 1)
saveRDS(object = mean_spline_fits, paste0('mean_splines.rds'))


# this generated the condition_class_bounds object
source('cond.thresholds.R')

 

if (plot_sheets == TRUE){

    # NOTE: only using time_vec[1:4], as the raw data is just 0, 20, 40, 60
    # years. The splines have an extra time point at the end and go out to 80
    # years, and we just assume this last20 years is at the constrant value from t=60 years.

   plot_sheet_data(numerical_data_matrix, plot_sheet_num = length(worksheets_to_collate),   author_num = length(authors_to_plot), plot_x_space, plot_y_space, time_vec[1:4], comments, 
                 sheet_mins, sheet_maxs, sheet_means, plot_starts, plot_nums, worksheet_comments, worksheet_names_to_use, sheet_y_lims)
}

# if (plot_splines == TRUE){

#   plot_spline_data(author_spline_fits, mean_spline_fits, plot_author, plot_mean, numerical_data_matrix, plot_sheet_num, plot_x_space, plot_y_space, plot_starts,
#              author_names, author_ind, sheet_means, time_vec[1:4], time_vec_interpolated, sheet_y_lims, worksheet_names_to_use, condition_class_bounds)
# }

