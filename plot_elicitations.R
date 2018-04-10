# load package
library(googlesheets)
library(plyr)
library(abind)

overlay_plot_list <- function(plot_type, plot_list, x_vec, yticks, ylims, heading, ylab, x_lab, col_vec, lty_vec, lwd_vec, legend_vec, legend_loc){
  
  if (plot_type == 'non-overlay'){
    graphics::plot(y = plot_list[[1]], x = x_vec, type = 'l', main = heading, ylim = ylims, ylab = ylab, xlab = x_lab, col = col_vec[1], lty = lty_vec[1], lwd = lwd_vec[1])
  } else {
    lines(y = plot_list[[1]], x = x_vec, type = 'l', main = heading, ylim = ylims, ylab = ylab, xlab = x_lab, col = col_vec[1], lty = lty_vec[1], lwd = lwd_vec[1])
  }
  
  if (length(plot_list) > 1){
    for (plot_ind in 2:length(plot_list)){
      lines(y = plot_list[[plot_ind]],  x = x_vec, ylim = ylims, col = col_vec[plot_ind], lwd = lwd_vec[plot_ind], lty = lty_vec[plot_ind])
    }
  }
  abline(h = 0, lty = 2)
  if (legend_vec[1] != 'NA'){
    legend(legend_loc, legend_vec, bty="n", lty = lty_vec, cex = 1,  pt.cex = 1, lwd = lwd_vec, col = col_vec)
  }
}

setup_sub_plots <- function(nx, ny, x_space, y_space){
  par(mfrow = c(ny, nx))
  par(cex = 0.6)
  par(mar = c(x_space, y_space, 1, 0), oma = c(5, 5, 5, 5))
  
  par(tcl = -0.25)
  par(mgp = c(2, 0.3, 0))
  
}


include_random_data = TRUE
author_num = 3
author_col = rainbow(author_num)
sheet_num = 2
write_pdf = TRUE
output_pdf_filename = '~/Documents/elicitations_test_1.pdf'
worksheets_to_use = c(2, 3)
plot_lwd = 1
plot_lty = 1
mean_plot_lwd = 2
mean_plot_lty = 2

col_vec = c(2, 3, 4)
time_vec = c(0, 20, 40, 60)
lty_vec = c(1, 1, 2)
ylims = c(0, 10)
nx = 3
ny = 2
plot_x_space = 5
plot_y_space = 5
plot_selection_type = 'by_plot'

sheet_characteristics = gs_ls()
author_sheets_to_use = grepl('CPW_test', sheet_characteristics$sheet_title)

googlesheet_names = sheet_characteristics$sheet_title[author_sheets_to_use]

column_names = c('Year', 'Lower Bound',	'Upper Bound',	'Best Estimate',	'Confidence 50-100 (%)',	
              '90% CI (LB)',	'90% CI (UB)')


numerical_data = vector('list', sheet_num)
for (sheet_ind in seq(sheet_num)){
  numerical_data[[sheet_ind]] = vector('list', author_num)
}

plot_names = numerical_data

for (author_ind in seq(author_num)){
  
  current_sheet_name = googlesheet_names[author_ind]
  current_sheet_characteristics = gs_title(current_sheet_name)
  current_worksheet_names = gs_ws_ls(current_sheet_characteristics)
  for (current_sheet_ind in seq_along(worksheets_to_use)){
    current_sheet_data = as.data.frame(gs_read(ss = current_sheet_characteristics, 
                                               ws = current_worksheet_names[worksheets_to_use[current_sheet_ind]]))
    #rows_to_use = !is.na(as.numeric(current_sheet_data[, 1]))
    #data_to_use = current_sheet_data[rows_to_use, 1:7]
    
    data_to_use = current_sheet_data[, 1:7]
    names(data_to_use) = column_names
    plot_names[[current_sheet_ind]][[author_ind]] = data_to_use[, 1]

    current_data = data.matrix(data_to_use)
    
    # simulate randomness in answers
    if (include_random_data == TRUE){
      random_data = matrix(data = sample(3, size = nrow(current_data)*(ncol(current_data) - 1), replace = TRUE), nrow = nrow(current_data))
      current_data[, 2:ncol(current_data)] = current_data[, 2:ncol(current_data)] + random_data
    }
    numerical_data[[current_sheet_ind]][[author_ind]] = current_data
  }
  
}

current_worksheet_names = current_worksheet_names[worksheets_to_use]

numerical_data_matrix = lapply(seq_along(numerical_data), function(i) abind(numerical_data[[i]], along=3))

sheet_means = lapply(seq_along(numerical_data), function(i) Reduce('+', numerical_data[[i]])/author_num)

sheet_mins = lapply(seq_along(numerical_data), function(i) aaply(laply(numerical_data[[i]], as.matrix), c(2, 3), min, na.omit = TRUE))
sheet_maxs = lapply(seq_along(numerical_data), function(i) aaply(laply(numerical_data[[i]], as.matrix), c(2, 3), max, na.omit = TRUE))
sheet_means = lapply(seq_along(numerical_data), function(i) aaply(laply(numerical_data[[i]], as.matrix), c(2, 3), mean, na.omit = TRUE))

plot_starts = lapply(seq_along(sheet_means), function(i) as.numeric(which(sheet_means[[i]][, 1] == 0)))


if (write_pdf == TRUE){
  pdf(output_pdf_filename, width = 11.7, height = 8.3)
}

for (sheet_ind in 1:sheet_num){
  current_plot_starts = plot_starts[[sheet_ind]]
  if (plot_selection_type == 'by_plot'){
    setup_sub_plots(nx, ny = length(current_plot_starts), plot_x_space, plot_y_space)
  }
  for (plot_ind in 1:length(current_plot_starts)){
    current_plot_vec = current_plot_starts[plot_ind]:(current_plot_starts[plot_ind] + 3)
    plot_list = lapply(seq_along(numerical_data[[sheet_ind]]), function(i) numerical_data[[sheet_ind]][[i]][current_plot_vec, ])
    current_mean_list = sheet_means[[sheet_ind]][current_plot_vec, ]
    if (plot_selection_type == 'by_author'){
      setup_sub_plots(nx, ny, x_space = plot_x_space, y_space = plot_y_space)
      mean_plot_list = lapply(col_vec, function(i) current_mean_list[, i])
      for (author_ind in seq(author_num)){
        current_plot_list = lapply(col_vec, function(i) plot_list[[author_ind]][, i])
        
        current_plot_name = plot_names[[sheet_ind]][[author_ind]][current_plot_starts[plot_ind] - 2]
        
        overlay_plot_list(plot_type = 'non-overlay', current_plot_list, x_vec = time_vec, yticks = 'y', ylims, heading = current_plot_name, ylab = '', x_lab = '', 
                          col_vec = rep(author_col[author_ind], 3), lty_vec, lwd_vec = rep(plot_lwd, length(plot_list)), 
                          legend_vec = 'NA', legend_loc = FALSE)
        
        overlay_plot_list(plot_type = 'overlay', mean_plot_list, x_vec = time_vec, yticks = 'y', ylims, heading = current_plot_name, ylab = '', x_lab = '', 
                          col_vec = rep('black', 3), lty_vec, lwd_vec = rep(mean_plot_lwd, length(plot_list)), 
                          legend_vec = 'NA', legend_loc = FALSE)
        
      }
      
    } else if (plot_selection_type == 'by_plot'){
      
      for (col_ind in col_vec){
        current_plot_list = lapply(seq_along(plot_list), function(i) plot_list[[i]][, col_ind])
#         if ((col_ind == 2) || (col_vec == 6)){
#           plot_type = 'non-overlay'
#         } else {
#           plot_type = 'overlay'
#         }
        print(column_names[col_ind])
        
        if (col_ind == 2){
          y_lab = plot_names[[sheet_ind]][[1]][current_plot_starts[plot_ind] - 2]
        } else {
          y_lab = ''
        }
        overlay_plot_list(plot_type = 'non-overlay', current_plot_list, x_vec = time_vec, yticks = 'y', ylims, heading = column_names[col_ind], y_lab, x_lab = '', 
                          col_vec = author_col, lty_vec = rep(plot_lty, length(plot_list)), lwd_vec = rep(plot_lwd, length(plot_list)), 
                          legend_vec = 'NA', legend_loc = FALSE)
        overlay_plot_list(plot_type = 'overlay', list(current_mean_list[, col_ind]), x_vec = time_vec, yticks = 'y', ylims, heading = column_names[col_ind], y_lab, x_lab = '', 
                          col_vec = 'black', lty_vec = mean_plot_lty, lwd_vec = mean_plot_lwd, 
                          legend_vec = 'NA', legend_loc = FALSE)
      }
    }
    
    title(current_worksheet_names[sheet_ind], outer=TRUE)
  } 
}


if (write_pdf == TRUE) {
  graphics.off()
}                            




