column_to_use = 3
x = c(1, 20, 40, 60)
x_new = 1:100
spls = vector('list', plot_sheet_num)
fit_type = 'by_mean'

for (sheet_ind in 1:plot_sheet_num){
  
  current_plot_starts = plot_starts[[sheet_ind]]
  sheet_spls = vector('list', length(current_plot_starts))
  for (plot_ind in 1:length(current_plot_starts)){
    
    current_plot_vec = current_plot_starts[plot_ind]:(current_plot_starts[plot_ind] + 3)
    
    if (fit_type == 'by_author'){
      plot_list = lapply(seq_along(numerical_data[[sheet_ind]]), function(i) numerical_data[[sheet_ind]][[i]][current_plot_vec, ])
      current_plot_list = lapply(seq_along(plot_list), function(i) plot_list[[i]][, column_to_use])
      author_spls = vector('list', author_num)
      for (author_ind in seq(author_num)){
        current_plot = current_plot_list[[author_ind]]
        if (all(!is.na(current_plot))){
          author_spls[[author_ind]] <- smooth.spline(x, current_plot, df = 8) 
        }
      }
    } else if (fit_type == 'by_mean'){
      current_plot = sheet_means[[sheet_ind]][current_plot_vec, column_to_use]
      if (all(!is.na(current_plot))){
        author_spls <- smooth.spline(x, current_plot) 
      }
    }
    
    sheet_spls[[plot_ind]] = author_spls
  }
  spls[[sheet_ind]] = sheet_spls
}

plot_x_space = 5
plot_y_space = 5


if (write_pdf == TRUE){
  pdf(paste0('~/Documents/spline_fits.pdf'), width = 8.3, height = 11.7)
}



for (sheet_ind in 1:plot_sheet_num){
  setup_sub_plots(nx = 3, ny = 3, plot_x_space, plot_y_space)
  current_plot_starts = plot_starts[[sheet_ind]]
  sheet_spls = vector('list', length(current_plot_starts))
  for (plot_ind in 1:length(current_plot_starts)){
    plot_lab = plot_names[[sheet_ind]][[1]][current_plot_starts[plot_ind] - 2]
    current_plot_vec = current_plot_starts[plot_ind]:(current_plot_starts[plot_ind] + 3)
    
    if (fit_type == 'by_author'){
      plot_list = lapply(seq(author_num), function(i) numerical_data_matrix[[sheet_ind]][rows_to_plot, , i])
      current_plot_list = lapply(seq_along(plot_list), function(i) plot_list[[i]][, column_to_use])
      author_spls = vector('list', author_num)
      for (author_ind in seq(author_num)){
        current_plot = current_plot_list[[author_ind]]
        if (all(!is.na(current_plot))){
          author_spls[[author_ind]] <- smooth.spline(x, current_plot) 
        }
      }
    } else if (fit_type == 'by_mean'){
      current_plot_points = sheet_means[[sheet_ind]][current_plot_vec, column_to_use]
      current_plot_spline = predict(spls[[sheet_ind]][[plot_ind]], x_new)
      current_plot_list = list(current_plot_spline$y)
      y_lab = gsub("Management scenario:", "", plot_lab)
      
      overlay_plot_list(plot_type = 'non-overlay', current_plot_list, x_vec = x_new, yticks = 'y', y_lims, heading = column_names[col_ind], y_lab, x_lab = '', 
                        col_vec = author_col, lty_vec = rep(plot_lty, length(plot_list)), lwd_vec = rep(plot_lwd, length(plot_list)), 
                        legend_vec = 'NA', legend_loc = FALSE)
      
      points(x, current_plot_points)

    }
    
  }
  title(plot_worksheet_names[sheet_ind], outer=TRUE)
}


if (write_pdf == TRUE){
  graphics.off()
}
