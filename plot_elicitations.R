# load package
library(googlesheets)
library(plyr)
library(abind)



fit_splines <- function(numerical_data_matrix, plot_sheet_num, time_vec_new, fit_type, plot_starts, column_to_use){
  spline_fits = vector('list', plot_sheet_num)
  
  for (sheet_ind in 1:plot_sheet_num){
    
    current_plot_starts = plot_starts[[sheet_ind]]
    sheet_spline_fits = vector('list', length(current_plot_starts))
    
    for (plot_ind in 1:length(current_plot_starts)){
      
      current_plot_vec = current_plot_starts[plot_ind]:(current_plot_starts[plot_ind] + 3)
      
      if (fit_type == 'by_author'){
        plot_list = lapply(seq_along(numerical_data_matrix[[sheet_ind]]), function(i) numerical_data_matrix[[sheet_ind]][[i]][current_plot_vec, ])
        current_plot_list = lapply(seq_along(plot_list), function(i) plot_list[[i]][, column_to_use])
        author_spline_fits = vector('list', author_num)
        for (author_ind in seq(author_num)){
          current_plot = current_plot_list[[author_ind]]
          if (all(!is.na(current_plot))){
            author_spline_fits[[author_ind]] <- smooth.spline(x, current_plot, df = 8) 
          }
        }
      } else if (fit_type == 'by_mean'){
        
        current_plot = sheet_means[[sheet_ind]][current_plot_vec, column_to_use]
        if (all(!is.na(current_plot))){
          author_spline_fits <- smooth.spline(time_vec, current_plot) 
        }
        
      }
      
      sheet_spline_fits[[plot_ind]] = author_spline_fits
    }
    spline_fits[[sheet_ind]] = sheet_spline_fits
  }
  return(spline_fits)
}



plot_splines <- function(spline_fits, numerical_data_matrix,plot_sheet_num, plot_x_space, plot_y_space, plot_starts, plot_names, 
                         fit_type, author_num, sheet_means, time_vec, time_vec_new, sheet_y_lims, worksheet_names){
  
  if (write_pdf == TRUE){
    pdf(paste0('plots/spline_fits.pdf'), width = 8.3, height = 11.7)
  }
  
  for (sheet_ind in 1:plot_sheet_num){
    y_lims = sheet_y_lims[[sheet_ind]]
    setup_sub_plots(nx = 3, ny = 3, plot_x_space, plot_y_space)
    current_plot_starts = plot_starts[[sheet_ind]]

    for (plot_ind in 1:length(current_plot_starts)){
      #plot_lab = plot_names[[sheet_ind]][[1]][current_plot_starts[plot_ind] - 2]
      plot_lab = ''
      current_plot_vec = current_plot_starts[plot_ind]:(current_plot_starts[plot_ind] + 3)
      
      if (fit_type == 'by_author'){
        plot_list = lapply(seq(author_num), function(i) numerical_data_matrix[[sheet_ind]][rows_to_plot, , i])
        current_plot_list = lapply(seq_along(plot_list), function(i) plot_list[[i]][, column_to_use])
        author_spline_fits = vector('list', author_num)
        for (author_ind in seq(author_num)){
          current_plot = current_plot_list[[author_ind]]
          if (all(!is.na(current_plot))){
            author_spline_fits[[author_ind]] <- smooth.spline(time_vec, current_plot) 
          }
        }
      } else if (fit_type == 'by_mean'){
        current_plot_points = sheet_means[[sheet_ind]][current_plot_vec, column_to_use]
        current_plot_spline = predict(spline_fits[[sheet_ind]][[plot_ind]], time_vec_new)
        
        current_plot_list = list(current_plot_spline$y)
        y_lab = gsub("Management scenario:", "", plot_lab)
        #plot_heading = column_names[col_ind]
        plot_heading = ''
        overlay_plot_list(plot_type = 'non-overlay', current_plot_list, x_vec = time_vec_new, yticks = 'y', y_lims, heading = plot_heading, y_lab, x_lab = '', 
                          col_vec = author_col, lty_vec = rep(plot_lty, length(current_plot_list)), lwd_vec = rep(plot_lwd, length(current_plot_list)), 
                          legend_vec = 'NA', legend_loc = FALSE)
        
        points(time_vec, current_plot_points)
        
      }
      
    }
    title(worksheet_names[sheet_ind], outer=TRUE)
  }
  
  
  if (write_pdf == TRUE){
    graphics.off()
  }
}






pull_worksheets <- function(file_prefix, sheet_characteristics, authors_to_pull, worksheets_to_pull){
  sheet_characteristics = gs_ls()
  author_sheets_to_use = grepl(file_prefix, sheet_characteristics$sheet_title)
  googlesheet_names = sheet_characteristics$sheet_title[author_sheets_to_use]
  googlesheet_names = sort(googlesheet_names)
  
  for (author_ind in authors_to_pull){
    current_sheet_name = googlesheet_names[author_ind]
    current_sheet_characteristics = gs_title(current_sheet_name)
    current_worksheet_names = gs_ws_ls(current_sheet_characteristics)
    
    for (current_sheet_ind in worksheets_to_pull){
      current_filename = paste0('author_responses/author_', author_ind, '_sheet_', current_sheet_ind, '.csv')
      gs_download(from = current_sheet_characteristics, 
                  current_worksheet_names[current_sheet_ind], to = current_filename, overwrite = TRUE)
      Sys.sleep(6)
    }
    
  }
  
}


collate_sheet_data <- function(worksheets_to_collate, author_num){
  plot_sheet_num = length(worksheets_to_collate)
  sheet_data = vector('list', plot_sheet_num)
  
  for (current_sheet_ind in seq_along(worksheets_to_collate)){
    current_sheet_data = vector('list', author_num)
    for (author_ind in seq(author_num)){
      
      current_filename = paste0('author_responses/author_', author_ind, '_sheet_', worksheets_to_collate[current_sheet_ind], '.csv')
      
      tmp_data = read.csv(current_filename, na.strings=c("","NA"), stringsAsFactors = FALSE)
      tmp_data = tmp_data[, 1:8]
      names(tmp_data) = column_names
      current_sheet_data[[author_ind]] = tmp_data
    }
    sheet_data[[current_sheet_ind]] = current_sheet_data
  }
  return(sheet_data)
}


plot_sheet_data <- function(numerical_data_matrix, plot_sheet_num, author_num, plot_x_space, plot_y_space, time_vec, comments, 
                            sheet_mins, sheet_maxs, sheet_means, plot_starts, plot_nums, worksheet_comments, worksheet_names, sheet_y_lims){

  if (write_pdf == TRUE){
    if (!file.exists(paste0(getwd(), '/plots/'))){
      dir.create(paste0(getwd(), '/plots/'))
    }
  }
  
  fileConn <- file("author_comments.txt", open = "w+")
  
  for (sheet_ind in 1:plot_sheet_num){
    
    cat(worksheet_names[sheet_ind], file = fileConn, sep = "\n")
    
    if (write_pdf == TRUE){
      pdf(paste0('plots/', worksheet_names[sheet_ind], '.pdf'), width = 8.3, height = 11.7)
    }
    
    current_plot_starts = plot_starts[[sheet_ind]]
    y_lims = sheet_y_lims[[sheet_ind]]
    
    if (plot_selection_type == 'by_plot'){
      setup_sub_plots(nx = 3, ny = 3, plot_x_space, plot_y_space)
    }
    
    for (plot_ind in 1:length(current_plot_starts)){
      
      plot_lab = worksheet_names[[sheet_ind]][[1]][current_plot_starts[plot_ind] - 2]
      rows_to_plot = current_plot_starts[plot_ind]:(current_plot_starts[plot_ind] + 3)
      
      plot_list = lapply(seq(author_num), function(i) numerical_data_matrix[[sheet_ind]][rows_to_plot, , i])
      current_mean_list = sheet_means[[sheet_ind]][rows_to_plot, ]
      
      if (plot_selection_type == 'by_author'){
        mean_plot_list = lapply(cols_to_plot, function(i) current_mean_list[, i])
        if ('plot_means' == TRUE){
          setup_sub_plots(nx = 1, ny = 1, x_space = plot_x_space, y_space = plot_y_space)
          overlay_plot_list(plot_type = 'non-overlay', mean_plot_list, x_vec = time_vec, yticks = 'y', y_lims, heading = 'mean profiles', ylab = '', x_lab = '', 
                            col_vec = rep('black', 3), lty_vec, lwd_vec = rep(mean_plot_lwd, length(plot_list)), 
                            legend_vec = 'NA', legend_loc = FALSE)
        } else {
          setup_sub_plots(nx = 3, ny = 2, x_space = plot_x_space, y_space = plot_y_space)
          for (author_ind in seq(author_num)){
            current_plot_list = lapply(cols_to_plot, function(i) plot_list[[author_ind]][, i])
            
            current_plot_name = plot_names[[sheet_ind]][[author_ind]][current_plot_starts[plot_ind] - 2]
            current_plot_name = gsub("Management scenario:", "", current_plot_name)
            overlay_plot_list(plot_type = 'non-overlay', current_plot_list, x_vec = time_vec, yticks = 'y', y_lims, heading = current_plot_name, ylab = '', x_lab = '', 
                              col_vec = rep(author_col[author_ind], 3), lty_vec, lwd_vec = rep(plot_lwd, length(plot_list)), 
                              legend_vec = 'NA', legend_loc = FALSE)
            
            overlay_plot_list(plot_type = 'overlay', mean_plot_list, x_vec = time_vec, yticks = 'y', y_lims, heading = current_plot_name, ylab = '', x_lab = '', 
                              col_vec = rep('black', 3), lty_vec, lwd_vec = rep(mean_plot_lwd, length(plot_list)), 
                              legend_vec = 'NA', legend_loc = FALSE)
            
          }
        }
        
      } else if (plot_selection_type == 'by_plot'){
        
        for (col_ind in cols_to_plot){
          current_plot_list = lapply(seq_along(plot_list), function(i) plot_list[[i]][, col_ind])
          print(column_names[col_ind])
          
          if (col_ind == 2){
            y_lab = plot_lab
            y_lab = gsub("Management scenario:", "", y_lab)
          } else {
            y_lab = ''
          }
          
          overlay_plot_list(plot_type = 'non-overlay', current_plot_list, x_vec = time_vec, yticks = 'y', y_lims, heading = column_names[col_ind], y_lab, x_lab = '', 
                            col_vec = author_col, lty_vec = rep(plot_lty, length(plot_list)), lwd_vec = rep(plot_lwd, length(plot_list)), 
                            legend_vec = 'NA', legend_loc = FALSE)
          overlay_plot_list(plot_type = 'overlay', list(current_mean_list[, col_ind]), x_vec = time_vec, yticks = 'y', y_lims, heading = column_names[col_ind], y_lab, x_lab = '', 
                            col_vec = 'black', lty_vec = mean_plot_lty, lwd_vec = mean_plot_lwd, 
                            legend_vec = 'NA', legend_loc = FALSE)
        }
        
      }
      
      
      cat(plot_lab, file = fileConn, sep = "\n")
      cat(paste(author_col, rep(':', author_num), unlist(worksheet_comments[[sheet_ind]][plot_ind])), file = fileConn, sep = "\n")
      cat('\n', file = fileConn, sep = "\n")
    } 
    title(worksheet_names[sheet_ind], outer=TRUE)
    if (write_pdf == TRUE) {
      graphics.off()
    }  
  }
  
  close(fileConn)
}
overlay_plot_list <- function(plot_type, plot_list, x_vec, yticks, y_lims, heading, ylab, x_lab, col_vec, lty_vec, lwd_vec, legend_vec, legend_loc){
  
  if (plot_type == 'non-overlay'){
    graphics::plot(y = plot_list[[1]], x = x_vec, type = 'l', main = heading, ylim = y_lims, ylab = ylab, xlab = x_lab, col = col_vec[1], lty = lty_vec[1], lwd = lwd_vec[1])
  } else {
    lines(y = plot_list[[1]], x = x_vec, type = 'l', main = heading, ylim = y_lims, ylab = ylab, xlab = x_lab, col = col_vec[1], lty = lty_vec[1], lwd = lwd_vec[1])
  }
  
  if (length(plot_list) > 1){
    for (plot_ind in 2:length(plot_list)){
      lines(y = plot_list[[plot_ind]],  x = x_vec, ylim = y_lims, col = col_vec[plot_ind], lwd = lwd_vec[plot_ind], lty = lty_vec[plot_ind])
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

# gs_auth(new_user = TRUE)

author_num = 7

author_col = c('darkblue',
               'red',
               'pink',
               'lightblue',
               'orange',
               'green',
               'darkgreen')

c("Elicitation_CP_Workshop_cmorris",
  "Elicitation_CP_Workshop_dkeith",
  "Elicitation_CP_Workshop_dkirk" ,     
  "Elicitation_CP_Workshop_gsteenbeeke",
  "Elicitation_CP_Workshop_jsanders",
  "Elicitation_CP_Workshop_pprice",     
  "Elicitation_CP_Workshop_pridgeway")

worksheet_names = c("Instructions", "1. TG-Low" , "2. TG-Med1", "3. TG-Med2", "4. TG-High","5. GG-Low","6. GG-Med1","7. GG-Med2",                           
                    "8. GG-High","9. FG-Low","10. FG-Med1","11. FG-Med2" ,                         
                    "12. FG-High","Cover Benchmarks","13. TG-Cover-Low" ,"14. TG-Cover-Med1" ,                   
                    "15. TG-Cover-Med2","16. TG-Cover-High", "17. GG-Cover-Low", "18. GG-Cover-Med1" ,                   
                    "19. GG-Cover-Med2","20. GG-Cover-High", "Further information", "Details of management actions" ,       
                    "Growth form categorization of species")

column_names = c('Year', 'Lower Bound',	'Upper Bound',	'Best Estimate',	'Confidence 50-100 (%)',	
                 '90% CI (LB)',	'90% CI (UB)', 'Uppermost Bound')
write_pdf = TRUE
pull_data = FALSE
plot_selection_type = 'by_plot'
plot_means = FALSE
include_random_data = FALSE
output_pdf_filename = 'CP_elicitation_workshop_1.pdf'
file_prefix = 'Elicitation_CP_Workshop_'
worksheets_to_pull = c(15:22)
sheet_num = length(worksheets_to_pull)
authors_to_pull = 5

plot_lwd = 2
plot_lty = 1
mean_plot_lwd = 3
mean_plot_lty = 2

cols_to_plot = c(2, 3, 4)
time_vec = c(0, 20, 40, 60)
lty_vec = c(1, 1, 2)

plot_x_space = 2
plot_y_space = 1.5

if (pull_data == TRUE){
  pull_worksheets(file_prefix, sheet_characteristics, authors_to_pull, worksheets_to_pull)
}

worksheets_to_collate = c(2:13, 15:22)

sheet_data <- collate_sheet_data(worksheets_to_collate, author_num)

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
                                               function(j) lapply(seq(author_num), 
                                                                  function(k) comments[[i]][[k]][j])))
    
sheet_y_lims = lapply(seq_along(sheet_maxs), function(i) c(0, max(sheet_maxs[[i]][, 3], na.rm = TRUE)))

plot_sheet_data(numerical_data_matrix, plot_sheet_num = length(worksheets_to_collate), author_num, plot_x_space, plot_y_space, time_vec, comments, 
                sheet_mins, sheet_maxs, sheet_means, plot_starts, plot_nums, worksheet_comments, worksheet_names_to_use, sheet_y_lims)
  

time_vec_new = 1:100 
fit_type = 'by_mean' 

column_to_use = 3

spline_fits = fit_splines(numerical_data_matrix, 
            plot_sheet_num = length(worksheets_to_collate), 
            time_vec_new, 
            fit_type, 
            plot_starts, 
            column_to_use)
                          
plot_splines(spline_fits, numerical_data_matrix, plot_sheet_num, plot_x_space, plot_y_space, plot_starts, plot_names = list(), 
             fit_type, author_num, sheet_means, time_vec, time_vec_new, sheet_y_lims, worksheet_names_to_use)




