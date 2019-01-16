# load package
library(googlesheets)
library(plyr)
library(abind)



fit_splines <- function(data_to_fit, plot_sheet_num, time_vec, fit_type, plot_starts, columns_to_use, author_ind){

  sheet_fits = vector('list', plot_sheet_num)
  
  for (sheet_ind in 1:plot_sheet_num){
     
    current_data_starts = plot_starts[[sheet_ind]]
    management_classs_fits = vector('list', length(current_data_starts))
    
    for (management_class in 1:length(current_data_starts)){
      
      current_data_locations = current_data_starts[management_class]:(current_data_starts[management_class] + 3)
      current_management_classs_fits = vector('list', length(columns_to_use))
#       if (sheet_ind == 13){
#         browser()
#       }
      
      for (spline_ind in seq_along(columns_to_use)){
        
        if (fit_type == 'by_author'){
          current_data = data_to_fit[[sheet_ind]][current_data_locations, columns_to_use[spline_ind], author_ind]
        } else if (fit_type == 'by_mean'){

          current_data = data_to_fit[[sheet_ind]][current_data_locations, columns_to_use[spline_ind]]
        }
        
        if (all(!is.na(current_data))){

          current_data = append(current_data, current_data[length(current_data)])
          #current_management_classs_fits[[spline_ind]] <- smooth.spline(time_vec, current_data, df = length(current_data)) 
          current_management_classs_fits[[spline_ind]] <- approxfun(x = time_vec, y = current_data, method = "linear")
          
        }
        
      }
      
      management_classs_fits[[management_class]] = current_management_classs_fits
    }

    sheet_fits[[sheet_ind]] = management_classs_fits
  }

  return(sheet_fits)
}

# -------------------------------------------------


generate_current_spline <- function(current_plot_spline, time_vec_interpolated){
  
  if (length(current_plot_spline) > 0){
    current_plot = predict(current_plot_spline, time_vec_interpolated)
  } else {
    current_plot = list(rep(0, length(time_vec_interpolated)))
    names(current_plot) = 'y'
  }
  
  current_plot_list = current_plot$y
  return(current_plot_list)
}


plot_spline_data <- function(author_spline_fits, mean_spline_fits, plot_author, plot_mean, numerical_data_matrix, plot_sheet_num, plot_x_space, plot_y_space, plot_starts, 
                         author_names, author_ind, sheet_means, time_vec, time_vec_interpolated, sheet_y_lims, worksheet_names){
  
  if (write_pdf == TRUE){
    spline_plot_filename = paste0('plots/spline_fits_')
    if (plot_author == TRUE){
      spline_plot_filename = paste0(spline_plot_filename, author_names[author_ind])
    }
    if (plot_mean == TRUE){
      spline_plot_filename = paste0(spline_plot_filename, '_mean')
    }
    
    spline_plot_filename = paste0(spline_plot_filename, '.pdf')
    pdf(spline_plot_filename, width = 8.3, height = 11.7)
  }
  
  setup_sub_plots(nx = 3, ny = 3, plot_x_space, plot_y_space)
  
  ct.index  <- 1 # index to keep track of how to access info from the ct dataframe (with the condition thresholds)
  sheet.ctr <- 1 # counter to keep track of sheets and refresh the plot window after every 4 plots.
  
  for (sheet_ind in 1:plot_sheet_num){
    
    y_lims = sheet_y_lims[[sheet_ind]]
    
    # Refresh the plot window every 4 plots
    cat('\n sheet_ind =', sheet_ind, 'worksheet_names =', worksheet_names[sheet_ind], ' ct.index=', ct.index, 
        'sheet.ctr =', sheet.ctr)
    
    #if( sheet_ind %% 5 == 0) {
    if( sheet.ctr==5 ) {
      setup_sub_plots(nx = 3, ny = 3, plot_x_space, plot_y_space)
      cat('**resetting plot window')
      #ct.index <- ct.index + sheet_ind - 1
      ct.index <- ct.index  + 4
      sheet.ctr <- 1
    }
    sheet.ctr <- sheet.ctr + 1
    current_plot_starts = plot_starts[[sheet_ind]]
    
    plot_headings <- c('unamanged', 'low intensity mgmnt', 'high intensity mgmnt')
    for (management_class in 1:length(current_plot_starts)){
      plot_lab = ''
      current_plot_vec = current_plot_starts[management_class]:(current_plot_starts[management_class] + 3)
      if (management_class == 1){
        y_lab = worksheet_names[sheet_ind]
      } else {
        y_lab = ''
      }
      
      plot_heading = plot_headings[management_class]
      
      current_plot_set = list()
      if (plot_author == TRUE){
        current_plot_list = generate_current_spline(current_plot_spline = author_spline_fits[[sheet_ind]][[management_class]], time_vec_interpolated)
        current_plot_set = append(current_plot_set, list(current_plot_list))
      } 
      
      if (plot_mean == TRUE){
        current_plot_list = generate_current_spline(current_plot_spline = mean_spline_fits[[sheet_ind]][[management_class]], time_vec_interpolated)
        current_plot_set = append(current_plot_set, list(current_plot_list))
      }
      
      overlay_plot_list(plot_type = 'non-overlay', current_plot_set, x_vec = time_vec_interpolated, yticks = 'y', y_lims, heading = plot_heading, y_lab, x_lab = '', 
                        col_vec = rep('black', 2), lty_vec = c(1, 2), lwd_vec = rep(plot_lwd, length(current_plot_set)), 
                        legend_vec = 'NA', legend_loc = FALSE)
      
      
      if (plot_author == TRUE){
        current_author_plot_points = numerical_data_matrix[[sheet_ind]][current_plot_vec, column_to_use, author_ind]
        points(time_vec, current_author_plot_points, cex=1.8, pch=19)
      } 
      
      if (plot_mean == TRUE){
        current_mean_plot_points = sheet_means[[sheet_ind]][current_plot_vec, column_to_use]
        points(time_vec, current_mean_plot_points, cex=1.8, pch=1)
      }
      
      
      # x <- c(0, 0, 100, 100)
      x <- c(time_vec_interpolated[1], time_vec_interpolated[1], time_vec_interpolated[length(time_vec_interpolated)], time_vec_interpolated[length(time_vec_interpolated)])
      
      
      # low condition
      y <- c(ct['lower.bound',ct.index], ct['upper.bound',ct.index], ct['upper.bound',ct.index],  ct['lower.bound',ct.index] )
      polygon( x, y, col=rgb(1, 0, 0,0.2), border=NA )
      abline(h=ct['initial.val',ct.index], col=rgb(1, 0, 0, 0.4), lwd=2, lty=1)
      text(9,ct['lower.bound',ct.index]+0.1, 'Low', cex=1.2, col=rgb(1, 0, 0,1) )
      
      # med1 condition
      y <- c(ct['lower.bound',ct.index+1], ct['upper.bound',ct.index+1], ct['upper.bound',ct.index+1],  ct['lower.bound',ct.index+1] )
      polygon( x, y, col=rgb(0, 0, 1, 0.4), border=NA, density=25 )
      abline(h=ct['initial.val',ct.index+1], col=rgb(0, 0, 1, 0.4), lwd=2, lty=1)
      text(9,ct['upper.bound',ct.index+1]+0.3, 'Med1', cex=1.2, col=rgb(0, 0, 1, 1))
      
      
      # high condition
      y <- c(ct['lower.bound',ct.index+3], ct['upper.bound',ct.index+3], ct['upper.bound',ct.index+3],  ct['lower.bound',ct.index+3] )
      polygon( x, y, col=rgb(0, 1, 0,0.2), border=NA )
      abline(h=ct['initial.val',ct.index+3], col=rgb(0, 1, 0, 0.4), lwd=2, lty=1)
      text(9,ct['upper.bound',ct.index+3]+0.1, 'High', cex=1.2, col=rgb(0, 1, 0, 1))
      
      
      # med2 condition
      rgbcol <- rgb(0.7, 0.3, 0.5, 0.4)
      y <- c(ct['lower.bound',ct.index+2], ct['upper.bound',ct.index+2], ct['upper.bound',ct.index+2],  ct['lower.bound',ct.index+2] )
      # polygon( x, y, col=rgbcol, border=NA)
      polygon( x, y, col=rgbcol, border=NA, density=10, angle=120)
      abline(h=ct['initial.val',ct.index+2], col=rgbcol, lwd=2, lty=1)
      text(85,ct['lower.bound',ct.index+2]-0.3, 'Med 2', cex=1.2, col=rgb(0.7, 0.3, 0.5), font.lab = 2 )
      
      #browser()
      
    }
    #title(worksheet_names[sheet_ind], outer=TRUE)
  }
  
  
  if (write_pdf == TRUE){
    graphics.off()
  }
}

# -------------------------------------------------


pull_worksheets <- function(file_prefix, strings_to_exclude, sheet_characteristics, authors_to_pull, worksheets_to_pull){

  sheet_characteristics = gs_ls()
  author_sheets_to_use = grepl(file_prefix, sheet_characteristics$sheet_title)
  if (is.character(strings_to_exclude)){
    author_sheets_to_exclude = grepl(strings_to_exclude, sheet_characteristics$sheet_title)
    author_sheets_to_use = author_sheets_to_use & !author_sheets_to_exclude
  } 
  
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
      Sys.sleep(10)
    }
    
  }
  
}

# -------------------------------------------------

collate_sheet_data <- function(worksheets_to_collate, authors_to_plot){
  plot_sheet_num = length(worksheets_to_collate)
  sheet_data = vector('list', plot_sheet_num)
  author_num = length(authors_to_plot)
  for (current_sheet_ind in seq_along(worksheets_to_collate)){
    current_sheet_data = vector('list', author_num)
    for (author_ind in seq(author_num)){
      
      current_filename = paste0('author_responses/author_', authors_to_plot[author_ind], '_sheet_', worksheets_to_collate[current_sheet_ind], '.csv')
      
      tmp_data = read.csv(current_filename, na.strings=c("","NA"), stringsAsFactors = FALSE)
      tmp_data = tmp_data[, 1:8]
      names(tmp_data) = column_names
      current_sheet_data[[author_ind]] = tmp_data
    }
    sheet_data[[current_sheet_ind]] = current_sheet_data
  }
  return(sheet_data)
}

# -------------------------------------------------


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
    
    for (management_class in 1:length(current_plot_starts)){
      
      plot_lab = worksheet_names[[sheet_ind]][[1]][current_plot_starts[management_class] - 2]
      rows_to_plot = current_plot_starts[management_class]:(current_plot_starts[management_class] + 3)
      
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
            
            current_plot_name = plot_names[[sheet_ind]][[author_ind]][current_plot_starts[management_class] - 2]
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
      cat(paste(author_col, rep(':', author_num), unlist(worksheet_comments[[sheet_ind]][management_class])), file = fileConn, sep = "\n")
      cat('\n', file = fileConn, sep = "\n")
    } 
    title(worksheet_names[sheet_ind], outer=TRUE)
    if (write_pdf == TRUE) {
      graphics.off()
    }  
  }
  
  close(fileConn)
}

# -------------------------------------------------

overlay_plot_list <- function(plot_type, plot_list, x_vec, yticks, y_lims, heading, ylab, x_lab, col_vec, lty_vec, lwd_vec, legend_vec, legend_loc){
  
  if (plot_type == 'non-overlay'){
    graphics::plot(y = plot_list[[1]], x = x_vec, type = 'l', main = heading, ylim = y_lims, ylab = ylab, xlab = x_lab, col = col_vec[1], lty = lty_vec[1], lwd = lwd_vec[1])
  } else {
    lines(y = plot_list[[1]], x = x_vec, type = 'l', main = heading, ylim = y_lims, ylab = ylab, xlab = x_lab, col = col_vec[1], lty = lty_vec[1], lwd = lwd_vec[1])
  }
  
  if (length(plot_list) > 1){
    for (management_class in 2:length(plot_list)){
      lines(y = plot_list[[management_class]],  x = x_vec, ylim = y_lims, col = col_vec[management_class], lwd = lwd_vec[management_class], lty = lty_vec[management_class])
    }
  }
  
  abline(h = 0, lty = 2)
  if (legend_vec[1] != 'NA'){
    legend(legend_loc, legend_vec, bty="n", lty = lty_vec, cex = 1,  pt.cex = 1, lwd = lwd_vec, col = col_vec)
  }
  
  
}

# -------------------------------------------------

setup_sub_plots <- function(nx, ny, x_space, y_space){
  par(mfrow = c(ny, nx))
  par(cex = 0.6)
  par(mar = c(x_space, y_space, 1, 0), oma = c(1, 3, 1, 1), font.lab = 2 )
  
  par(tcl = -0.25)
  par(mgp = c(2, 0.3, 0))
  
}

# -------------------------------------------------


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
write_pdf = TRUE
pull_data = TRUE
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
authors_to_plot = 1
time_vec_interpolated = 0:100
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


for (author_ind in authors_to_pull){
    author_spline_fits = fit_splines(data_to_fit = numerical_data_matrix, plot_sheet_num, time_vec, fit_type = 'by_author',  plot_starts, columns_to_use = c(2, 3, 4), author_ind)
    saveRDS(object = author_spline_fits, paste0(author_names[author_ind], '_splines.rds'))
}

mean_spline_fits = fit_splines(data_to_fit = sheet_means, plot_sheet_num, time_vec, fit_type = 'by_mean',  plot_starts, columns_to_use = c(2, 3, 4), author_ind = 1)
saveRDS(object = mean_spline_fits, paste0('mean_splines.rds'))

# source('cond.thresholds.R')
# 
# if (plot_sheets == TRUE){
#   plot_sheet_data(numerical_data_matrix, plot_sheet_num = length(worksheets_to_collate),   author_num = length(authors_to_plot), plot_x_space, plot_y_space, time_vec, comments, 
#                 sheet_mins, sheet_maxs, sheet_means, plot_starts, plot_nums, worksheet_comments, worksheet_names_to_use, sheet_y_lims)
# }
# 
# if (plot_splines == TRUE){
#   plot_spline_data(author_spline_fits, mean_spline_fits, plot_author, plot_mean, numerical_data_matrix, plot_sheet_num, plot_x_space, plot_y_space, plot_starts,
#              author_names, author_ind, sheet_means, time_vec, time_vec_interpolated, sheet_y_lims, worksheet_names_to_use)
# }

