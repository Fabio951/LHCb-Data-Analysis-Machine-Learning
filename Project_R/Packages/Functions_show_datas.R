######################################
############ Show Datas ##############
######################################


show_datas <- function(datas, density=TRUE, cuts=FALSE, choose_logs=FALSE){
  col_names <- names(datas[[1]])
  if ('Label' %in% col_names){
    col_names <- col_names[-length(col_names)]
  }
  ranges <- rep(NULL, times=length(col_names))
  if (cuts){
    ranges <- list(NULL, NULL, c(3000,8000), NULL, c(-15,25), NULL,
                   c(-1.5,1.5), c(-2,2), c(0,10), NULL, NULL, NULL,
                   NULL, NULL, NULL, NULL, NULL, NULL,
                   NULL, NULL, NULL, NULL, NULL, NULL, NULL
    )
  }
  logs <- rep("", times=length(col_names))
  if (choose_logs){
    logs <- list('y', 'y', 'y', 'y', "", "",
                 "", "", "", "", "", "",
                 'y', 'y', "", 'y', 'y', 'y',
                 'y', 'y', 'y', 'y', "", "", ""
    )
  }
  transparency <- 0.6
  colors <- c(rgb(0,0,1,transparency+0.2), rgb(1,1,0,transparency), rgb(0.2,0.7,0.3,transparency), rgb(0.5,0.5,1,transparency))
  angle <- c(45, -45, 0)
  p <- matrix(1:4, byrow=T, ncol=2)
  layout(p)
  for (i in 1:length(col_names)){
    
    x_min <- 1e10
    x_max <- -1e10
    for (l in 1:length(datas)){
      x_val_min <- min(datas[[l]][[col_names[i]]])
      if (x_val_min<x_min){
        x_min <- x_val_min
      }
      x_val_max <- max(datas[[l]][[col_names[i]]])
      if (x_val_max>x_max){
        x_max <- x_val_max
      }
    }
    if (is.null(ranges[[i]])){
      xlimits <- c(x_min, x_max)
    }
    else{
      xlimits <- ranges[[i]]
    }
    
    safe_band <- (xlimits[[2]]-xlimits[[1]])*0.1
    breaks <- seq(xlimits[[1]]-safe_band, xlimits[[2]]+safe_band, length.out=60)
    
    hists_counts = list()
    hists_densities = list()
    max_counts <- 0
    max_density <- 0
    for (k in 1:length(datas)){
      filter_ <- (datas[[k]][[col_names[[i]]]]>xlimits[[1]]) & (datas[[k]][[col_names[[i]]]]<xlimits[[2]])
      hist_counts <- hist(datas[[k]][[col_names[[i]]]][filter_],
                          freq = !density,
                          xlim = xlimits,
                          breaks = breaks,
                          plot = FALSE,
                          warn.unused = FALSE)
      max_dens <- max(hist_counts$density)
      max_count <- max(hist_counts$counts)
      if (max_dens>max_density){
        max_density <- max_dens
      }
      if (max_count>max_counts){
        max_counts <- max_count
      }
      
    }
    
    if (density){
      ylimits <- c(0, max_density)
    }
    else{
      ylimits <- c(0, max_counts)
    }
    
    for (j in 1:length(datas)){
      add <- ifelse(j==1, FALSE, TRUE)
      ylab <- ifelse(density, 'Frequency', 'Counts')
      filter_ <- (datas[[j]][[col_names[[i]]]]>xlimits[[1]]) & (datas[[j]][[col_names[[i]]]]<xlimits[[2]])
      hist(datas[[j]][[col_names[[i]]]][filter_],
           col = colors[j],
           freq = !density,
           #log = 'y',
           xlim = xlimits,
           ylim = ylimits,
           add = add,
           main = col_names[i],
           xlab = 'values',
           ylab = ylab,
           breaks = breaks#,
           #density = 20,
           #angle = angle[j]
      )
    }
    legend("topright", names(datas), fill=colors, cex=0.5)
  }
}


#############################################
############ Show Correlations ##############
#############################################


correlations <- function(datas_or, show=TRUE, columns_to_drop=TRUE, only_useful=TRUE, threshold=0.8){
  if (only_useful){
    datas <- clean_datas(datas_or, only_useful=TRUE)
  }
  else{
    datas <- datas_or
  }
  col_names <- names(datas)
  colnames(datas) <- 1:length(col_names)
  corr <- cor(datas)
  if (show){
    p <- matrix(c(1,2,2), byrow=T, ncol=3)
    layout(p)
    plot(1:length(datas), 1:length(datas), xaxt='n', ann=FALSE, bty='n', yaxt='n', cex=0)
    for (i in 1:length(col_names)){
      text(7,(length(col_names)-i)/2 + length(col_names)/3.3,
           sprintf('%i_ %s', i, col_names[[i]]), cex=.8, font=2)
    }
    corrplot.mixed(corr, tl.col = "black", tl.cex = 1, number.cex = 1) 
  }
  if (columns_to_drop){
    corr_df <- as.data.frame(corr)
    drop_col <- list()
    for (i in 1:(length(col_names)-2)){
      if (max(corr_df[[i]][-(1:i)])>threshold){
        drop_col <- c(drop_col, col_names[[i]])
      }
    }
    return(drop_col)
  }
}




#########################################
############ Show Datas MOD##############
#########################################


show_datas_mod <- function(datas, density=TRUE, cuts=FALSE, choose_logs=FALSE){
  col_names <- names(datas[[1]])
  if ('Label' %in% col_names){
    col_names <- col_names[-length(col_names)]
  }
  transparency <- 0.6
  colors <- c(rgb(0,0,1,transparency+0.2), rgb(1,1,0,transparency), rgb(0.2,0.7,0.3,transparency), rgb(0.5,0.5,1,transparency))
  angle <- c(45, -45, 0)
  p <- matrix(1:4, byrow=T, ncol=2)
  layout(p)
  for (i in 1:length(col_names)){
    
    hists_counts = list()
    hists_densities = list()
    max_counts <- 0
    max_density <- 0
    for (k in 1:length(datas)){
      hist_counts <- hist(datas[[k]][[col_names[[i]]]],
                          freq = !density,
                          plot = FALSE,
                          warn.unused = FALSE)
      max_dens <- max(hist_counts$density)
      max_count <- max(hist_counts$counts)
      if (max_dens>max_density){
        max_density <- max_dens
      }
      if (max_count>max_counts){
        max_counts <- max_count
      }
      
    }
    
    if (density){
      ylimits <- c(0, max_density)
    }
    else{
      ylimits <- c(0, max_counts)
    }
    
    for (j in 1:length(datas)){
      add <- ifelse(j==1, FALSE, TRUE)
      ylab <- ifelse(density, 'Frequency', 'Counts')
      hist(datas[[j]][[col_names[[i]]]],
           col = colors[j],
           freq = !density,
           #log = 'y',
           ylim = ylimits,
           add = add,
           main = col_names[i],
           xlab = 'values',
           ylab = ylab
           #density = 20,
           #angle = angle[j]
      )
    }
    legend("topright", names(datas), fill=colors, cex=0.5)
  }
}




















