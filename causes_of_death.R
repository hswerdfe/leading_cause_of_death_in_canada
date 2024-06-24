
    library(cansim)
    library(tidyverse)
    library(ggplot2)
    library(janitor)
    #library(lubridate)
    #library(glue)
    #library(feasts)
    #library(fpp3)
    library(ggrepel)
    library(stringr)
    library(magick)
    library(gganimate)
    
    # Leading causes of death, total population, by age group
    # https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310039401&pickMembers%5B0%5D=2.1&pickMembers%5B1%5D=3.1&cubeTimeFrame.startYear=2000&cubeTimeFrame.endYear=2022&referencePeriods=20000101%2C20220101
    
    #####################
    # define some constants
    table_number <- '13-10-0394'
    transition_length = 0
    state_length = 2
    start_pause = 10
    end_pause = 30
    final_h = 800 *1.5
    final_w = 1600 * 1.5
    
    caption_txt = paste0('cansim:', table_number)
    
    ###########################################
    #
    # custom Colors (I asked ChatGPT version 🤷 to give me 15 hex colors to use, in a color map in 15 categorical strings. )
    #
    custom_colors <- c(
      "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
      "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
      "#f7b6d2", "#c7c7c7", "#ff9896", "#c49c94", "#98df8a"
    )
    
    #############################
    # 
    # Set some theme stuff for ggplot
    #
    theme_set(theme_minimal() +
                theme(
                  strip.background = element_blank(),
                  strip.text = element_blank(),
                  axis.title = element_blank(),
                  axis.text.y = element_blank(),,
                  axis.text.x = element_blank(),
                  panel.grid = element_blank(), 
                  plot.title = element_text(size = 25, hjust = 0.5, color = 'black'),
                  plot.subtitle =  element_text(size = 15, hjust = 0.5,, color = 'grey'),
                  
                ) 
    ) 
    
    
    
    
    ##################
    # Fetch Data from Internet
    df_raw <- 
      cansim::get_cansim(table_number) |> 
      janitor::clean_names()
    
    
    
    ##########################
    # Use this data for Everything else from now on
    p_dat <- 
      df_raw |>
      mutate(leading_causes_of_death_icd_10 = str_replace_all(leading_causes_of_death_icd_10, "\\[.*?\\]", "")) |> 
      mutate(leading_causes_of_death_icd_10 = str_replace_all(leading_causes_of_death_icd_10, "\\(.*?\\)", "")) |> 
      mutate(leading_causes_of_death_icd_10 = str_squish(leading_causes_of_death_icd_10)) |> 
      filter(characteristics ==  'Number of deaths' & sex != 'Both sexes') |>
      filter(str_count(hierarchy_for_age_at_time_of_death, '\\.') == 2) |> 
      filter(str_count(hierarchy_for_leading_causes_of_death_icd_10, '\\.') == 1) |>
      mutate(yr = as.integer(ref_date)) |> 
      #filter(yr == max(yr)) |> 
      #filter(age_at_time_of_death == 'Age at time of death, 45 to 49 years') |> 
      select(yr, sex, age_at_time_of_death, value, leading_causes_of_death_icd_10) |>
      mutate(age_at_time_of_death = str_squish(str_remove(str_remove(age_at_time_of_death, 'Age at time of death, '), 'years'))) |>
      mutate(age_min = as.integer(str_extract(age_at_time_of_death, '\\w+'))) |>
      mutate(age_at_time_of_death = fct_reorder(age_at_time_of_death, age_min)) |>
      arrange(value) |> 
      filter(value > 0) |>
      mutate(life_yr_taken = (90-age_min)*value) |>
      mutate(leading_causes_of_death_icd_10 = fct_lump_n(leading_causes_of_death_icd_10, 15, w = value, other_level = 'Other causes of death')) |>
      mutate(leading_causes_of_death_icd_10 = fct_reorder(leading_causes_of_death_icd_10, value, .fun = sum)) |>
      summarise(value = sum(value), 
                life_yr_taken = sum(life_yr_taken),
                .by = c(yr, sex, age_at_time_of_death, leading_causes_of_death_icd_10, age_min))# |>
      
    
    ######################
    # generate data frame for Yearly Labels in animation
    yr_lbl <- 
      p_dat |>
      mutate(value = sum(value), .by = c(yr, age_at_time_of_death, sex)) |>
      mutate(value = max(value)/2, 
             age_at_time_of_death = mean(range(as.integer(age_at_time_of_death)))) |>
      distinct(yr, sex, value, age_at_time_of_death) 
    
    
    
    
    
    
    
    ######################################
    # Generates the Base graph by Age and time and cause
    p_age <-   
      p_dat |> 
      ggplot(aes(y = age_at_time_of_death, x = value)) +
      geom_col(aes(fill = leading_causes_of_death_icd_10), alpha = 0.5, color = 'black') +
      geom_text(data = yr_lbl,
                mapping = aes(label = paste(sex, yr, sep = '\n')),
                color = 'grey', size = 15
      ) +
      scale_fill_manual(values = custom_colors) +
      facet_grid(vars(sex)) +
      labs(
          y = 'Age at Time of Death',
          x = '', 
          title = 'Deaths by Age and Cause', 
          subtitle = 'in Canada',
          caption = caption_txt
      ) +
      guides(color = 'none', fill = 'none') +
      theme(
        axis.title = element_text(size = 25, color = 'black'),
        axis.text.y = element_text(size = 10),
        panel.grid = element_blank()
      ) + 
      transition_states(yr, transition_length =transition_length, state_length = state_length) 
    
    
    ######################################
    # Write the giff to file
    p_age |> 
      animate(end_pause = end_pause, start_pause = start_pause, height = final_h, width =final_w/2) |> 
      image_write(path="deaths/cause_and_age.gif")
    
    
    
    ######################################
    # generate a dataframe that will repot the year in the label
    p_hist_data_yr_lbl <- 
      p_dat |>
      summarise(value = sum(value), .by = c(yr, leading_causes_of_death_icd_10)) |>
      mutate(value = max(value)/2) |>
      mutate(leading_causes_of_death_icd_10 = mean(range(as.integer(leading_causes_of_death_icd_10)))) |>
      distinct() 
    
    
    
    
    
    
    ######################################
    # Generate column chart of leading causes of death
    p_hist <- 
      expand_grid(yr = unique(p_dat$yr), 
                  leading_causes_of_death_icd_10 = unique(p_dat$leading_causes_of_death_icd_10     )
      ) |> 
      left_join(p_dat, by = c('yr', 'leading_causes_of_death_icd_10')) |>
      summarise(value = sum(value), 
                .by = c(yr, leading_causes_of_death_icd_10)) |>
      ggplot(aes(x = value, y = leading_causes_of_death_icd_10)) +
      geom_text(data = p_hist_data_yr_lbl, mapping = aes(
        label = paste(yr, sep = '\n')
      ),
        color = 'grey',
        size = 20
      ) +
      geom_col(aes(fill = leading_causes_of_death_icd_10),color = 'black', alpha = 0.5) + 
      geom_label(aes(label = round(value/1000,1), size = 12, color = leading_causes_of_death_icd_10), fill = 'white') +
      geom_text(aes(x = 0, label = leading_causes_of_death_icd_10, color = leading_causes_of_death_icd_10), hjust = 1, nudge_x = -1000, size = 6) +
      #facet_grid(cols = vars(sex)) +
      scale_y_discrete() +
      scale_x_continuous(limits = c(-70000,NA)) +
      scale_fill_manual(values = custom_colors) +
      scale_color_manual(values = custom_colors) +
      labs(
          title = 'Leading Cause of Death', 
          subtitle = 'Thousands per Year (Canada)',      caption = caption_txt,
          y ='', x = '') +
      guides(fill = 'none', size = 'none', alpha = 'none', color = 'none') +
      transition_states(yr, transition_length = transition_length, state_length = state_length) 
      
    
      
    
    p_hist |> 
      animate(end_pause = end_pause, start_pause = start_pause, height = final_h/2, width =final_w/2) |>
      image_write(path="deaths/cause_hist.gif")
    
    
    
    
    ##################################
    # Time labels datafame for the time chart
    time_lbls <- 
      p_dat  |> 
      summarise(num = sum(value), .by = yr) |>
      mutate(x_yr = yr, value = num) |>
      mutate(yr = mean(range(yr)), value = max(value)/2 )
    
    
    
    p_time <- 
      p_dat |> 
      summarise(value = sum(value), .by = c(yr, leading_causes_of_death_icd_10)) |>
      mutate(value_total = sum(value), .by = yr) |>
      cross_join(tibble(x_yr = unique(p_dat$yr) )) |>
      mutate(this_year = yr == x_yr) |>
      #filter(x_yr == max(x_yr)) |>
      ggplot(aes(x = yr, y = value)) +
      geom_col(aes(fill = leading_causes_of_death_icd_10, alpha = this_year),color = 'black') + 
      geom_label(nudge_y = 12500, alpha = 0.85 , 
                 data = time_lbls, 
                 mapping = aes(
                   label = paste(format(num, big.mark = ','), x_yr, sep = '\n') 
                  ), 
                  size = 15, 
                  color = 'grey'
      ) +
      guides(fill = 'none', alpha = 'none') +
      scale_alpha_ordinal(range = c(0.2, 1)) +
      scale_x_continuous(breaks = range(p_dat$yr)) + 
      scale_fill_manual(values = custom_colors) +
      scale_color_manual(values = custom_colors) +
      labs(
          title = 'Total Deaths', 
          subtitle =  'Canada',
          caption = caption_txt
      ) +
      theme(
        axis.text.x = element_text(size = 20, color = 'darkgrey')
      ) +
      transition_states(x_yr, transition_length = transition_length, state_length = state_length) 
    
    
    p_time |> animate(end_pause = end_pause, start_pause = start_pause, height = final_h/2, width =final_w/2) |>
      image_write(path="deaths/time_bar.gif")
    
    
    
    
    # Load the GIFs
    gif_left <- image_read("deaths/cause_and_age.gif")
    gif_top_right <- image_read("deaths//cause_hist.gif")
    gif_bottom_right <- image_read("deaths/time_bar.gif")
    
    
    
    # Get the Frame count
    n_frames <- max(length(gif_left), length(gif_top_right), length(gif_bottom_right))
    
    
    gif_left <- image_coalesce(gif_left)
    gif_top_right <- image_coalesce(gif_top_right)
    gif_bottom_right <- image_coalesce(gif_bottom_right)
    
    
    
    
    combined_frames <- list()
    
    
    # Combine each frame individually
    for (i in 1:n_frames) {
      left_frame <- gif_left[i]
      top_right_frame <- gif_top_right[i]
      bottom_right_frame <- gif_bottom_right[i]
      
      right_combined <- image_append(c(bottom_right_frame, top_right_frame), stack = TRUE)
      combined <- image_append(c(right_combined, left_frame), stack = FALSE)
      
      combined_frames[[i]] <- combined
    }
    
    
    # Join the frames into a single image and animate
    combined_gif <- image_animate(image_join(combined_frames), fps = 10)
    
    image_write(combined_gif, "deaths/combined.gif")
