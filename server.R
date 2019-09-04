if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggforce)) install.packages("ggforce")
library(tidyverse)
library(ggforce)
theme_set(theme_gray(base_size = 15))

circle_coords = function(x, y, r, n_points = 100) {
  if(n_points %% 2 != 0) stop("n_points must be an even number")
  xapu = sin(seq(0, pi, length = n_points/2) - pi / 2)
  xv1 = x + xapu * r
  xv2 = x - xapu * r
  yv1 = sqrt(pmax(0, r ^ 2 - (xv1 - x) ^ 2)) + y
  yv2 = -sqrt(pmax(0, r ^ 2 - (xv2 - x) ^ 2)) + y
  yv = c(yv1, yv2)
  xv = c(xv1, xv2)
  tibble::tibble(xv = xv, yv = yv)
}

# data
d2 = read_csv("SCBI_initial_woody_stem_census_2012.csv", col_types = list())
d3 =  filter(d2, Status == "alive", Stem == "main",
             gy >= 240, DBH > 0) %>%  # 400m by 400m = 16 ha
  mutate(dbh_cm = DBH/10, gy = gy - 240, id = 1:n()) # dbh was in mm
d3 = rename(d3, x = gx, y = gy)
d3 = mutate(d3, basal_area_m2 = pi * (dbh_cm/200)^2)
b_true = sum(d3$basal_area_m2)/16 # 32.738
n_tree_ha = nrow(d3)/16


function(input, output) {
  # save results
  basal_areas = reactiveValues(res = tibble(true_BA = numeric(),
                                            fixed_BA = numeric(),
                                            variable_BA = numeric(),
                                            true_N = numeric(),
                                            fixed_N = numeric(),
                                            variable_N = numeric()))
  
  all_dat = eventReactive(input$run, {
    # select one point
    # area of fixed-radius plot
    xcoord = input$xcoord
    ycoord = input$ycoord
    radius = input$radius
    baf = as.integer(input$baf)
    area_fixed = pi * radius^2
    kl = c(0.02, 0.0283, 0.0346, 0.04, 0.0447, 0.0490)
    k = kl[baf]
    d3 = mutate(d3, r_circle = dbh_cm/(100 * k)) # variable-radius values
    
    # trees to be included in fixed-radius (r = 5m) plot
    d4 = filter(d3, x < xcoord + radius + 1, 
                x > xcoord - radius - 1,
                y < ycoord + radius + 1, 
                y > ycoord - radius -1)
    d4 = d4[raster::pointDistance(d4[, c("x", "y")], c(xcoord, ycoord), lonlat = F) <= radius, ]
    d3 = mutate(d3, in_circle = ifelse(id %in% d4$id, T, F))
    
    # trees to be included in variable-radius plot
    r2 = max(d3$r_circle)
    d5 = filter(d3, x < xcoord + r2 + 1, x > xcoord - r2 - 1,
                y < ycoord + r2 + 1, y > ycoord - r2 -1)
    d5$dis = raster::pointDistance(d5[, c("x", "y")], c(xcoord, ycoord), lonlat = F)
    d5 = filter(d5, r_circle >= dis) #those will be tallied
    d5 = mutate(d5, inclusion_area = pi * r_circle^2,
                n_tree = 10000/inclusion_area)
    
    # basal area per hectare
    # fixed-radius plot
    b_fixed = (sum(d4$basal_area_m2)/area_fixed) * 10000 # m2/ha
    n_fixed = 10000 * nrow(d4)/area_fixed # n tree / ha
    
    # variable-radius plot
    b_variable = baf * length(d5$basal_area_m2) # m2/ha
    n_variable = sum(d5$n_tree)
    
    out = data.frame(x = xcoord, y = ycoord,
                     true_BA = b_true, 
                     fixed_BA = b_fixed,
                     variable_BA = b_variable,
                     true_N = n_tree_ha,
                     fixed_N = n_fixed,
                     variable_N = n_variable)
    basal_areas$res = bind_rows(basal_areas$res, out)
    list(out = out, fixed_data = d4, variable_data = d5, all_data = d3,
         xp = xcoord, yp = ycoord, r = radius)
    })
  
  output$basalAreas = renderTable({
    # all_dat()$out
    basal_areas$res
  })
  
  width_n = 350
  height_n = 350
  
  output$fixedPlot = renderPlot({
    ggplot(data = all_dat()$fixed_data) +
      geom_point(aes(x, y, size = dbh_cm, color = dbh_cm), alpha = 0.8) +
      # scale_color_viridis_c(direction = -1, option = "A") +
      geom_point(data = NULL, aes(x = all_dat()$xp, y = all_dat()$yp), color = "red", shape = "+") +
      geom_path(data = circle_coords(all_dat()$xp, all_dat()$yp, all_dat()$r, 500), 
                aes(x = xv, y = yv), color = "red") +
      coord_fixed() +
      labs(title = "Trees included in fixed-radius plot") +
      theme(legend.position = "none")
  }, width = width_n, height = height_n)
  
  output$variablePlot = renderPlot({
    ggplot(data = all_dat()$variable_data) +
      geom_point(aes(x, y, size = dbh_cm, color = dbh_cm), alpha = 0.8) +
      geom_circle(aes(x0 = x, y0 = y, r = r_circle, color = dbh_cm, linetype = in_circle)) +
      # scale_color_viridis_c(direction = -1, option = "A") +
      geom_point(data = NULL, aes(x = all_dat()$xp, y = all_dat()$yp), color = "red", shape = "+") +
      # trees in fixed-radius plot but are not in variable-radius plot
      geom_point(data = filter(all_dat()$fixed_data, !id %in% all_dat()$variable_data$id), 
                 aes(x = x, y = y), color = "red", shape = "x") +
      geom_path(data = circle_coords(all_dat()$xp, all_dat()$yp, all_dat()$r, 500), 
                aes(x = xv, y = yv), color = "red") +
      coord_fixed() +
      scale_linetype_manual(values = c(2, 1)) +
      labs(title = "Trees included in variable-radius plot") +
      theme(legend.position = "none")
  }, width = width_n, height = height_n)
  
  output$allPlot = renderPlot({
    d3 = all_dat()$all_data
    ggplot() +
      geom_point(data = filter(d3, !in_circle), aes(x, y), color = "gray10", alpha = 0.06) +
      geom_point(data = filter(d3, in_circle), aes(x, y, color = dbh_cm), alpha = 0.8) +
      # scale_color_viridis_c(direction = -1) +
      geom_path(data = circle_coords(all_dat()$xp, all_dat()$yp, all_dat()$r, 60), 
                aes(x = xv, y = yv), color = "red") +
      coord_fixed()
  }, width = width_n, height = height_n)
  
  output$comparePlot = renderPlot({
    dd = basal_areas$res %>% 
      rename(Fixed_radius = fixed_BA, Variable_radius = variable_BA) %>% 
      gather("Method", "Basal Area per Hectare", Fixed_radius, Variable_radius) 
    dd2 = group_by(dd, Method) %>% 
      summarise(`Basal Area per Hectare` = mean(`Basal Area per Hectare`))
    ggplot(dd, aes(x = Method, y = `Basal Area per Hectare`, color = Method)) +
      geom_point(show.legend = F) +
      geom_point(data = dd2, aes(x = Method, y = `Basal Area per Hectare`), 
                 color = "black", size = 2) +
      geom_hline(yintercept = unique(dd$true_BA), linetype = 2) +
      labs(y = "Basal Area (m2) per Hectare")
  }, width = width_n, height = height_n)
  
  output$comparePlot2 = renderPlot({
    dd = basal_areas$res %>% 
      rename(Fixed_radius = fixed_N, Variable_radius = variable_N) %>% 
      gather("Method", "N trees per Hectare", Fixed_radius, Variable_radius) 
    dd2 = group_by(dd, Method) %>% 
      summarise(`N trees per Hectare` = mean(`N trees per Hectare`))
    ggplot(dd, aes(x = Method, y = `N trees per Hectare`, color = Method)) +
      geom_point(show.legend = F) +
      geom_point(data = dd2, aes(x = Method, y = `N trees per Hectare`), 
                 color = "black", size = 2) +
      geom_hline(yintercept = unique(dd$true_N), linetype = 2) +
      labs(y = "N trees per Hectare")
  }, width = width_n, height = height_n)
}

