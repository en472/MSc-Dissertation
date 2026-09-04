# Determining Differences in Cell Cycle - this is just plotting with no statistics

# libraries


# read in data


# assign cell cycle G1 or S/G2/M phase per surface type (as DAPI
# staining differs signfiicantly) ======================


# medium flat surface ---------------------------

cell_cycle_medium <- df %>%
  filter(surface == 'flat_medium_density') %>%
  mutate(cell_cycle = case_when(
    Intensity_MeanIntensity_CorrectedCycA_Nuclei_normalised <= 2 & Intensity_IntegratedIntensity_CorrectedDNA_Nuclei_normalised <= 1.5  ~ 'G1 Phase',
   .default = 'S/G2/M Phase'
  ))

# nn 2 700 --------------------------------------

cell_cycle_2_700 <- df %>%
  filter(surface == 'nN_2_700') %>%
  mutate(cell_cycle = case_when(
    Intensity_MeanIntensity_CorrectedCycA_Nuclei_normalised <= 2 & Intensity_IntegratedIntensity_CorrectedDNA_Nuclei_normalised <= 1.5  ~ 'G1 Phase',
    .default = 'S/G2/M Phase'
  ))

# nn 2 1000 --------------------------------------

cell_cycle_2_1000 <- df %>%
  filter(surface == 'nN_2_1000') %>%
  mutate(cell_cycle = case_when(
    Intensity_MeanIntensity_CorrectedCycA_Nuclei_normalised <= 2 & Intensity_IntegratedIntensity_CorrectedDNA_Nuclei_normalised <= 0.8  ~ 'G1 Phase',
    .default = 'S/G2/M Phase'
  ))

# nn 5 700 --------------------------------------

cell_cycle_5_700 <- df %>%
  filter(surface == 'nN_5_700') %>%
  mutate(cell_cycle = case_when(
    Intensity_MeanIntensity_CorrectedCycA_Nuclei_normalised <= 2 & Intensity_IntegratedIntensity_CorrectedDNA_Nuclei_normalised <= 1.75  ~ 'G1 Phase',
    .default = 'S/G2/M Phase'
  ))

# nn 5 1000 --------------------------------------

cell_cycle_5_1000 <- df %>%
  filter(surface == 'nN_5_1000') %>%
  mutate(cell_cycle = case_when(
    Intensity_MeanIntensity_CorrectedCycA_Nuclei_normalised <= 2 & Intensity_IntegratedIntensity_CorrectedDNA_Nuclei_normalised <= 1.75  ~ 'G1 Phase',
    .default = 'S/G2/M Phase'
  ))

## now join all together to re-assemble the dataset
df_cell_cycle <- rbind(cell_cycle_medium,
                       cell_cycle_2_700,
                       cell_cycle_2_1000,
                       cell_cycle_5_700,
                       cell_cycle_5_1000)

## initial summary statistics ====================================

# flat proportion of cells in S/G2/M Phase across surface types

df_cell_cycle %>%
  group_by(surface, cell_cycle) %>%
  summarise(
    n = n()
  ) %>%
  pivot_wider(names_from = 'cell_cycle', values_from = 'n') %>%
  mutate(proportion_cycling = `S/G2/M Phase` / (`G1 Phase` + `S/G2/M Phase`) * 100) %>%
  select(surface, proportion_cycling)

## Plotting =====================================================

# proportion of cells in S/G2/M Phase across surface types by image (error bar)

df_cell_cycle %>%
# now plot bar chart based on proportion of replicating cells
  group_by(surface, cell_cycle, ImageNumber_Image) %>%
  summarise(
    n = n()
  ) %>%
  pivot_wider(names_from = cell_cycle, values_from = n) %>%
  # filter out images with NA values (where there are no cells in this category - only 16 images total)
  filter(is.na(`S/G2/M Phase`) == FALSE) %>%
  filter(is.na(`G1 Phase`) == FALSE) %>%
  # calculate the proportion that is replicating
  mutate(prop_replicating = `S/G2/M Phase` / (`G1 Phase` + `S/G2/M Phase`) * 100) %>%
  # select specific columns
  select(surface, ImageNumber_Image, prop_replicating) %>%
  # average across images
  summarise(
    # drop grouping for images
    .groups = 'drop_last',
    avg_proportion_replicating = mean(prop_replicating),
    stdev_proportion_replicating = sd(prop_replicating)
  ) %>%
  # plot
  ggplot(aes(x = surface, y = avg_proportion_replicating, fill = surface)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = avg_proportion_replicating - stdev_proportion_replicating, ymax = avg_proportion_replicating + stdev_proportion_replicating), width = 0.3) +
  theme_minimal() +
  labs(
    title = 'Proportion of S/G2/M Phase Cells Across Images'
  )

# proportion of cells in S/G2/M phase across images by surface type and adjacent touching

df_cell_cycle %>%
  # add neighbor labels
  mutate(per_touch = case_when(
    Neighbors_PercentTouching_Adjacent_Cells >= 0 & Neighbors_PercentTouching_Adjacent_Cells <= 25 ~ '0-25%',
    Neighbors_PercentTouching_Adjacent_Cells > 25 & Neighbors_PercentTouching_Adjacent_Cells <= 50 ~ '25-50%',
    Neighbors_PercentTouching_Adjacent_Cells > 50 & Neighbors_PercentTouching_Adjacent_Cells <= 75 ~ '50-75%',
    Neighbors_PercentTouching_Adjacent_Cells > 75 & Neighbors_PercentTouching_Adjacent_Cells <= 100 ~ '75-100%',
    .default = NA # to check for mismatches - none
  )) %>%
  group_by(surface, cell_cycle, per_touch, ImageNumber_Image) %>%
  summarise(
    n = n()
  ) %>%
  pivot_wider(names_from = cell_cycle, values_from = n) %>%
  # filter out data with NA values (where there are no cells in this category - 350 groups total)
  filter(is.na(`S/G2/M Phase`) == FALSE) %>%
  filter(is.na(`G1 Phase`) == FALSE) %>%
  # calculate the proportion that is replicating
  mutate(prop_replicating = `S/G2/M Phase` / (`G1 Phase` + `S/G2/M Phase`) * 100) %>%
  # select specific columns
  select(surface, ImageNumber_Image, per_touch, prop_replicating) %>%
  # average across images
  summarise(
    # drop grouping for images
    .groups = 'drop_last',
    avg_proportion_replicating = mean(prop_replicating),
    stdev_proportion_replicating = sd(prop_replicating)
  ) %>%
  # plot
  ggplot(aes(x = surface, y = avg_proportion_replicating, fill = per_touch)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_errorbar(aes(ymin = avg_proportion_replicating - stdev_proportion_replicating, ymax = avg_proportion_replicating + stdev_proportion_replicating), width = 0.3, position = position_dodge(0.9)) +
  theme_minimal() +
  labs(
    title = 'Proportion of S/G2/M Phase Cells Across Images By Cell Density'
  )

# Proportion of cells in S/G2/M Phase across images by surface type and number of neighbors

df_cell_cycle %>%
  select(ImageNumber_Image, surface, cell_cycle, Neighbors_NumberOfNeighbors_Adjacent_Cells) %>%
  # update neighbor categories
  mutate(neighbor_category = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells >=8 ~ '8+',
    .default = as.character(Neighbors_NumberOfNeighbors_Adjacent_Cells)
  )) %>%
  # drop old nighbor col
  select(!Neighbors_NumberOfNeighbors_Adjacent_Cells) %>%
  group_by(surface, cell_cycle, neighbor_category, ImageNumber_Image) %>%
  summarise(
    n = n()
  ) %>%
  pivot_wider(names_from = cell_cycle, values_from = n) %>%
  # filter out data with NA values (where there are no cells in this category - 2295 groups total)
  filter(is.na(`S/G2/M Phase`) == FALSE) %>%
  filter(is.na(`G1 Phase`) == FALSE) %>%
  # calculate the proportion that is replicating per group
  mutate(prop_replicating = `S/G2/M Phase` / (`G1 Phase` + `S/G2/M Phase`) * 100) %>%
  # select specific columns
  select(surface, ImageNumber_Image, neighbor_category, prop_replicating) %>%
  # average across images
  summarise(
    # drop grouping for images
    .groups = 'drop_last',
    # calculate
    avg_proportion_replicating = mean(prop_replicating),
    stdev_proportion_replicating = sd(prop_replicating)
  ) %>%
  # plot
  ggplot(aes(x = neighbor_category, y = avg_proportion_replicating, colour =  surface, group = surface)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = avg_proportion_replicating - stdev_proportion_replicating, ymax = avg_proportion_replicating + stdev_proportion_replicating), width = 0.3) +
  theme_minimal() +
  facet_wrap(~surface)
  
