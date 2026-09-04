## Cellular Orientation Density Plots

# this codes for both the -90 to 90 degree graphs and the folded 0 to 90 degree
# graphs (which is the data passed forward for spm). Change the code where marked
# to get the two different graphs from each block of code.

# condition v control density plots ===================

# cells ------------------------------------------------

# 2 700

df %>%
  group_by(surface) %>%
  # change factors for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, c('flat_high_density', 'flat_medium_density', 'flat_low_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000'))) %>%
  filter(surface == c('flat_medium_density', 'nN_2_700')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700')) %>%
  # change angle to absolute value - REMOVE OR ADD FOR FOLDED/REGULAR PLOT
  mutate(offset_orientation_cells = abs(offset_orientation_cells)) %>%
  # filter out values with tick over 90 or under 0 from angle adjustment - REMOVE OR ADD FOR FOLDED/REGULAR PLOT
  filter(offset_orientation_cells < 90 & offset_orientation_cells > 0) %>%
  # plot
  ggplot(aes(x = offset_orientation_cells, colour = surface)) +
  geom_density(linewidth = 1) +
  scale_colour_manual(values = c('darkgrey', '#f8961e')) +
  theme_minimal() +
  # SWITCH SCALES DEPENDING ON PLOT
  scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90)) +
  #scale_x_continuous(breaks = c(-90, -45, 0, 45, 90)) + 
  labs(
    title = 'Density Plot of Cell Orientation'
  )

# 2 1000

df %>%
  group_by(surface) %>%
  # change factors for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, c('flat_high_density', 'flat_medium_density', 'flat_low_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000'))) %>%
  filter(surface == c('flat_medium_density', 'nN_2_1000')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 1000μm Width' = 'nN_2_1000')) %>%
  # change angle to absolute value
  mutate(offset_orientation_cells = abs(offset_orientation_cells)) %>%
  # filter out values with tick over 90 or under 0 from angle adjustment
  filter(offset_orientation_cells < 90 & offset_orientation_cells > 0) %>%
  # plot
  ggplot(aes(x = offset_orientation_cells, colour = surface)) +
  geom_density(linewidth = 1) +
  scale_colour_manual(values = c('darkgrey', '#f3722c')) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90)) +
  #scale_x_continuous(breaks = c(-90, -45, 0, 45, 90)) +
  labs(
    title = 'Density Plot of Cell Orientation'
  )

# 5 700

df %>%
  group_by(surface) %>%
  # change factors for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, c('flat_high_density', 'flat_medium_density', 'flat_low_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000'))) %>%
  filter(surface == c('flat_medium_density', 'nN_5_700')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '5μm Dist. 700μm Width' = 'nN_5_700')) %>%
  # change angle to absolute value
  mutate(offset_orientation_cells = abs(offset_orientation_cells)) %>%
  # filter out values with tick over 90 or under 0 from angle adjustment
  filter(offset_orientation_cells < 90 & offset_orientation_cells > 0) %>%
  # plot
  ggplot(aes(x = offset_orientation_cells, colour = surface)) +
  geom_density(linewidth = 1) +
  scale_colour_manual(values = c('darkgrey', '#90be6d')) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90)) +
  #scale_x_continuous(breaks = c(-90, -45, 0, 45, 90)) +
  labs(
    title = 'Density Plot of Cell Orientation'
  )

# 5 1000

df %>%
  group_by(surface) %>%
  # change factors for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, c('flat_high_density', 'flat_medium_density', 'flat_low_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000'))) %>%
  filter(surface == c('flat_medium_density', 'nN_5_1000')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '5μm Dist. 1000μm Width' = 'nN_5_1000')) %>%
  # change angle to absolute value
  mutate(offset_orientation_cells = abs(offset_orientation_cells)) %>%
  # filter out values with tick over 90 or under 0 from angle adjustment
  filter(offset_orientation_cells < 90 & offset_orientation_cells > 0) %>%
  # plot
  ggplot(aes(x = offset_orientation_cells, colour = surface)) +
  geom_density(linewidth = 1) +
  scale_colour_manual(values = c('darkgrey', '#43aa8b')) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90)) +
  #scale_x_continuous(breaks = c(-90, -45, 0, 45, 90)) +
  labs(
    title = 'Density Plot of Cell Orientation'
  )


# nuclei ------------------------------------------------

# 2 700

df %>%
  group_by(surface) %>%
  # change factors for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, c('flat_high_density', 'flat_medium_density', 'flat_low_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000'))) %>%
  filter(surface == c('flat_medium_density', 'nN_2_700')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700')) %>%
  # change angle to absolute value - REMOVE OR ADD FOR FOLDED/REGULAR PLOT
  #mutate(offset_orientation_nuclei = abs(offset_orientation_nuclei)) %>%
  # filter out values with tick over 90 or under 0 from angle adjustment - REMOVE OR ADD FOR FOLDED/REGULAR PLOT
  #filter(offset_orientation_nuclei < 90 & offset_orientation_nuclei > 0) %>%
  # plot
  ggplot(aes(x = offset_orientation_nuclei, colour = surface)) +
  geom_density(linewidth = 1) +
  scale_colour_manual(values = c('darkgrey', '#f8961e')) +
  theme_minimal() +
  # SWITCH SCALES DEPENDING ON PLOT
  #scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90)) +
  scale_x_continuous(breaks = c(-90, -45, 0, 45, 90)) + 
  labs(
    title = 'Density Plot of Nucleus Orientation'
  )

# 2 1000

df %>%
  group_by(surface) %>%
  # change factors for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, c('flat_high_density', 'flat_medium_density', 'flat_low_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000'))) %>%
  filter(surface == c('flat_medium_density', 'nN_2_1000')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 1000μm Width' = 'nN_2_1000')) %>%
  # change angle to absolute value
  #mutate(offset_orientation_nuclei = abs(offset_orientation_nuclei)) %>%
  # filter out values with tick over 90 or under 0 from angle adjustment
  #filter(offset_orientation_nuclei < 90 & offset_orientation_nuclei > 0) %>%
  # plot
  ggplot(aes(x = offset_orientation_nuclei, colour = surface)) +
  geom_density(linewidth = 1) +
  scale_colour_manual(values = c('darkgrey', '#f3722c')) +
  theme_minimal() +
  #scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90)) +
  scale_x_continuous(breaks = c(-90, -45, 0, 45, 90)) +
  labs(
    title = 'Density Plot of Nucleus Orientation'
  )

# 5 700

df %>%
  group_by(surface) %>%
  # change factors for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, c('flat_high_density', 'flat_medium_density', 'flat_low_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000'))) %>%
  filter(surface == c('flat_medium_density', 'nN_5_700')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '5μm Dist. 700μm Width' = 'nN_5_700')) %>%
  # change angle to absolute value
  #mutate(offset_orientation_nuclei = abs(offset_orientation_nuclei)) %>%
  # filter out values with tick over 90 or under 0 from angle adjustment
  #filter(offset_orientation_nuclei < 90 & offset_orientation_nuclei > 0) %>%
  # plot
  ggplot(aes(x = offset_orientation_nuclei, colour = surface)) +
  geom_density(linewidth = 1) +
  scale_colour_manual(values = c('darkgrey', '#90be6d')) +
  theme_minimal() +
  #scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90)) +
  scale_x_continuous(breaks = c(-90, -45, 0, 45, 90)) +
  labs(
    title = 'Density Plot of Nucleus Orientation'
  )

# 5 1000

df %>%
  group_by(surface) %>%
  # change factors for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, c('flat_high_density', 'flat_medium_density', 'flat_low_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000'))) %>%
  filter(surface == c('flat_medium_density', 'nN_5_1000')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '5μm Dist. 1000μm Width' = 'nN_5_1000')) %>%
  # change angle to absolute value
  #mutate(offset_orientation_nuclei = abs(offset_orientation_nuclei)) %>%
  # filter out values with tick over 90 or under 0 from angle adjustment
  #filter(offset_orientation_nuclei < 90 & offset_orientation_nuclei > 0) %>%
  # plot
  ggplot(aes(x = offset_orientation_nuclei, colour = surface)) +
  geom_density(linewidth = 1) +
  scale_colour_manual(values = c('darkgrey', '#43aa8b')) +
  theme_minimal() +
  #scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90)) +
  scale_x_continuous(breaks = c(-90, -45, 0, 45, 90)) +
  labs(
    title = 'Density Plot of Nucleus Orientation'
  )

