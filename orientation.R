
# Comparing the Orientation of Cells and Nuclei across surface conditions

# libraries

# read in dataset


# ==========================================================================

# angle adjustments are as follows:

# 2um needle spacing and 700um needle width: 2.5

# 2um needle spacing and 1000um needle width: 0.25

# 5um needle spacing and 700um needle width: -4.0

# 5um needle spacing and 1000um needle width: 1.0

# =========================================================================

# adjust angles

df <- df %>%
  mutate(angle_of_offset = case_when(
    (grepl('nN 2-700', PathName_Actin_Image) == TRUE) ~ 2.5,
    (grepl('nN 2-1000', PathName_Actin_Image) == TRUE) ~ 0.25,
    (grepl('nN 5-700', PathName_Actin_Image) == TRUE) ~ -4,
    (grepl('nN 5-1000', PathName_Actin_Image) == TRUE) ~ 1,
    # for flat surfaces:
    .default = 0
  )) %>%
  # offset the orientation to correct for different plate angles when imaging
  mutate(offset_orientation_cells = AreaShape_Orientation_Cells - angle_of_offset) %>%
  # nuclei too
  mutate(offset_orientation_nuclei = AreaShape_Orientation_Nuclei - angle_of_offset)



# ============================================================================

# ----------------------- basic oriented proportions ------------------------

# the proportion of cells within 10 degrees of true left/right or up/down

df %>%
  # per image
  group_by(ImageNumber_Image) %>%
  # label cells as oriented or not
  mutate(alignment = case_when(
    offset_orientation_cells > -5 & offset_orientation_cells < 5 ~ 'oriented',
    offset_orientation_cells <= -85 & offset_orientation_cells > -90  ~ 'oriented',
    offset_orientation_cells >= 85 & offset_orientation_cells < 90 ~ 'oriented',
    .default = 'non-oriented'
  )) %>%
  # select relevant cols
  select(ImageNumber_Image, ObjectNumber_Nuclei, alignment, surface) %>%
  group_by(ImageNumber_Image, surface) %>%
  # calculate proportion of cells in each image aligned / number not aligned
  mutate(prop_alignment = length(which(alignment == 'oriented')) / length(which(alignment == 'non-oriented')) * 100) %>%
  # replace inf values where all cells are oriented (unlikely but possible)
  mutate(prop_alignment = replace(prop_alignment, is.infinite(prop_alignment), 1)) %>%
  # select specific cols
  select(ImageNumber_Image, prop_alignment, surface) %>%
  distinct() %>%
  # filter out high and low density samples
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # set the surface type to factor to set the order for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  # rename
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  group_by(surface) %>%
  # calculate
  summarise(
    n = n(),
    avg_alignment = mean(prop_alignment),
    stdev = sd(prop_alignment),
    stderr = stdev / sqrt(n),
    ci_lower = avg_alignment - 1.96 * stderr,
    ci_upper = avg_alignment + 1.96 * stderr
  ) %>%
  ggplot(aes(x = surface, y = avg_alignment, fill = surface)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  #ylim(0, 20) +
  theme_minimal() +
  labs(
    title = 'Percentage of Orientated Cells (10 Degrees)'
  ) +
  scale_fill_manual(values = c('grey', '#f8961e', '#f3722c', '#90be6d', '#43aa8b')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# the proportion of nuclei within 10 degrees of true left/right or up/down

df %>%
  # per image
  group_by(ImageNumber_Image) %>%
  # label cells as oriented or not
  mutate(alignment = case_when(
    offset_orientation_nuclei > -5 & offset_orientation_nuclei < 5 ~ 'oriented',
    offset_orientation_nuclei <= -85 & offset_orientation_nuclei > -90  ~ 'oriented',
    offset_orientation_nuclei >= 85 & offset_orientation_nuclei < 90 ~ 'oriented',
    .default = 'non-oriented'
  )) %>%
  # select relevant cols
  select(ImageNumber_Image, ObjectNumber_Nuclei, alignment, surface) %>%
  group_by(ImageNumber_Image, surface) %>%
  # calculate proportion of cells in each image aligned / number not aligned
  mutate(prop_alignment = length(which(alignment == 'oriented')) / length(which(alignment == 'non-oriented')) * 100) %>%
  # replace inf values where all cells are oriented (unlikely but possible)
  mutate(prop_alignment = replace(prop_alignment, is.infinite(prop_alignment), 1)) %>%
  # select specific cols
  select(ImageNumber_Image, prop_alignment, surface) %>%
  distinct() %>%
  # filter out high and low density samples
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # set the surface type to factor to set the order for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  # rename
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  group_by(surface) %>%
  # calculate
  summarise(
    n = n(),
    avg_alignment = mean(prop_alignment),
    stdev = sd(prop_alignment),
    stderr = stdev / sqrt(n),
    ci_lower = avg_alignment - 1.96 * stderr,
    ci_upper = avg_alignment + 1.96 * stderr
  ) %>%
  ggplot(aes(x = surface, y = avg_alignment, fill = surface)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  #ylim(0, 20) +
  theme_minimal() +
  labs(
    title = 'Percentage of Orientated Nuclei (10 Degrees)'
  ) +
  scale_fill_manual(values = c('grey', '#f8961e', '#f3722c', '#90be6d', '#43aa8b')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# --------------- orientated proportion split by cell density ----------------

# cells

df %>%
  # assign neighbors
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # per image
  group_by(ImageNumber_Image) %>%
  # label cells as oriented or not
  mutate(alignment = case_when(
    offset_orientation_cells > -5 & offset_orientation_cells < 5 ~ 'oriented',
    offset_orientation_cells <= -85 & offset_orientation_cells > -90  ~ 'oriented',
    offset_orientation_cells >= 85 & offset_orientation_cells < 90 ~ 'oriented',
    .default = 'non-oriented'
  )) %>%
  # select relevant cols
  select(ImageNumber_Image, ObjectNumber_Nuclei, alignment, surface, neighbors) %>%
  group_by(ImageNumber_Image, surface, neighbors) %>%
  # calculate proportion of cells in each image aligned / number not aligned
  mutate(prop_alignment = length(which(alignment == 'oriented')) / length(which(alignment == 'non-oriented')) * 100) %>%
  # replace inf values where all cells are oriented (unlikely but possible)
  mutate(prop_alignment = replace(prop_alignment, is.infinite(prop_alignment), 1)) %>%
  # select specific cols
  select(ImageNumber_Image, prop_alignment, surface, neighbors) %>%
  distinct() %>%
  # filter out high and low density samples
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # set the surface type to factor to set the order for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  # rename
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  group_by(surface, neighbors) %>%
  # calculate
  summarise(
    n = n(),
    avg_alignment = mean(prop_alignment),
    stdev = sd(prop_alignment),
    stderr = stdev / sqrt(n),
    ci_lower = avg_alignment - 1.96 * stderr,
    ci_upper = avg_alignment + 1.96 * stderr
  ) %>%
  ggplot(aes(x = neighbors, y = avg_alignment, fill = surface)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(0.9)) +
  #ylim(0, 20) +
  theme_minimal() +
  labs(
    title = 'Percentage of Orientated Cells (10 Degrees)'
  ) +
  scale_fill_manual(values = c('grey', '#f8961e', '#f3722c', '#90be6d', '#43aa8b')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# nuclei

df %>%
  # assign neighbors
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # per image
  group_by(ImageNumber_Image) %>%
  # label cells as oriented or not
  mutate(alignment = case_when(
    offset_orientation_nuclei > -5 & offset_orientation_nuclei < 5 ~ 'oriented',
    offset_orientation_nuclei <= -85 & offset_orientation_nuclei > -90  ~ 'oriented',
    offset_orientation_nuclei >= 85 & offset_orientation_nuclei < 90 ~ 'oriented',
    .default = 'non-oriented'
  )) %>%
  # select relevant cols
  select(ImageNumber_Image, ObjectNumber_Nuclei, alignment, surface, neighbors) %>%
  group_by(ImageNumber_Image, surface, neighbors) %>%
  # calculate proportion of cells in each image aligned / number not aligned
  mutate(prop_alignment = length(which(alignment == 'oriented')) / length(which(alignment == 'non-oriented')) * 100) %>%
  # replace inf values where all cells are oriented (unlikely but possible)
  mutate(prop_alignment = replace(prop_alignment, is.infinite(prop_alignment), 1)) %>%
  # select specific cols
  select(ImageNumber_Image, prop_alignment, surface, neighbors) %>%
  distinct() %>%
  # filter out high and low density samples
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # set the surface type to factor to set the order for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  # rename
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  group_by(surface, neighbors) %>%
  # calculate
  summarise(
    n = n(),
    avg_alignment = mean(prop_alignment),
    stdev = sd(prop_alignment),
    stderr = stdev / sqrt(n),
    ci_lower = avg_alignment - 1.96 * stderr,
    ci_upper = avg_alignment + 1.96 * stderr
  ) %>%
  ggplot(aes(x = neighbors, y = avg_alignment, fill = surface)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(0.9)) +
  #ylim(0, 20) +
  theme_minimal() +
  labs(
    title = 'Percentage of Orientated Nuclei (10 Degrees)'
  ) +
  scale_fill_manual(values = c('grey', '#f8961e', '#f3722c', '#90be6d', '#43aa8b')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------------------- Statistics --------------------------------------

# cell orientation by surface

orientation_by_surface_cells <- df %>%
  # per image
  group_by(ImageNumber_Image) %>%
  # label cells as oriented or not
  mutate(alignment = case_when(
    offset_orientation_cells > -5 & offset_orientation_cells < 5 ~ 'oriented',
    offset_orientation_cells <= -85 & offset_orientation_cells > -90  ~ 'oriented',
    offset_orientation_cells >= 85 & offset_orientation_cells < 90 ~ 'oriented',
    .default = 'non-oriented'
  )) %>%
  # select relevant cols
  select(ImageNumber_Image, ObjectNumber_Nuclei, alignment, surface) %>%
  group_by(ImageNumber_Image, surface) %>%
  # calculate proportion of cells in each image aligned / number not aligned
  mutate(prop_alignment = length(which(alignment == 'oriented')) / length(which(alignment == 'non-oriented')) * 100) %>%
  # replace inf values where all cells are oriented (unlikely but possible)
  mutate(prop_alignment = replace(prop_alignment, is.infinite(prop_alignment), 1)) %>%
  # select specific cols
  select(ImageNumber_Image, prop_alignment, surface) %>%
  distinct() %>%
  # filter out high and low density samples
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  select(prop_alignment, surface)

# anova and post hoc
orientation_by_surface_cells_aov <- aov(prop_alignment ~ surface, data = orientation_by_surface_cells)
summary(orientation_by_surface_cells_aov) # does show high level of significance

# tukey post hoc
TukeyHSD(orientation_by_surface_cells_aov) 


# cell orientation by surface and non ------------------------------

orientation_by_surface_cells_0 <- df %>%
  # assign neighbors
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # per image
  group_by(ImageNumber_Image) %>%
  # label cells as oriented or not
  mutate(alignment = case_when(
    offset_orientation_cells > -5 & offset_orientation_cells < 5 ~ 'oriented',
    offset_orientation_cells <= -85 & offset_orientation_cells > -90  ~ 'oriented',
    offset_orientation_cells >= 85 & offset_orientation_cells < 90 ~ 'oriented',
    .default = 'non-oriented'
  )) %>%
  # select relevant cols
  select(ImageNumber_Image, ObjectNumber_Nuclei, alignment, surface, neighbors) %>%
  group_by(ImageNumber_Image, surface, neighbors) %>%
  # calculate proportion of cells in each image aligned / number not aligned
  mutate(prop_alignment = length(which(alignment == 'oriented')) / length(which(alignment == 'non-oriented')) * 100) %>%
  # replace inf values where all cells are oriented (unlikely but possible)
  mutate(prop_alignment = replace(prop_alignment, is.infinite(prop_alignment), 1)) %>%
  # select specific cols
  select(ImageNumber_Image, prop_alignment, surface, neighbors) %>%
  distinct() %>%
  # filter out high and low density samples
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # filter for neighbor category
  filter(neighbors == '0') %>%
  select(prop_alignment, surface)

# anova and post hoc
orientation_by_surface_cells_aov_0 <- aov(prop_alignment ~ surface, data = orientation_by_surface_cells_0)
summary(orientation_by_surface_cells_aov_0) # does show high level of significance

# tukey post hoc
TukeyHSD(orientation_by_surface_cells_aov_0) 

orientation_by_surface_cells_13 <- df %>%
  # assign neighbors
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # per image
  group_by(ImageNumber_Image) %>%
  # label cells as oriented or not
  mutate(alignment = case_when(
    offset_orientation_cells > -5 & offset_orientation_cells < 5 ~ 'oriented',
    offset_orientation_cells <= -85 & offset_orientation_cells > -90  ~ 'oriented',
    offset_orientation_cells >= 85 & offset_orientation_cells < 90 ~ 'oriented',
    .default = 'non-oriented'
  )) %>%
  # select relevant cols
  select(ImageNumber_Image, ObjectNumber_Nuclei, alignment, surface, neighbors) %>%
  group_by(ImageNumber_Image, surface, neighbors) %>%
  # calculate proportion of cells in each image aligned / number not aligned
  mutate(prop_alignment = length(which(alignment == 'oriented')) / length(which(alignment == 'non-oriented')) * 100) %>%
  # replace inf values where all cells are oriented (unlikely but possible)
  mutate(prop_alignment = replace(prop_alignment, is.infinite(prop_alignment), 1)) %>%
  # select specific cols
  select(ImageNumber_Image, prop_alignment, surface, neighbors) %>%
  distinct() %>%
  # filter out high and low density samples
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # filter for neighbor category
  filter(neighbors == '1-3') %>%
  select(prop_alignment, surface)

# anova and post hoc
orientation_by_surface_cells_aov_13 <- aov(prop_alignment ~ surface, data = orientation_by_surface_cells_13)
summary(orientation_by_surface_cells_aov_13) # does show high level of significance

# tukey post hoc
TukeyHSD(orientation_by_surface_cells_aov_13) 


orientation_by_surface_cells_46 <- df %>%
  # assign neighbors
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # per image
  group_by(ImageNumber_Image) %>%
  # label cells as oriented or not
  mutate(alignment = case_when(
    offset_orientation_cells > -5 & offset_orientation_cells < 5 ~ 'oriented',
    offset_orientation_cells <= -85 & offset_orientation_cells > -90  ~ 'oriented',
    offset_orientation_cells >= 85 & offset_orientation_cells < 90 ~ 'oriented',
    .default = 'non-oriented'
  )) %>%
  # select relevant cols
  select(ImageNumber_Image, ObjectNumber_Nuclei, alignment, surface, neighbors) %>%
  group_by(ImageNumber_Image, surface, neighbors) %>%
  # calculate proportion of cells in each image aligned / number not aligned
  mutate(prop_alignment = length(which(alignment == 'oriented')) / length(which(alignment == 'non-oriented')) * 100) %>%
  # replace inf values where all cells are oriented (unlikely but possible)
  mutate(prop_alignment = replace(prop_alignment, is.infinite(prop_alignment), 1)) %>%
  # select specific cols
  select(ImageNumber_Image, prop_alignment, surface, neighbors) %>%
  distinct() %>%
  # filter out high and low density samples
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # filter for neighbor category
  filter(neighbors == '4-6') %>%
  select(prop_alignment, surface)

# anova and post hoc
orientation_by_surface_cells_aov_46 <- aov(prop_alignment ~ surface, data = orientation_by_surface_cells_46)
summary(orientation_by_surface_cells_aov_46) # does show high level of significance

# tukey post hoc
TukeyHSD(orientation_by_surface_cells_aov_46) 


orientation_by_surface_cells_7 <- df %>%
  # assign neighbors
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # per image
  group_by(ImageNumber_Image) %>%
  # label cells as oriented or not
  mutate(alignment = case_when(
    offset_orientation_cells > -5 & offset_orientation_cells < 5 ~ 'oriented',
    offset_orientation_cells <= -85 & offset_orientation_cells > -90  ~ 'oriented',
    offset_orientation_cells >= 85 & offset_orientation_cells < 90 ~ 'oriented',
    .default = 'non-oriented'
  )) %>%
  # select relevant cols
  select(ImageNumber_Image, ObjectNumber_Nuclei, alignment, surface, neighbors) %>%
  group_by(ImageNumber_Image, surface, neighbors) %>%
  # calculate proportion of cells in each image aligned / number not aligned
  mutate(prop_alignment = length(which(alignment == 'oriented')) / length(which(alignment == 'non-oriented')) * 100) %>%
  # replace inf values where all cells are oriented (unlikely but possible)
  mutate(prop_alignment = replace(prop_alignment, is.infinite(prop_alignment), 1)) %>%
  # select specific cols
  select(ImageNumber_Image, prop_alignment, surface, neighbors) %>%
  distinct() %>%
  # filter out high and low density samples
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # filter for neighbor category
  filter(neighbors == '7+') %>%
  select(prop_alignment, surface)

# anova and post hoc
orientation_by_surface_cells_aov_7 <- aov(prop_alignment ~ surface, data = orientation_by_surface_cells_7)
summary(orientation_by_surface_cells_aov_7) # does show high level of significance

# tukey post hoc
TukeyHSD(orientation_by_surface_cells_aov_7) 

# ------------------------------ nuclear ------------------

# nuclear orientation by surface

orientation_by_surface_nuc <- df %>%
  # per image
  group_by(ImageNumber_Image) %>%
  # label cells as oriented or not
  mutate(alignment = case_when(
    offset_orientation_nuclei > -5 & offset_orientation_nuclei < 5 ~ 'oriented',
    offset_orientation_nuclei <= -85 & offset_orientation_nuclei > -90  ~ 'oriented',
    offset_orientation_nuclei >= 85 & offset_orientation_nuclei < 90 ~ 'oriented',
    .default = 'non-oriented'
  )) %>%
  # select relevant cols
  select(ImageNumber_Image, ObjectNumber_Nuclei, alignment, surface) %>%
  group_by(ImageNumber_Image, surface) %>%
  # calculate proportion of cells in each image aligned / number not aligned
  mutate(prop_alignment = length(which(alignment == 'oriented')) / length(which(alignment == 'non-oriented')) * 100) %>%
  # replace inf values where all cells are oriented (unlikely but possible)
  mutate(prop_alignment = replace(prop_alignment, is.infinite(prop_alignment), 1)) %>%
  # select specific cols
  select(ImageNumber_Image, prop_alignment, surface) %>%
  distinct() %>%
  # filter out high and low density samples
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  select(prop_alignment, surface)

# anova and post hoc
orientation_by_surface_nuc_aov <- aov(prop_alignment ~ surface, data = orientation_by_surface_nuc)
summary(orientation_by_surface_nuc_aov) # does show high level of significance

# tukey post hoc
TukeyHSD(orientation_by_surface_nuc_aov) 

# ------------- nuclear prop. orientated by non --------------

orientation_by_surface_nuc_0 <- df %>%
  # assign neighbors
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # per image
  group_by(ImageNumber_Image) %>%
  # label cells as oriented or not
  mutate(alignment = case_when(
    offset_orientation_nuclei > -5 & offset_orientation_nuclei < 5 ~ 'oriented',
    offset_orientation_nuclei <= -85 & offset_orientation_nuclei > -90  ~ 'oriented',
    offset_orientation_nuclei >= 85 & offset_orientation_nuclei < 90 ~ 'oriented',
    .default = 'non-oriented'
  )) %>%
  # select relevant cols
  select(ImageNumber_Image, ObjectNumber_Nuclei, alignment, surface, neighbors) %>%
  group_by(ImageNumber_Image, surface, neighbors) %>%
  # calculate proportion of cells in each image aligned / number not aligned
  mutate(prop_alignment = length(which(alignment == 'oriented')) / length(which(alignment == 'non-oriented')) * 100) %>%
  # replace inf values where all cells are oriented (unlikely but possible)
  mutate(prop_alignment = replace(prop_alignment, is.infinite(prop_alignment), 1)) %>%
  # select specific cols
  select(ImageNumber_Image, prop_alignment, surface, neighbors) %>%
  distinct() %>%
  # filter out high and low density samples
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # filter for neighbor category
  filter(neighbors == '0') %>%
  select(prop_alignment, surface)

# anova and post hoc
orientation_by_surface_nuc_aov_0 <- aov(prop_alignment ~ surface, data = orientation_by_surface_nuc_0)
summary(orientation_by_surface_nuc_aov_0) # does show high level of significance

# tukey post hoc
TukeyHSD(orientation_by_surface_nuc_aov_0)


orientation_by_surface_nuc_13 <- df %>%
  # assign neighbors
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # per image
  group_by(ImageNumber_Image) %>%
  # label cells as oriented or not
  mutate(alignment = case_when(
    offset_orientation_nuclei > -5 & offset_orientation_nuclei < 5 ~ 'oriented',
    offset_orientation_nuclei <= -85 & offset_orientation_nuclei > -90  ~ 'oriented',
    offset_orientation_nuclei >= 85 & offset_orientation_nuclei < 90 ~ 'oriented',
    .default = 'non-oriented'
  )) %>%
  # select relevant cols
  select(ImageNumber_Image, ObjectNumber_Nuclei, alignment, surface, neighbors) %>%
  group_by(ImageNumber_Image, surface, neighbors) %>%
  # calculate proportion of cells in each image aligned / number not aligned
  mutate(prop_alignment = length(which(alignment == 'oriented')) / length(which(alignment == 'non-oriented')) * 100) %>%
  # replace inf values where all cells are oriented (unlikely but possible)
  mutate(prop_alignment = replace(prop_alignment, is.infinite(prop_alignment), 1)) %>%
  # select specific cols
  select(ImageNumber_Image, prop_alignment, surface, neighbors) %>%
  distinct() %>%
  # filter out high and low density samples
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # filter for neighbor category
  filter(neighbors == '1-3') %>%
  select(prop_alignment, surface)

# anova and post hoc
orientation_by_surface_nuc_aov_13 <- aov(prop_alignment ~ surface, data = orientation_by_surface_nuc_13)
summary(orientation_by_surface_nuc_aov_13) # does show high level of significance

# tukey post hoc
TukeyHSD(orientation_by_surface_nuc_aov_13)

orientation_by_surface_nuc_46 <- df %>%
  # assign neighbors
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # per image
  group_by(ImageNumber_Image) %>%
  # label cells as oriented or not
  mutate(alignment = case_when(
    offset_orientation_nuclei > -5 & offset_orientation_nuclei < 5 ~ 'oriented',
    offset_orientation_nuclei <= -85 & offset_orientation_nuclei > -90  ~ 'oriented',
    offset_orientation_nuclei >= 85 & offset_orientation_nuclei < 90 ~ 'oriented',
    .default = 'non-oriented'
  )) %>%
  # select relevant cols
  select(ImageNumber_Image, ObjectNumber_Nuclei, alignment, surface, neighbors) %>%
  group_by(ImageNumber_Image, surface, neighbors) %>%
  # calculate proportion of cells in each image aligned / number not aligned
  mutate(prop_alignment = length(which(alignment == 'oriented')) / length(which(alignment == 'non-oriented')) * 100) %>%
  # replace inf values where all cells are oriented (unlikely but possible)
  mutate(prop_alignment = replace(prop_alignment, is.infinite(prop_alignment), 1)) %>%
  # select specific cols
  select(ImageNumber_Image, prop_alignment, surface, neighbors) %>%
  distinct() %>%
  # filter out high and low density samples
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # filter for neighbor category
  filter(neighbors == '4-6') %>%
  select(prop_alignment, surface)

# anova and post hoc
orientation_by_surface_nuc_aov_46 <- aov(prop_alignment ~ surface, data = orientation_by_surface_nuc_46)
summary(orientation_by_surface_nuc_aov_46) # does show high level of significance

# tukey post hoc
TukeyHSD(orientation_by_surface_nuc_aov_46)

orientation_by_surface_nuc_7 <- df %>%
  # assign neighbors
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # per image
  group_by(ImageNumber_Image) %>%
  # label cells as oriented or not
  mutate(alignment = case_when(
    offset_orientation_nuclei > -5 & offset_orientation_nuclei < 5 ~ 'oriented',
    offset_orientation_nuclei <= -85 & offset_orientation_nuclei > -90  ~ 'oriented',
    offset_orientation_nuclei >= 85 & offset_orientation_nuclei < 90 ~ 'oriented',
    .default = 'non-oriented'
  )) %>%
  # select relevant cols
  select(ImageNumber_Image, ObjectNumber_Nuclei, alignment, surface, neighbors) %>%
  group_by(ImageNumber_Image, surface, neighbors) %>%
  # calculate proportion of cells in each image aligned / number not aligned
  mutate(prop_alignment = length(which(alignment == 'oriented')) / length(which(alignment == 'non-oriented')) * 100) %>%
  # replace inf values where all cells are oriented (unlikely but possible)
  mutate(prop_alignment = replace(prop_alignment, is.infinite(prop_alignment), 1)) %>%
  # select specific cols
  select(ImageNumber_Image, prop_alignment, surface, neighbors) %>%
  distinct() %>%
  # filter out high and low density samples
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # filter for neighbor category
  filter(neighbors == '7+') %>%
  select(prop_alignment, surface)

# anova and post hoc
orientation_by_surface_nuc_aov_7 <- aov(prop_alignment ~ surface, data = orientation_by_surface_nuc_7)
summary(orientation_by_surface_nuc_aov_7) # does show high level of significance

# tukey post hoc
TukeyHSD(orientation_by_surface_nuc_aov_7)

# ====================================================================================

# ------------------------- density plots condition vs control -----------------------

# 2 700

df %>%
  group_by(surface) %>%
  # change factors for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, c('flat_high_density', 'flat_medium_density', 'flat_low_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000'))) %>%
  filter(surface == c('flat_medium_density', 'nN_2_700')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700')) %>%
  # change angle to absolute value
  mutate(offset_orientation_cells = abs(offset_orientation_cells)) %>%
  # filter out values with tick over 90 or under 0 from angle adjustment
  filter(offset_orientation_cells < 90 & offset_orientation_cells > 0) %>%
  # plot
  ggplot(aes(x = offset_orientation_cells, colour = surface)) +
  geom_density(linewidth = 1) +
  scale_colour_manual(values = c('darkgrey', '#f8961e')) +
  theme_minimal() +
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
