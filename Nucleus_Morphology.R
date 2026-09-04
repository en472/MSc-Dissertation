
# Morphology of Nuclei via Form Factor and Eccentricity ==================

# libraries

# read in data

# Eccentricity ----------------------------------------------------------

# overall -----------------

df %>%
  # select relevant columns
  select(AreaShape_Eccentricity_Nuclei, surface, material_type, ImageNumber_Image) %>%
  # filter only for wanted groups
  filter(surface %in% c('flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  # set the surface type to factor and set the order for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  # rename for plotting
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  # remove any rows with NA data
  na.omit() %>%
  # group by image number
  group_by(ImageNumber_Image) %>%
  # average eccentricity per images
  mutate(mean_eccentricity = mean(AreaShape_Eccentricity_Nuclei)) %>%
  # drop individual measure for eccentricity
  select(!AreaShape_Eccentricity_Nuclei) %>%
  # remove repeats
  distinct() %>%
  # plot
  ggplot(aes(x = surface, y = mean_eccentricity, fill = surface))  +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  # add jittered points to graph
  geom_jitter(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.75
    ),
    alpha = 0.3,
    size = 0.5
  ) +
  labs(
    title = 'Nuclear Eccentricity by Surface Type'
  ) +
  theme_minimal() +
  #ylim(0.55, 0.70) +
  #scale_y_continuous(breaks = c(0.68, 0.70, 0.72, 0.74, 0.76, 0.78)) +
  scale_fill_manual(values = c('grey', '#f8961e', '#f3722c', '#90be6d', '#43aa8b')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# stats

overall <- df %>%
  # select relevant columns
  select(AreaShape_Eccentricity_Nuclei, surface, material_type, ImageNumber_Image) %>%
  # filter only for wanted groups
  filter(surface %in% c('flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  # set the surface type to factor and set the order for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  # rename for plotting
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  # remove any rows with NA data
  na.omit() %>%
  # group by image number
  group_by(ImageNumber_Image) %>%
  # average eccentricity per images
  mutate(mean_eccentricity = mean(AreaShape_Eccentricity_Nuclei)) %>%
  # drop individual measure for eccentricity
  select(!AreaShape_Eccentricity_Nuclei) %>%
  # remove repeats
  distinct()

# generate anova model and summarise to view
overall_aov <- aov(mean_eccentricity ~ surface, data = overall)
summary(overall_aov) 

# tukey post hoc
TukeyHSD(overall_aov) # all ns

# by number of neighbors ------------

df %>%
  # select relevant columns
  select(AreaShape_Eccentricity_Nuclei, surface, Neighbors_NumberOfNeighbors_Adjacent_Cells, ImageNumber_Image) %>%
  # put number of neighbors into clear bins for plotting
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # filter for only medium density flat surface sample (to prevent overcrowding graph)
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # check for NA values - none
  #anyNA()
  # rename
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  na.omit() %>%
  #group by image number
  group_by(ImageNumber_Image, neighbors) %>%
  # average eccentricity across images
  mutate(mean_ecc = mean(AreaShape_Eccentricity_Nuclei)) %>%
  # drop indiviual measure for eccentricity
  select(!AreaShape_Eccentricity_Nuclei) %>%
  select(!Neighbors_NumberOfNeighbors_Adjacent_Cells) %>%
  # remove repeats
  distinct() %>%
  ggplot(aes(x = neighbors, y = mean_ecc, fill = surface)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  geom_jitter(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.75
    ),
    alpha = 0.2,
    size = 0.5
  ) +
  labs(
    title = 'Nuclear Eccentricity by Surface Type'
  ) +
  theme_minimal() +
  scale_fill_manual(values = c('grey', '#f8961e', '#f3722c', '#90be6d', '#43aa8b'))

# stats

# no neighbors

neighbors <- df %>%
  # select relevant columns
  select(AreaShape_Eccentricity_Nuclei, surface, Neighbors_NumberOfNeighbors_Adjacent_Cells, ImageNumber_Image) %>%
  # put number of neighbors into clear bins for plotting
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # filter for only medium density flat surface sample (to prevent overcrowding graph)
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # check for NA values - none
  #anyNA()
  # rename
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  na.omit() %>%
  #group by image number
  group_by(ImageNumber_Image, neighbors) %>%
  # average eccentricity across images
  mutate(mean_ecc = mean(AreaShape_Eccentricity_Nuclei)) %>%
  # drop indiviual measure for eccentricity
  select(!AreaShape_Eccentricity_Nuclei) %>%
  select(!Neighbors_NumberOfNeighbors_Adjacent_Cells) %>%
  # remove repeats
  distinct() %>%
  filter(neighbors == '0')

# generate anova model and summarise to view
neighbors_aov <- aov(mean_ecc ~ surface, data = neighbors)
summary(neighbors_aov) 

# tukey post hoc
TukeyHSD(neighbors_aov) # all ns


# 1-3 neighbors

neighbors <- df %>%
  # select relevant columns
  select(AreaShape_Eccentricity_Nuclei, surface, Neighbors_NumberOfNeighbors_Adjacent_Cells, ImageNumber_Image) %>%
  # put number of neighbors into clear bins for plotting
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # filter for only medium density flat surface sample (to prevent overcrowding graph)
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # check for NA values - none
  #anyNA()
  # rename
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  na.omit() %>%
  #group by image number
  group_by(ImageNumber_Image, neighbors) %>%
  # average eccentricity across images
  mutate(mean_ecc = mean(AreaShape_Eccentricity_Nuclei)) %>%
  # drop indiviual measure for eccentricity
  select(!AreaShape_Eccentricity_Nuclei) %>%
  select(!Neighbors_NumberOfNeighbors_Adjacent_Cells) %>%
  # remove repeats
  distinct() %>%
  filter(neighbors == '1-3')

# generate anova model and summarise to view
neighbors_aov <- aov(mean_ecc ~ surface, data = neighbors)
summary(neighbors_aov) 

# tukey post hoc
TukeyHSD(neighbors_aov) # 2 700 significant 0.05


# 4-6 neighbors

neighbors <- df %>%
  # select relevant columns
  select(AreaShape_Eccentricity_Nuclei, surface, Neighbors_NumberOfNeighbors_Adjacent_Cells, ImageNumber_Image) %>%
  # put number of neighbors into clear bins for plotting
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # filter for only medium density flat surface sample (to prevent overcrowding graph)
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # check for NA values - none
  #anyNA()
  # rename
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  na.omit() %>%
  #group by image number
  group_by(ImageNumber_Image, neighbors) %>%
  # average eccentricity across images
  mutate(mean_ecc = mean(AreaShape_Eccentricity_Nuclei)) %>%
  # drop indiviual measure for eccentricity
  select(!AreaShape_Eccentricity_Nuclei) %>%
  select(!Neighbors_NumberOfNeighbors_Adjacent_Cells) %>%
  # remove repeats
  distinct() %>%
  filter(neighbors == '4-6')

# generate anova model and summarise to view
neighbors_aov <- aov(mean_ecc ~ surface, data = neighbors)
summary(neighbors_aov) 

# tukey post hoc
TukeyHSD(neighbors_aov) # all ns


# 7+

neighbors <- df %>%
  # select relevant columns
  select(AreaShape_Eccentricity_Nuclei, surface, Neighbors_NumberOfNeighbors_Adjacent_Cells, ImageNumber_Image) %>%
  # put number of neighbors into clear bins for plotting
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # filter for only medium density flat surface sample (to prevent overcrowding graph)
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # check for NA values - none
  #anyNA()
  # rename
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  na.omit() %>%
  #group by image number
  group_by(ImageNumber_Image, neighbors) %>%
  # average eccentricity across images
  mutate(mean_ecc = mean(AreaShape_Eccentricity_Nuclei)) %>%
  # drop indiviual measure for eccentricity
  select(!AreaShape_Eccentricity_Nuclei) %>%
  select(!Neighbors_NumberOfNeighbors_Adjacent_Cells) %>%
  # remove repeats
  distinct() %>%
  filter(neighbors == '7+')

# generate anova model and summarise to view
neighbors_aov <- aov(mean_ecc ~ surface, data = neighbors)
summary(neighbors_aov) 

# tukey post hoc
TukeyHSD(neighbors_aov) # all ns



# ===============================================================================


# Form Factor --------------------------------------------------------------------

# overall

df %>%
  # select relevant columns
  select(AreaShape_FormFactor_Nuclei, surface, material_type, ImageNumber_Image) %>%
  # filter only for wanted groups
  filter(surface %in% c('flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  # set the surface type to factor and set the order for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  # rename for plotting
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  # remove any rows with NA data
  na.omit() %>%
  # group by image number
  group_by(ImageNumber_Image) %>%
  # average form factor per images
  mutate(mean_ff = mean(AreaShape_FormFactor_Nuclei)) %>%
  # drop individual measure for eccentricity
  select(!AreaShape_FormFactor_Nuclei) %>%
  # remove repeats
  distinct() %>%
  # plot
  ggplot(aes(x = surface, y = mean_ff, fill = surface))  +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  # add jittered points to graph
  geom_jitter(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.75
    ),
    alpha = 0.3,
    size = 0.5
  ) +
  labs(
    title = 'Nuclear Form Factor by Surface Type'
  ) +
  theme_minimal() +
  scale_fill_manual(values = c('grey', '#f8961e', '#f3722c', '#90be6d', '#43aa8b')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# stats

overall <- df %>%
  # select relevant columns
  select(AreaShape_FormFactor_Nuclei, surface, material_type, ImageNumber_Image) %>%
  # filter only for wanted groups
  filter(surface %in% c('flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  # set the surface type to factor and set the order for plotting
  mutate(surface = as.factor(surface)) %>%
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  # rename for plotting
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  # remove any rows with NA data
  na.omit() %>%
  # group by image number
  group_by(ImageNumber_Image) %>%
  # average FormFactor per images
  mutate(mean_ff = mean(AreaShape_FormFactor_Nuclei)) %>%
  # drop individual measure for eccentricity
  select(!AreaShape_FormFactor_Nuclei) %>%
  # remove repeats
  distinct()

# generate anova model and summarise to view
overall_aov <- aov(mean_ff ~ surface, data = overall)
summary(overall_aov) 

# tukey post hoc
TukeyHSD(overall_aov) # all ns


# by number of neighbors ---

# plot

df %>%
  # select relevant columns
  select(AreaShape_FormFactor_Nuclei, surface, Neighbors_NumberOfNeighbors_Adjacent_Cells, ImageNumber_Image) %>%
  # put number of neighbors into clear bins for plotting
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # filter for only medium density flat surface sample (to prevent overcrowding graph)
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # check for NA values - none
  #anyNA()
  # rename
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  na.omit() %>%
  #group by image number
  group_by(ImageNumber_Image, neighbors) %>%
  # average FormFactor across images
  mutate(mean_ff = mean(AreaShape_FormFactor_Nuclei)) %>%
  # drop indiviual measure for FormFactor
  select(!AreaShape_FormFactor_Nuclei) %>%
  select(!Neighbors_NumberOfNeighbors_Adjacent_Cells) %>%
  # remove repeats
  distinct() %>%
  ggplot(aes(x = neighbors, y = mean_ff, fill = surface)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  geom_jitter(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.75
    ),
    alpha = 0.2,
    size = 0.5
  ) +
  labs(
    title = 'Nuclear Form Factor by Surface Type'
  ) +
  theme_minimal() +
  scale_fill_manual(values = c('grey', '#f8961e', '#f3722c', '#90be6d', '#43aa8b'))


# stats

# no neighbors

neighbors <- df %>%
  # select relevant columns
  select(AreaShape_FormFactor_Nuclei, surface, Neighbors_NumberOfNeighbors_Adjacent_Cells, ImageNumber_Image) %>%
  # put number of neighbors into clear bins for plotting
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # filter for only medium density flat surface sample (to prevent overcrowding graph)
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # check for NA values - none
  #anyNA()
  # rename
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  na.omit() %>%
  #group by image number
  group_by(ImageNumber_Image, neighbors) %>%
  # average form factor across images
  mutate(mean_ff = mean(AreaShape_FormFactor_Nuclei)) %>%
  # drop indiviual measure for form factor
  select(!AreaShape_FormFactor_Nuclei) %>%
  select(!Neighbors_NumberOfNeighbors_Adjacent_Cells) %>%
  # remove repeats
  distinct() %>%
  filter(neighbors == '0')

# generate anova model and summarise to view
neighbors_aov <- aov(mean_ff ~ surface, data = neighbors)
summary(neighbors_aov) 

# tukey post hoc
TukeyHSD(neighbors_aov) # 5 700 significantly less than control


# 1-3 neighbors

neighbors <- df %>%
  # select relevant columns
  select(AreaShape_FormFactor_Nuclei, surface, Neighbors_NumberOfNeighbors_Adjacent_Cells, ImageNumber_Image) %>%
  # put number of neighbors into clear bins for plotting
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # filter for only medium density flat surface sample (to prevent overcrowding graph)
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # check for NA values - none
  #anyNA()
  # rename
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  na.omit() %>%
  #group by image number
  group_by(ImageNumber_Image, neighbors) %>%
  # average form factor across images
  mutate(mean_ff = mean(AreaShape_FormFactor_Nuclei)) %>%
  # drop indiviual measure for form factor
  select(!AreaShape_FormFactor_Nuclei) %>%
  select(!Neighbors_NumberOfNeighbors_Adjacent_Cells) %>%
  # remove repeats
  distinct() %>%
  filter(neighbors == '1-3')

# generate anova model and summarise to view
neighbors_aov <- aov(mean_ff ~ surface, data = neighbors)
summary(neighbors_aov) 

# tukey post hoc
TukeyHSD(neighbors_aov) # ns


# 4-6 neighbors

neighbors <- df %>%
  # select relevant columns
  select(AreaShape_FormFactor_Nuclei, surface, Neighbors_NumberOfNeighbors_Adjacent_Cells, ImageNumber_Image) %>%
  # put number of neighbors into clear bins for plotting
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # filter for only medium density flat surface sample (to prevent overcrowding graph)
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # check for NA values - none
  #anyNA()
  # rename
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  na.omit() %>%
  #group by image number
  group_by(ImageNumber_Image, neighbors) %>%
  # average form factor across images
  mutate(mean_ff = mean(AreaShape_FormFactor_Nuclei)) %>%
  # drop indiviual measure for form factor
  select(!AreaShape_FormFactor_Nuclei) %>%
  select(!Neighbors_NumberOfNeighbors_Adjacent_Cells) %>%
  # remove repeats
  distinct() %>%
  filter(neighbors == '4-6')

# generate anova model and summarise to view
neighbors_aov <- aov(mean_ff ~ surface, data = neighbors)
summary(neighbors_aov) 

# tukey post hoc
TukeyHSD(neighbors_aov) # ns



# 7+ neighbors

neighbors <- df %>%
  # select relevant columns
  select(AreaShape_FormFactor_Nuclei, surface, Neighbors_NumberOfNeighbors_Adjacent_Cells, ImageNumber_Image) %>%
  # put number of neighbors into clear bins for plotting
  mutate(neighbors = case_when(
    Neighbors_NumberOfNeighbors_Adjacent_Cells == 0 ~ '0',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 1 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 4 ~ '1-3',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 4 & Neighbors_NumberOfNeighbors_Adjacent_Cells < 7 ~ '4-6',
    Neighbors_NumberOfNeighbors_Adjacent_Cells >= 7 ~ '7+',
    # to see if any end up unassigned
    .default = 'NA'
  )) %>%
  # filter for only medium density flat surface sample (to prevent overcrowding graph)
  filter(surface != 'flat_high_density') %>%
  filter(surface != 'flat_low_density') %>%
  # check for NA values - none
  #anyNA()
  # rename
  mutate(surface = fct_relevel(surface, 'flat_medium_density', 'nN_2_700', 'nN_2_1000', 'nN_5_700', 'nN_5_1000')) %>%
  mutate(surface = fct_recode(surface, 'Control' = 'flat_medium_density', '2μm Dist. 700μm Width' = 'nN_2_700', '2μ Dist. 1000μ Width' = 'nN_2_1000', '5μ Dist. 700μ Width' = 'nN_5_700', '5μ Dist. 1000μ Width' = 'nN_5_1000')) %>%
  na.omit() %>%
  #group by image number
  group_by(ImageNumber_Image, neighbors) %>%
  # average form factor across images
  mutate(mean_ff = mean(AreaShape_FormFactor_Nuclei)) %>%
  # drop indiviual measure for form factor
  select(!AreaShape_FormFactor_Nuclei) %>%
  select(!Neighbors_NumberOfNeighbors_Adjacent_Cells) %>%
  # remove repeats
  distinct() %>%
  filter(neighbors == '7+')

# generate anova model and summarise to view
neighbors_aov <- aov(mean_ff ~ surface, data = neighbors)
summary(neighbors_aov) 

# tukey post hoc
TukeyHSD(neighbors_aov) # signficant results
