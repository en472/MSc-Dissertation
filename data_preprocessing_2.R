

#  data preprocessing part 2

library(tidyverse)


# whole image data is fine as is, but individual cell data needs fixing


# read in flat high density data ------------------------------------------

FHD <- read.csv('flat high density/Flat_highDensity_Nuclei.csv')

# readjust

old_colnames <- colnames(FHD)

new_colnames <- as.character(FHD[1, ])

FHD_swapped <- FHD[-1, , drop = FALSE]

colnames(FHD_swapped) <- new_colnames

FHD <- rbind(old_colnames, FHD_swapped)

rm(FHD_swapped)

# now remove numbers and points from label column
FHD[1,] <- gsub('[^A-Za-z]', '', FHD[1,])

# Extract first row
first_row <- as.character(FHD[1, ])

# Create new combined headers
new_headers <- paste(colnames(FHD), first_row, sep = "_")

# Replace headers and remove first row from data
FHD <- FHD[-1, , drop = FALSE]
colnames(FHD) <- new_headers

# then mutate surface and material type o
FHD <- FHD %>%
  mutate(material_type = 'flat') %>%
  mutate(surface = 'flat_high_density')

# flat medium density -------------------------------------------------------

FMD <- read.csv('flat med density/Flat_medDensity_Nuclei.csv')

# readjust

old_colnames <- colnames(FMD)

new_colnames <- as.character(FMD[1, ])

FMD_swapped <- FMD[-1, , drop = FALSE]

colnames(FMD_swapped) <- new_colnames

FMD <- rbind(old_colnames, FMD_swapped)

rm(FMD_swapped)

# now remove numbers and points from label column
FMD[1,] <- gsub('[^A-Za-z]', '', FMD[1,])

# Extract first row
first_row <- as.character(FMD[1, ])

# Create new combined headers
new_headers <- paste(colnames(FMD), first_row, sep = "_")

# Replace headers and remove first row from data
FMD <- FMD[-1, , drop = FALSE]
colnames(FMD) <- new_headers

# then mutate surface and material type o
FMD <- FMD %>%
  mutate(material_type = 'flat') %>%
  mutate(surface = 'flat_medium_density')


# flat low density -------------------------------------------------------

FLD <- read.csv('flat low density/Flat_lowDensity_Nuclei.csv')

# readjust

old_colnames <- colnames(FLD)

new_colnames <- as.character(FLD[1, ])

FLD_swapped <- FLD[-1, , drop = FALSE]

colnames(FLD_swapped) <- new_colnames

FLD <- rbind(old_colnames, FLD_swapped)

rm(FLD_swapped)

# now remove numbers and points from label column
FLD[1,] <- gsub('[^A-Za-z]', '', FLD[1,])

# Extract first row
first_row <- as.character(FLD[1, ])

# Create new combined headers
new_headers <- paste(colnames(FLD), first_row, sep = "_")

# Replace headers and remove first row from data
FLD <- FLD[-1, , drop = FALSE]
colnames(FLD) <- new_headers

# then mutate surface and material type o
FLD <- FLD %>%
  mutate(material_type = 'flat') %>%
  mutate(surface = 'flat_low_density')


# nN 2-700 ------------------------------------------------------------


nN_2_700 <- read.csv('nN 2-700/nN_2-700_Nuclei.csv')

# readjust

old_colnames <- colnames(nN_2_700)

new_colnames <- as.character(nN_2_700[1, ])

nN_2_700_swapped <- nN_2_700[-1, , drop = FALSE]

colnames(nN_2_700_swapped) <- new_colnames

nN_2_700 <- rbind(old_colnames, nN_2_700_swapped)

rm(nN_2_700_swapped)

# now remove numbers and points from label column
nN_2_700[1,] <- gsub('[^A-Za-z]', '', nN_2_700[1,])

# Extract first row
first_row <- as.character(nN_2_700[1, ])

# Create new combined headers
new_headers <- paste(colnames(nN_2_700), first_row, sep = "_")

# Replace headers and remove first row from data
nN_2_700 <- nN_2_700[-1, , drop = FALSE]
colnames(nN_2_700) <- new_headers

# then mutate surface and material type o
nN_2_700 <- nN_2_700 %>%
  mutate(material_type = 'nanoneedle') %>%
  mutate(surface = 'nN_2_700')

# nN 2-1000 ------------------------------------------------------------


nN_2_1000 <- read.csv('nN 2-1000/nN2-1000_Nuclei.csv')

# readjust

old_colnames <- colnames(nN_2_1000)

new_colnames <- as.character(nN_2_1000[1, ])

nN_2_1000_swapped <- nN_2_1000[-1, , drop = FALSE]

colnames(nN_2_1000_swapped) <- new_colnames

nN_2_1000 <- rbind(old_colnames, nN_2_1000_swapped)

rm(nN_2_1000_swapped)

# now remove numbers and points from label column
nN_2_1000[1,] <- gsub('[^A-Za-z]', '', nN_2_1000[1,])

# Extract first row
first_row <- as.character(nN_2_1000[1, ])

# Create new combined headers
new_headers <- paste(colnames(nN_2_1000), first_row, sep = "_")

# Replace headers and remove first row from data
nN_2_1000 <- nN_2_1000[-1, , drop = FALSE]
colnames(nN_2_1000) <- new_headers

# then mutate surface and material type o
nN_2_1000 <- nN_2_1000 %>%
  mutate(material_type = 'nanoneedle') %>%
  mutate(surface = 'nN_2_1000')


# nN 5-1000 ------------------------------------------------------------


nN_5_1000 <- read.csv('nN 5-1000/nN-5-1000_Nuclei.csv')

# readjust

old_colnames <- colnames(nN_5_1000)

new_colnames <- as.character(nN_5_1000[1, ])

nN_5_1000_swapped <- nN_5_1000[-1, , drop = FALSE]

colnames(nN_5_1000_swapped) <- new_colnames

nN_5_1000 <- rbind(old_colnames, nN_5_1000_swapped)

rm(nN_5_1000_swapped)

# now remove numbers and points from label column
nN_5_1000[1,] <- gsub('[^A-Za-z]', '', nN_5_1000[1,])

# Extract first row
first_row <- as.character(nN_5_1000[1, ])

# Create new combined headers
new_headers <- paste(colnames(nN_5_1000), first_row, sep = "_")

# Replace headers and remove first row from data
nN_5_1000 <- nN_5_1000[-1, , drop = FALSE]
colnames(nN_5_1000) <- new_headers

# then mutate surface and material type o
nN_5_1000 <- nN_5_1000 %>%
  mutate(material_type = 'nanoneedle') %>%
  mutate(surface = 'nN_5_1000')


# nN 5-700 ------------------------------------------------------------


nN_5_700 <- read.csv('nN 5-700/nN-5-700_Nuclei.csv')

# readjust

old_colnames <- colnames(nN_5_700)

new_colnames <- as.character(nN_5_700[1, ])

nN_5_700_swapped <- nN_5_700[-1, , drop = FALSE]

colnames(nN_5_700_swapped) <- new_colnames

nN_5_700 <- rbind(old_colnames, nN_5_700_swapped)

rm(nN_5_700_swapped)

# now remove numbers and points from label column
nN_5_700[1,] <- gsub('[^A-Za-z]', '', nN_5_700[1,])

# Extract first row
first_row <- as.character(nN_5_700[1, ])

# Create new combined headers
new_headers <- paste(colnames(nN_5_700), first_row, sep = "_")

# Replace headers and remove first row from data
nN_5_700 <- nN_5_700[-1, , drop = FALSE]
colnames(nN_5_700) <- new_headers

# then mutate surface and material type on
nN_5_700 <- nN_5_700 %>%
  mutate(material_type = 'nanoneedle') %>%
  mutate(surface = 'nN_5_700')


# join together ----------------------------------------------------------------

objects <- rbind(FHD, FLD, FMD, nN_2_700, nN_2_1000, nN_5_700, nN_5_1000)

# save progress

write.csv(objects, 'Individual_objects.csv')

objects <- read.csv('Individual_objects.csv')

# filter objects ---------------------------------------------------------------

# object size

df %>% # cut from 500 - 10,000px
  select(AreaShape_Area_Cells) %>%
  ggplot(aes(x = AreaShape_Area_Cells)) +
  geom_histogram(bins = 100) +
  xlim(0, 25000)

objects %>% # dont filter, just leave
  select(AreaShape_Area_Nuclei) %>%
  ggplot(aes(x = AreaShape_Area_Nuclei)) +
  geom_histogram(bins = 100) +
  xlim(0, 5000)

objects %>%
  filter(AreaShape_Area_Cells < 10000) %>%
  summarise(
    n = n()
  )

# 255190 originally then to 241639

objects <- objects %>%
  filter(AreaShape_Area_Cells >= 500) %>%
  filter(AreaShape_Area_Cells <= 10000)


objects %>% # dont filter, just leave
  select(Intensity_IntegratedIntensity_CorrectedActin_Cells) %>%
  ggplot(aes(x = Intensity_IntegratedIntensity_CorrectedActin_Cells)) +
  geom_histogram(bins = 100) +
  xlim(0, 500)


# filter stain intensity - then to 238116 - need to check outliers here for this

objects <- objects %>%
  filter(Intensity_IntegratedIntensity_CorrectedDNA_Cells > 10 & Intensity_IntegratedIntensity_CorrectedDNA_Cells < 250) %>%
  filter(Intensity_IntegratedIntensity_CorrectedYAP_Cells > 10 & Intensity_MinIntensity_CorrectedYAP_Cells < 550) %>%
  filter(Intensity_IntegratedIntensity_CorrectedActin_Cells > 1.5 & Intensity_IntegratedIntensity_CorrectedActin_Cells < 300) %>%
  filter(Intensity_IntegratedIntensity_CorrectedCycA_Nuclei > 2.5) %>%
  mutate(cycle_phase = case_when(
    Intensity_IntegratedIntensity_CorrectedCycA_Nuclei > 2.5 & Intensity_IntegratedIntensity_CorrectedCycA_Nuclei <= 30 ~ 'G1 Phase',
    Intensity_IntegratedIntensity_CorrectedCycA_Nuclei > 30 & Intensity_IntegratedIntensity_CorrectedCycA_Nuclei <= 200 ~ 'S Phase',
    Intensity_IntegratedIntensity_CorrectedCycA_Nuclei > 200 ~ 'G2 Phase',
    .default = NA
  )) %>%
  mutate(cycle_phase = as.factor(cycle_phase)) %>%
  mutate(cycle_phase = fct_relevel(cycle_phase, c('G1 Phase', 'S Phase', 'G2 Phase')))

write.csv(objects, 'individual_objects_filtered.csv')
