Scripts and their functions:

- data_preprocessing_2.R: data cleaning and pre-processing after the cell profiler output. reformats the data for further analysis.

- GMM_Normalisation.R scripts: normalised the pixel intestity values for DNA and cyclin A stains across images.

- Cell_Morphology.R: all statsitics and plotting for comparing the morphology of cells

- Nucleus_Morphology.R: all statsitics and plotting for comparing the morphology of nuclei

- cell_cycle.R: all stats and plotting for cell cycle differences, and YAP staining. also assigns cells into their cycle phases

- orientation.R: adjusts the orientation values per object and contains all stats and plotting as in the report.

- Orientation_density_plots.R: plots the kernel density per condition

- SPM_preprocessing.R: prepares and formats data for input into spm. this is crucial to run before spm and has instructions within.

- spm.py: runs spm. results will be output automatically and need to be saved individually per run.  

Note that this is the order that the scripts should be run in order to repeat the analysis as in the dissertation report.


RNA-SEQUENCING: =========================================================================================================

- functional_enrichment_analysis.R: this is required to produce the visualisations (volcano plot, FEA plot) as seen in the dissertation report.
- this should be run after the galaxy pipeline.

GALAXY PIPELINE OVERVIEW:

<img width="399" height="470" alt="RNA-Seq Methods" src="https://github.com/user-attachments/assets/e4288895-8b04-4c69-8f87-023de725734a" />

Specific software used is found in the dissertation report.

NOTES FOR RUNNING THE PIPELINE:

- in deseq2, you should put your test condition (not control) first when inputting your datasets

- in deseq2, its useful to apply something called 'lfc shrinkage' which is listed in the advanced options. you can google which type to use, but I chose to use 'apeglm'. shrinkage adjusts log2fc values in genes which have low expression, since otherwise these can get inflated. 

- you need to download the dataset file from the deseq2 output after it is done running - it will download as a .tabular file. you can go into your downloads, and edit the .tabular extension to .tsv instead. then open this in google sheets, and add a new column at the top and manually label each column. then export as csv, and you're ready to use the R script. note: you will need to change some variable names in the R script if you name the headers differently to how I did. you can see what the data in each column is by looking at the dataset in galaxy.

- to identify the functionally expressed pathways you need to get a list of the gene ids. go to this website: https://www.biotools.fr/mouse/ensembl_symbol_converter and paste in your column of emsembl ids (which will start with ENSG). it is important to do this because it means that your gene annotations will be up to date. turn the results into a two column csv which are both labelled. you may need to adapt the R script accordingly.
