# Readme <a href='https://osf.io/zcvbs/'><img src="worcs_icon.png" align="right" height="139"/></a>

This is the github of the manuscript "Too Little, Too Much: Evidence for Quadratic Patterns in Emotion Regulation Strategy Use in Daily Life".
Here, you can reproduce all analyses and documents from the pre-registration and manuscript.

The raw data from Medland et al. (2020) was downloaded from <https://osf.io/uqg23/> on March 24, 2023 and can be found under `raw data/RESSEMA_data.dat`.
Those data were used for the power analyses.

## Where do I start?

You can load this project in RStudio by opening the file called `TYM_ER_NonLinear.Rproj`.

## Access to data

The analytical dataset is hosted on a university data repository operated under the [FAIR principles](https://www.go-fair.org/fair-principles/):

<https://doi.org/10.34973/s90j-0r08>

The data file you need to download is called `data_evening.csv` and needs to be added into the parent folder `TYM_ER_context` (i.e., not into a subfolder!).

## Project structure

### Folder "TYM_ER_context" (parent folder)

Contains data files and associated information (codebooks).
Also contains other read-only files (e.g., licenses).

When downloading the data from the repository, you need to add the data file to this parent folder.

|  |  |  |
|--------------------------|--------------------------|--------------------|
| **File** | **Description** | **Usage** |
| .gitignore | Gitignore file | Read only |
| .worcs | WORCS metadata YAML | Read only |
| codebook_data_evening | Codebook & metadata dataset Track your Mood | Read only |
| codebook_ressema_df | Codebook & metadata dataset Medland (2020) | Read only |
| LICENSE | User permissions | Read only |
| README.md | Description of project | Human editable |
| renv.lock | Reproducible R environment | Read only |
| ressema_df.csv | Dataset Medland et al. (2020) - for power analyses | Read only |
| TYM_ER_NonLinear.Rproj | Project file | Loads project |
| value_labels_data_evening.yml | Value labels dataset Track your mood | Read only |
| value_labels_RESSEMA_df | Value labels dataset Medland (2020) | Read only |
| worcs_icon.png | Worcs icon | Read only |

### Folder "manuscript"

Contains all files relevant to the manuscript.
All text for the manuscript are in the file `manuscript.Rmd`.
All text for the supplementary material are in the file `supplementary_materials.Rmd`.

All other files are either called or created in this script.

Please run the scripts in the order of presentation (i.e., starting with `1_Manuscript_setup.R` and ending with `6_Manuscript_supplementary.R`).
You cannot run the file `0_Manuscript_data-prep.R` because this contains the raw data with still identifying information (that data file is not shared).

| File | **Description** | **Usage** |
|:-----------------------|:-----------------------|:-----------------------|
| apa.csl | APA 7 citation format | Read only |
| ERVariability_explained.png | Plot explaining Bray-Curtis ER Variability | Read only |
| figure1_negERonly.png | Plot study design | Read only |
| manuscript.pdf | Knitted pdf file of paper | Read only - changes through source code manuscript.Rmd |
| manuscript.Rmd | Source code for paper | Human editable |
| mlm_plots_er_cont.png | Plot multilevel model intensity | Read only - changes through source code manuscript.Rmd |
| mlm_plots_er_int.png | Plot multilevel model controllability | Read only - changes through source code manuscript.Rmd |
| supplementary_materials.pdf | Knitted pdf file of supplementary materials | Read only - changes through source code supplementary_materials.Rmd |
| supplementary_materials.Rmd | Source code for supplementary materials | Human editable |
| TYMCon_r-references.bib | BibTex references of R packages for manuscript | Human editable |
| TYMCon_references.bib | BibTex references for manuscript | Human editable |
| vio_apa.png | Violin plots | Read only - changes through source code manuscript.Rmd |

#### Subfolder "R"
The Subfolder `manuscript/R` contains all `R` analyses files.

| File | **Description** | **Usage** |
|:-----------------------|:-----------------------|:-----------------------|
| 0_Manuscript_data-prep.R | R script for preparing data set for sharing | Human editable |
| 1_Manuscript_setup.R | R script for analyses setup (e.g., libraries, options) | Human editable |
| 2_Manuscript_processing.R | R script for data processing (data checks etc) | Human editable |
| 3_Manuscript_descriptives.R | R script for participant characteristics and descriptive statistics | Human editable |
| 4_Manuscript_multilevel.R | R script for multilevel models (main analyses) | Human editable |
| 5_Manuscript_figures-tables.R | R script for creating figures and tables | Human editable |
| 6_Manuscript_supplementary.R | R script for conducting supplementary analyses | Human editable |
| BrayCurtisDissimilarity_Calculate.R | R script to calculate Bray Curtis dissimilarity scores | Human editable |

#### Subfolder "output"

The subfolder `manuscript/R/output` contains saved `R`-objects that I call in the manuscript and supplementary text files.
I added the descripton here, but they not included in this repo, but will be created when you run the `R` scripts.

| File | **Description** | **Usage** |
|:-----------------------|:-----------------------|:-----------------------|
| aic_bic_table.rds | Formatted table on linear versus quadratic model comparisons (saved R object) | Read only - changes through source code 5_Manuscript_figures-tables.R |
| all_estimates.rds | All estimates from multilevel models (saved R object) | Read only - changes through source code 4_Manuscript_multilevel.R |
| coefs.mod.adj.rds | Coefficients interaction test - formatted for table (saved R object) | Read only - changes through source code 5_Manuscript_figures-tables.R |
| coefs.n.adj.rds | Coefficients multilevel results test reported in manuscript - formatted for table (saved R object) | Read only - changes through source code 5_Manuscript_figures-tables.R |
| coefs.n.end.adj.rds | Coefficients multilevel endorsement change only - formatted for table (saved R object) - for supplement | Read only - changes through source code 5_Manuscript_figures-tables.R |
| coefs.p.adj.rds | Coefficients multilevel positive ER only (saved R object) - for supplement | Read only - changes through source code 6_Manuscript_supplementary.R |
| correlation.xlsx | Correlation matrix that was created in manuscript script | Human editable - changes through source code 3_Manuscript_descriptives.R |
| data_evening_clean.rds | Processed dataset (saved R object) | Read only - changes through source code 2_Manuscript_processing.R |
| data_person_clean.rds | Processed dataset - person-level aggregated (saved R object) | Read only - changes through source code 2_Manuscript_processing.R |
| descriptives.rds | Descriptive and participant information characteristics (saved R object) | Read only - changes through source code 3_Manuscript_descriptives.R |
| fits.n.rds | Fits from multilevel models (saved R object) | Read only - changes through source code 4_Manuscript_multilevel.R |
| names.rds | Several names used for table creation (saved R object) | Read only - changes through source code 4_Manuscript_multilevel.R |
| pred_quad_models.rds | Coefficients predicted values for models with significant quadratic effects (saved R object) | Read only - changes through source code 4_Manuscript_multilevel.R |
| pred_quad_table.rds | Coefficients predicted values for models with significant quadratic effects  - formatted for table (saved R object) | Read only - changes through source code 5_Manuscript_figures-tables.R |
| results_n_er_anova.rds | Results from linear versus quadratic model comparisons (saved R object) | Read only - changes through source code 4_Manuscript_multilevel.R |
| sample_flow.rds | Information of sample flow (before and after excluding participants) (saved R object) | Read only - changes through source code 2_Manuscript_processing.R |

### Folder "pre-registration"

Contains all files relevant to the pre-registration.
All text and analyses for the pre-registration are in the file `preregistration.Rmd`.
All other files are either called or created in this script.
For instance, I call the results from the script `Power-analyses.R`, which contain the simulation study for the power-analysis.

| File | Description | Usage |
|:-----------------------|:-----------------------|:-----------------------|
| apa.csl | APA 7 citation format | Read only |
| figure1.jpg | Plot study design | Read only |
| power_emo_control.xlsx | Results power analyses emotional control | Read only - changes through through source code Power-analyses.R |
| power_emo_intensity.xlsx | Results power analyses emotional intensity | Read only - changes through through source code Power-analyses.R |
| Power-analyses.R | Script for power analyses | Human editable |
| Power-analyses_prepare_data.R | Script to process raw data from Medland et al. (2020) | Human editable |
| preregistration.rmd | Source code for pre-registration | Human editable |
| preregistration.docx | Word file of pre-registration | Read only - changes through source code .Rmd |
| preregistration.pdf | PDF file of pre-registration | Read only - changes through source code .Rmd |
| references_prereg.bib | BibTex references for pre-registration | Human editable |
| wordtemplate.docx | Wordtemplate for knitting .rmd file | Human editable |

### Other files

| File | Description | Usage |
|:-----------------------|:-----------------------|:-----------------------|
| raw data/RESSEMA_data.dat | RESS data Medland et al. (2020) - for power analyses | Read only |
| raw data/RESSEMA_data.dat | RESS codebook Medland et al. (2020) - for power analyses | Read only |

## Reproducibility

Reproduce the results by these steps.

1.  Install RStudio and R.

2.  Install WORCS dependencies.

    `install.packages("worcs", dependencies = TRUE)`\
    `tinytex::install_tinytex()`

    *Note*: See <https://cjvanlissa.github.io/worcs/articles/setup.html> for more information on this step.

3.   Download the data from <https://doi.org/10.34973/s90j-0r08>. Without this step, you cannot run the analyses, but still check the code.

4.   [Clone](https://cjvanlissa.github.io/worcs/articles/reproduce.html#obtaining-the-project-repository) this repo (<https://github.com/DominiqueMaciejewski/TYM_ER_NonLinear.git>) to your RStudio

5.   Open the `R`project file called `TYM_ER_NonLinear.RProj`.

6.   To reproduce the power analyses, run the `preregistration/Power-analyses.R` and knit the `preregistration/preregistration.Rmd` file.

7.   To reproduce the manuscript including the results, run the R scripts in the order of presentation from the folder `manuscript/R` 
(i.e., starting with `1_Manuscript_setup.R` and ending with `6_Manuscript_supplementary.R`). 
You do not need to run the script `0_Manuscript_data-prep`. This was just for me to prepare the dataset for sharing).
    Then, knit the `manuscript/manuscript.Rmd` file and `manuscript/supplementary_materials.Rmd`.

  *Note: In an earlier version of this GitHub repo, I worked with the renv package to restore the exact package dependencies used in the analyses. 
  However, I personally found that there were always issues when trying to activate the project.
  So, I decided to not use this package anymore and instead give all the versions of packages used in `sessionInfo()`
  (found in the file `1_Manuscript_setup.R`). 
  Given that the analyses here are relatively standard multilevel models, I do not expect different results with different packages.*
    
# Reproducibility

This project uses the Workflow for Open Reproducible Code in Science (WORCS) to ensure transparency and reproducibility.
The workflow is designed to meet the principles of Open Science throughout a research project.

To learn how WORCS helps researchers meet the TOP-guidelines and FAIR principles, read the preprint at <https://osf.io/zcvbs/>

## WORCS: Advice for authors

-   To get started with `worcs`, see the [setup vignette](https://cjvanlissa.github.io/worcs/articles/setup.html)
-   For detailed information about the steps of the WORCS workflow, see the [workflow vignette](https://cjvanlissa.github.io/worcs/articles/workflow.html)

## WORCS: Advice for readers

Please refer to the vignette on [reproducing a WORCS project]() for step by step advice.

