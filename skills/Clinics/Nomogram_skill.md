# Skill: Nomogram (R)

## Category
Clinics

## When to Use
Simply put, a nomogram graphically displays the results of logistic regression or Cox regression. It uses the regression coefficient of each independent variable to develop a scoring criteria, assigning a score to each independent variable value. A total score is then calculated for each patient, and a conversion function is used to convert this score into the probability of a specific outcome for that patient.

## Required R Packages
- readr
- regplot
- rms
- survival

## Minimal Reproducible Code
```r
# Load packages
library(readr)
library(regplot)
library(rms)
library(survival)

# Prepare data
## Loading data
clinical <- readr::read_tsv("https://bizard-1301043367.cos.ap-guangzhou.myqcloud.com/TCGA-LIHC.clinical.tsv")
LIHC <- cbind(clinical$sample,clinical[,c('gender.demographic',
                                             'vital_status.demographic',
                                             'days_to_death.demographic',
                                             'age_at_index.demographic',
                                             'ajcc_pathologic_stage.diagnoses')])
colnames(LIHC) <- c('bcr_patient_barcode','gender','status','time','age','stage')
table(LIHC$status)
LIHC <- LIHC[LIHC$status != 'Not Reported',]
LIHC$status <- as.numeric(ifelse(LIHC$status=='Dead','2','1') ) # Death is 2 in nomogram

# Create visualization
# Basic Nomogram
dd=datadist(LIHC)
options(datadist="dd")
## Build a logist model and draw a nomogram
f1 <- lrm(status ~ age + gender + stage , data =  LIHC)
nom <- nomogram(f1, fun=plogis, lp=F, funlabel="Risk")
plot(nom)
```

## Key Parameters
- `stat`: Statistical transformation to use
- `fill`: Maps a variable to fill color for group comparison
- `color`: Maps a variable to outline/point color

## Tips
- The tutorial includes a '2. Beautify Nomogram' section with advanced styling options
- Adjust text size with `theme(text = element_text(size = 14))` for presentations
- Follow CONSORT or STROBE guidelines for clinical data visualization where applicable

## Full Tutorial
https://openbiox.github.io/Bizard/Clinics/Nomogram.html
