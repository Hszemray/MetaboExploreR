
# MetaboExploreR: Targeted Lipidomics Processing Pipeline

**Authors**: Harrison John Szemray, Luke Gray Whiley **Date**: July 10,
2025

## Overview

MetaboExploreR provides an automated pipeline for processing and quality
control of targeted lipidomics data. The toolkit consists of two main
functions:

1.  **`SkylineR()`**: Processes raw mass spectrometry data (.wiff
    files), performs retention time optimization, and prepares data for
    Skyline analysis, saves Skyline output to project directory.
    (WINDOWS OS COMPATIBLE ONLY)

2.  **`qcCheckeR()`**: Performs comprehensive quality control, batch
    correction, and generates analytical reports ready for data
    analysis.

## Installation

## Directory Structure

Create this proejct folder structure before execution:

``` r
project_directory/
├── wiff/          # Raw .wiff files and associated .wiff.scan files
```

## Functions

### 1. SkylineR()

``` r
SkylineR(project_directory, mrm_template_list, QC_sample_label)
```

#### Processing Workflow:

1.  Converts .wiff → .mzML via ProteoWizard  
2.  Performs retention time optimisation  
3.  Generates Skyline input files  
4.  Executes Skyline processing  
5.  Organises output into plate-specific folders

### 2. qcCheckeR()

``` r
qcCheckeR(project_directory, mrm_template_list, QC_sample_label, user_name)
```

#### Quality Control Workflow:

1.  Batch correction using statTarget  
2.  Sample and feature filtering  
3.  Quality metric calculation (RSD, missing values)  
4.  Generation of:
    - Interactive PCA plots  
    - Control charts  
    - Quality summary tables  
    - HTML/Excel reports

## Usage Example

``` r
# Process raw data
SkylineR(project_directory = "USER/PATH/TO/PROJECT/DIRECTORY", mrm_template_list = list("user_mrm_guide_v1.tsv", "user_mrm_guide_v2.tsv"))

# Perform quality control
qcCheckR(project_directory = "path/to/project_directory", mrm_template_list = mrm_template_list, user_name = "Jane_Doe")
```

## Output Structure

Post-execution directory of SkylineR and qcCheckR:

    User Project Directory
    ├── Archive #Archive of all mzml and wiff files for backup
    ├── ALL
    │   ├── data
    │   │   ├── batch_correction
    │   │   ├── skyline_reports
    │   └── html_report #Summary of project, PCA, and control charts
    │   └── xlsx_report # Data for further user analysis
    ├── PLATE_X (This structure will be created per plate)
        ├── data
            ├── mzml #mzml files from proteowizard
            ├── rda  #saved rda from processing
            ├── sciex_raw #original .wiff files
            └── skyline #input and output files of Skyline processing
        

## Technical Notes

1.  **Dependencies**:
    - ProteoWizard installed at `C:/Program Files/ProteoWizard/`  
    - Skyline installed at `C:/Program Files/Skyline/`  
2.  **Memory Management(Automated)**:
    - Process large datasets plate-by-plate
    - Offload processed plate data before processing next plate
3.  **Troubleshooting**:
    - Run R as systems administrator
    - Ensure your R, Rstudio and package versions are all up to date
    - Check .wiff files for acquisition errors  
    - Ensure sufficient disk space (2GB/plate recommended)
    - Minimum of 8GB RAM required

## Support

For technical assistance, please open an issue at:  
<https://github.com/Hszemray/MetaboExploreR>

> **Version Information**: LipidExplorer v6.0 (2025-06-30)
