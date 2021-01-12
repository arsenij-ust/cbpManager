# Abstract <img src="vignettes/images/logo.png" align="right" width="150px" />

Intuitive visualization and interactive exploration of multidimensional cancer genomics data sets is essential to the field of cancer genomics. The cBioPortal for Cancer Genomics is an open-access, open-source tool that can integrate different types of alterations with clinical data. "The goal of cBioPortal is to significantly lower the barriers between complex genomic data and cancer researchers by providing rapid, intuitive, and high-quality access to molecular profiles and clinical attributes from large-scale cancer genomics projects, and therefore to empower researchers to translate these rich data sets into biologic insights and clinical applications." (read more about cBioPortal for Cancer Genomics [here](https://www.cbioportal.org/faq).) cBioPortal enables the installation of an own instance for the analysis of your own data. The data for uploading to the own instance must have certain file formats. Although these specifications are documented in detail [here](https://docs.cbioportal.org/5.1-data-loading/data-loading/file-formats), the creation of such specific files is not easy for medical professionals or technically inexperienced persons and is often very time-consuming.

The R package cbpManager provides an R Shiny application that facilitates the generation of files suitable for the import in cBioPortal for Cancer Genomics. It enables the user to manage and edit clinical data maintain new patient data over time.

## Install

```
remotes::install_gitlab("arsenij_temp/cbpmanager", host="https://gitlab.miracum.org/")

```

## Usage

After installation, run the Shiny application with the following R command:

```
cbpManager::launchApp("path/to/study")
```

Optionally you can provide further parameters to `cbpManager::launchApp()` function that are used by `shiny::runApp`, e.g.
`host` or `port`.

### File naming convention

If cbpManager should recognize files of a study, the files should be named as following:

- data_clinical_patient.txt (Clinical Data)
- data_clinical_sample.txt (Clinical Data)
- data_mutations_extended.txt (Mutation Data)

- meta_study.txt (Cancer Study)
- meta_clinical_patient.txt (Clinical Data)
- meta_clinical_sample.txt (Clinical Data)
- meta_mutations_extended.txt (Mutation Data)

Optional files:

- data_timeline_surgery.txt / meta_timeline_surgery.txt 
- data_timeline_status.txt / meta_timeline_status.txt
- data_timeline_treatment.txt / meta_timeline_treatment.txt

Further custom timeline tracks should be named:

e.g. data_timeline_<custom>.txt / meta_timeline_<custom>.txt

For further details see [File Formats](https://docs.cbioportal.org/5.1-data-loading/data-loading/file-formats) and the 'testpatient' study in this package under 'inst/study/'.

## Dockerized usage

See [here](https://gitlab.miracum.org/arsenij_temp/cbpmanager.deploy)

## Maintainers

Arsenij Ustjanzew (arsenij.ustjanzew@gmail.com)

## Contributing

Feel free to dive in! [Open an issue](https://gitlab.miracum.org/mainz/cbpmanager/-/issues) or submit PRs.


## License

[MIT](LICENSE)
