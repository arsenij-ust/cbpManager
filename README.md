# cbpManager

This R package provides an R Shiny application that enables the user to generate, manage and edit data and metadata files suitable for the import in cBioPortal for Cancer Genomics. 

## Install

This project uses [node](http://nodejs.org) and [npm](https://npmjs.com). Go check them out if you don't have them locally installed.

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
