**Load a valid copy number data file.** This file is used in cBioPortal for the copy number data panel (see image on the right.)

---

#### Description of a valid copy number alteration data file:

The user can currently upload the file **"all_thresholded.by_genes.txt"** derived from GISTIC 2.0 or a **tab-separated file** containing the following columns:

One or both of:

- **Hugo_Symbol (Required)**: A HUGO gene symbol.
- **Entrez_Gene_Id (Optional, but recommended)**: An Entrez Gene identifier.

**For each sample in the dataset an additional column is required using the sample ID as column header.** Please make sure that all of the sample IDs have been added to the loaded study via the Sample tab before upload the file.

The discrete copy number data file contains for each gene-sample combination a copy number level. By default the following applies:
- "-2" is a deep loss, possibly a homozygous deletion
- "-1" is a single-copy loss (heterozygous deletion)
- "0" is diploid
- "1" indicates a low-level gain
- "2" is a high-level amplification

The information "profile_name" and "profile_description" listed in the metafile can be adapted to the uploaded file (see "Change metadata of copy number alteration" below). It is recommended to provide the source and a brief characterization of the data under "profile_name" and to define the interpretation of the copy number level in the "profile_description". 