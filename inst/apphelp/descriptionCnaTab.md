**Load a valid copy number data file.** This file is used in cBioPortal for the copy number data panel (see image on the right.)

---

#### A minimal copy number data file with only the columns required for cBioPortal:

The discrete copy number data file contain values that would be derived from copy-number analysis algorithms like GISTIC 2.0 or RAE. For each gene (row) in the data file, the following columns are required in the order specified:

- **Hugo_Symbol (Required)**: A HUGO gene symbol.

Next to Hugo_Symbol, it is recommended to have the Entrez gene ID:

- **Entrez_Gene_Id (Optional, but recommended)**: An Entrez Gene identifier.

For each sample in the dataset an additional column is required using the sample id as the column header:

- **SAMPLE_ID**: A sample ID. This field can only contain numbers, letters, points, underscores and hyphens.

For each gene-sample combination, a copy number level is specified. By default, the following applies:
- "-2" is a deep loss, possibly a homozygous deletion
- "-1" is a single-copy loss (heterozygous deletion)
- "0" is diploid
- "1" indicates a low-level gain
- "2" is a high-level amplification

Standard settings can be changed by selecting "Change global description" below.