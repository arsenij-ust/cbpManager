**Load a valid Mutation Annotation Format (MAF) file.** This file is used in cBioPortal for the mutation panel (see image on the right.)

---

#### A minimal MAF file with only the columns required for cBioPortal:

A minimal mutation annotations file can contain just three of the MAF columns plus one annotation column. From this minimal MAF, it is possible to create an extended MAF by running maf2maf.

- **Hugo_Symbol (Required)**: (MAF column) A HUGO gene symbol.
- **Tumor_Sample_Barcode (Required)**: (MAF column) This is the sample ID as listed in the clinical data file.
- **Variant_Classification (Required)**: (MAF column) Translational effect of variant allele.
- **HGVSp_Short (Required)**: (annotation column) Amino Acid Change, e.g. p.V600E.

Next to Hugo_Symbol, it is recommended to have the Entrez gene ID:

- **Entrez_Gene_Id (Optional, but recommended)**: An Entrez Gene identifier.

The following extra annotation columns are important for making sure mutation specific UI functionality works well in the portal:

- **Protein_position (Optional)**: (annotation column) Required to initialize the 3D viewer in mutations view.
- **SWISSPROT (Optional)**: (annotation column) UniProtKB/SWISS-PROT name (formerly called ID) or accession code depending on the value of the 'swissprot_identifier' metadatum, e.g. O11H1_HUMAN or Q8NG94. Is not required, but not having it may result in inconsistent PDB structure matching in mutations view.

