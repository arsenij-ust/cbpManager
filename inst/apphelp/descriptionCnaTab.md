**Load a valid Copy Number Data file.** This file is used in cBioPortal for the cna panel (see image on the right.)

---

#### A minimal CNA file with only the columns required for cBioPortal:

A minimal CNA file can contain just one obligatory column plus an additional column for each sample in the dataset using the sample id as the column header. From this minimal file, it is possible to create an extended version.

- **Hugo_Symbol (Required)**: (CNA column) A HUGO gene symbol.

Next to Hugo_Symbol, it is recommended to have the Entrez gene ID:

- **Entrez_Gene_Id (Optional, but recommended)**: An Entrez Gene identifier.

The following extra annotation columns are important for complementing or replacing default driver annotation resources OncoKB and HotSpots:

- **SAMPLE_ID**: (annotation column) A sample ID. This field can only contain numbers, letters, points, underscores and hyphens.
- **cbp_driver (Optional)**: (annotation column) "Putative_Driver", "Putative_Passenger", "Unknown", "NA" or "" (empty value). This field must be present if the cbp_driver_annotation is also present in the MAF file.
- **cbp_driver_annotation (Optional)**: Description field for the cbp_driver value (limited to 80 characters) This field must be present if the cbp_driver is also present in the MAF file. This field is free text. Example values for this field are: "Pathogenic" or "VUS".
- **cbp_driver_tiers (Optional)**: Free label/category that marks the mutation as a putative driver such as "Driver", "Highly actionable", "Potential drug target". This field must be present if the cbp_driver_tiers_annotation is also present in the MAF file. In the OncoPrint view's Mutation Color dropdown menu, these tiers are ordered alphabetically. This field is free text and limited to 20 characters. For mutations without a custom annotation, leave the field blank or type "NA".
- **cbp_driver_tiers_annotation (Optional)**: Description field for the cbp_driver_tiers value (limited to 80 characters). This field must be present if the cbp_driver_tiers is also present on the MAF file. This field can not be present when the cbp_driver_tiers field is not present.

