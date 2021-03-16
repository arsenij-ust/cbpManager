The following columns affect the header of the patient view by adding text to the samples in the header:

- **'SAMPLE_DISPLAY_NAME'**: displayed in addition to the ID.
- **'SAMPLE_CLASS'**
- **'METASTATIC_SITE'** or **'PRIMARY_SITE'**: Override 'TUMOR_SITE' (patient level attribute) depending on sample type.

The following columns additionally affect the timeline data visualization:

- **'OTHER_SAMPLE_ID'**
- **'SAMPLE_TYPE'**, **'TUMOR_TISSUE_SITE'** or **'TUMOR_TYPE'**: gives sample icon in the timeline a color.
  - If set to recurrence, recurred, progression or progressed: orange
  - If set to metastatic or metastasis: red
  - If set to primary or otherwise: black
