#### Required columns

- The first column **'PATIENT_ID'** is **obligatory** and contains patient identifier. (Please do not override the cells containing 'Patient Identifier' and 'Patient identifier' in the first column.) 
- The second column **'SAMPLE_ID'** is also **required** and contains a unique sample identifier.

By adding 'PATIENT_ID' here, cBioPortal will map the given sample to this patient. This enables one to associate multiple samples to one patient. For example, a single patient may have had multiple biopsies, each of which has been genomically profiled.

---

#### Optional columns

Further columns contain variable sample attributes. You can select a predefined attribute. These attributes are described in the following. Optionally you can add a custom column.

The following columns are required for the pan-cancer summary statistics tab:

- **'CANCER_TYPE'**: Overrides study wide cancer type.
- **'CANCER_TYPE_DETAILED'**: Cancer Type Detailed, a sub-type of the specified 'CANCER_TYPE'.
