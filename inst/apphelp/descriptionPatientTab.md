**Add clinical data, which are shown in cBioPortal in form of the table** (see image on the right.)

---

#### Data:

The first two rows contain a **short name**  (1st row) and a **longer name** (2nd row) of the clinical attribute.

- The first column **'PATIENT_ID'** is **obligatory** and should contain a unique patient identifier. (Please do not override the cells containing 'Patient Identifier' and 'Patient identifier' in the first column.)

Several pre-defined attributes are described in the following:

- **'OS_STATUS'**:   Overall patient survival status. Possible values: 1:DECEASED, 0:LIVING
- **'OS_MONTHS'**:   Overall survival in months since initial diagnosis.
- **'DFS_STATUS'**:  Disease free status since initial treatment. Possible values: 0:DiseaseFree, 1:Recurred/Progressed
- **'DFS_MONTHS'**:  Disease free (months) since initial treatment.
- **'PATIENT_DISPLAY_NAME'**: Patient display name (string).
- **'GENDER'**:      Gender or sex of the patient (string).
- **'AGE'**:         Age at which the condition or disease was first diagnosed, in years (number).

---

#### Edit table:

- **Add patient:** opens a dialog window with input fields per column of the table.
- **Edit patient:** select a row by clicking on it and then click on 'Edit patient' to edit the values in this row.
- **Import patient** allows you to import patients from other studies. The patient data, sample data, mutation data, and timeline data associated with the selected patient will be copied in the current study.
- **Delete patient:** select a row by clicking on it and then click on 'Delete patient' to delete the selected entry.
- **Add column(s):** after clicking on 'Add column' choose whether you want to add a pre-defined column or create a custom column. In case of a custom column, you need to provide a 'column name', a 'short name' and a 'long name' for this column.
- **Delete column(s):** choose the columns you want to delete.
- **Save:** saves the edited table to the final file.
