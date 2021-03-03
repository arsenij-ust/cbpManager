#### Visualization of timeline data 

The timeline data is a representation of the various events that occur during the course of treatment for a patient from initial diagnosis. In cBioPortal timeline data is represented as one or more tracks in the patient view. This application produces separate files for treatment, surgery and status events (see tabs below). Add a treatment, a surgery or a status to a patient by providing the Patient ID and the necessary dates. The event timepoints are calculated in days from the date of diagnosis. The 'Date of the first Diagnosis' act as point zero on the timeline scale.

---

#### Set the date of first diagnosis:
Before adding timeline events, provide a date of first diagnosis (the zero point in cBioPortal) in the table on the right. Click on **Add date**, select the Patient ID (the ID's come from the 'Patient' tab) and add the date.

---

#### Edit timeline tables:

A row in the timeline tables represents a timeline event in a timeline track.

- **Add:** Add a new row. A dialog window opens with input fields per column of the table.
- **Edit:** Edit a row. Select a row by clicking on it and then click on 'Edit' to edit the values in this row. **Note:** When editing an event the correct dates should be set again!
- **Delete:** Select a row by clicking on it and then click on 'Delete' to delete the selected entry.
- **Add column(s):** After clicking on 'Add column(s)', provide a column name and create the new column.
- **Delete column(s):** Choose the columns you want to delete.
- **Save:** Saves the edited table to the final file.

---

#### Custom timeline tracks:

At the bottom of this page you can create and manage custom timeline tracks.
First provide a name for your custom track. This name will be used as "EVENT_TYPE" in the final file. The naming of the final file will be `data_timeline_<track-name>.txt` (& `meta_timeline_<track-name>.txt`). 
In the drop down menu you can choose from already existing custom timelines.
To edit the selected timeline track, click on **Edit track**. A table with the selected track data will appear below.
