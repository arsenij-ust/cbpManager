# generateUIwidgets works correctly

    <div class="row">
      <div class="col-sm-8">
        <div class="form-group shiny-input-container">
          <label class="control-label" id="addPatientInput_TEST-label" for="addPatientInput_TEST">TEST</label>
          <input id="addPatientInput_TEST" type="text" class="form-control" value=""/>
        </div>
      </div>
    </div>

---

    <div class="row">
      <div class="col-sm-8">
        <div class="form-group shiny-input-container">
          <label class="control-label" id="addPatientInput_OS_STATUS-label" for="addPatientInput_OS_STATUS">OS_STATUS</label>
          <div>
            <select id="addPatientInput_OS_STATUS"><option value="DECEASED">DECEASED</option>
    <option value="LIVING">LIVING</option></select>
            <script type="application/json" data-for="addPatientInput_OS_STATUS" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
          </div>
        </div>
      </div>
    </div>

---

    <div class="row">
      <div class="col-sm-8">
        <div class="form-group shiny-input-container">
          <label class="control-label" id="editSampleInput_PATIENT_ID-label" for="editSampleInput_PATIENT_ID">PATIENT_ID</label>
          <div>
            <select id="editSampleInput_PATIENT_ID"><option value="p1">p1</option>
    <option value="p2" selected>p2</option></select>
            <script type="application/json" data-for="editSampleInput_PATIENT_ID" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
          </div>
        </div>
      </div>
    </div>

