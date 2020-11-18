mutationsTab <- tabItem(
  tabName = "mutations",
  h2("Mutations"),
  fluidRow(
    width = 12,
    box(
      title="Description",
      collapsible = TRUE,
      collapsed = TRUE,
      solidHeader = TRUE,
      htmltools::HTML(
        "<p><b>Load a valid Mutation Annotation Format (MAF) file.</b> This file is used in cBioPortal for the mutation panel (see image on the right.)</p>
        <h4>A minimal MAF file with only the columns required for cBioPortal:</h4>
        <p>A minimal mutation annotations file can contain just three of the MAF columns plus one annotation column. From this minimal MAF, it is possible to create an extended MAF by running maf2maf.</p>
        <ul>
        <li><b>Hugo_Symbol (Required)</b>: (MAF column) A HUGO gene symbol.</li>
        <li><b>Tumor_Sample_Barcode (Required)</b>: (MAF column) This is the sample ID as listed in the clinical data file.</li>
        <li><b>Variant_Classification (Required)</b>: (MAF column) Translational effect of variant allele.</li>
        <li><b>HGVSp_Short (Required)</b>: (annotation column) Amino Acid Change, e.g. p.V600E.</li>
        </ul>
        <p>Next to Hugo_Symbol, it is recommended to have the Entrez gene ID:</p>
        <ul>
        <li><b>Entrez_Gene_Id (Optional, but recommended)</b>: An Entrez Gene identifier.</li>
        </ul>
        <p>The following extra annotation columns are important for making sure mutation specific UI functionality works well in the portal:</p>
        <ul>
        <li><b>Protein_position (Optional)</b>: (annotation column) Required to initialize the 3D viewer in mutations view.</li>
        <li><b>SWISSPROT (Optional)</b>: (annotation column) UniProtKB/SWISS-PROT name (formerly called ID) or accession code depending on the value of the 'swissprot_identifier' metadatum, e.g. O11H1_HUMAN or Q8NG94. Is not required, but not having it may result in inconsistent PDB structure matching in mutations view.</li>
        </ul>"
      ),
      width = 6
      ),
    box(
      title="Sample from cBioPortal",
      collapsible = TRUE,
      collapsed = FALSE,
      solidHeader = TRUE,
      tags$head(
        tags$style(
          type="text/css",
          "#MutDataImg img {max-width: 100%; width: 100%; height: auto}"
        )
      ),
      imageOutput(
        "MutDataImg",
        height = "auto"
      ),
      width = 6
    )
    ),
  fluidRow(
    width = 12,
    box(
      column(
        3,
        fileInput("chooseMAF", "Choose MAF File",
                  multiple = FALSE,
                  accept = c("text/tsv",
                             "text/tab-separated-values,text/plain",
                             ".tsv", ".txt", ".maf", ".MAF"
                  )
        ),
        actionButton("saveMAF", "Save MAF file", class = "btn-success")
      ),
      column(
        9,
        DT::DTOutput("MAFdata")
      ),
      width = 12
    )
  )
)
