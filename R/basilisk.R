#' Validation Dependencies
#'
#' Vector defining a set of Python dependencies and versions required to operate
#' with the validation scripts for cBioPortal
#'
#' @format
#' A character vector containing the pinned versions of all Python packages on
#' which the import validation depends.
#'
#' @name ValidationDependencies
.validation_dependencies <- c(
  "requests==2.24.0",
  "pyyaml==5.4.1",
  "jinja2==2.11.2",

  #
  # Following packages are dependencies from the above:
  #
  "brotlipy==0.7.0",
  "certifi==2020.12.5",
  "cffi==1.14.5",
  "chardet==3.0.4",
  "cryptography==3.4.4",
  "idna==2.10",
  "MarkupSafe==1.1.1",
  "pip==21.0.1",
  "pycparser==2.20",
  "pyOpenSSL==20.0.1",
  "PySocks==1.7.1",
  "six==1.15.0",
  "urllib3==1.25.11",
  "wheel==0.36.2"
)

env_validation <- BasiliskEnvironment(
  envname = "env_validation",
  pkgname="cbpManager",
  packages = .validation_dependencies,
  channels = c("bioconda", "conda-forge")
)
