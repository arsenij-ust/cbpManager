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
  "abc==",
  "argparse==",
  "base64==",
  "collections==",
  "csv==",
  "datetime==",
  "importlib==",
  "itertools==",
  "jinja2==",
  "json==",
  "logging==",
  "logging.handlers==",
  "math==",
  "os==",
  "pathlib==",
  "pyyaml==5.4.1",
  "re==",
  "requests==",
  "subprocess==",
  "sys==",
  "tempfile==",
  "traceback==",
  "urllib.parse",
  "xml.etree.ElementTree==",
  "yaml=="
)

env_validation <- BasiliskEnvironment(
  envname = "env_validation",
  pkgname="cbpManager",
  packages = .validation_dependencies,
  channels = c("bioconda", "conda-forge")
)
