#!/usr/bin/env python3

# wrapper function to provide argments and execute functions from the script "validateData.py"
# imporerDir - directory containing the importer module
# studyDir - directory of the study to validate
# outfile - file where the report should be saved
# 
# returns an exitCode from the main_validate function
def executeScript(importerDir, studyDir, outfile):
    import os
    import sys
    sys.path.append(importerDir)
    
    from importer import validateData
    
    args = validateData.interface(["-s"+studyDir+"/", "-n", "-v", "-html", outfile])
    return validateData.main_validate(args)
