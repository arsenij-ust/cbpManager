#!/usr/bin/env python3

def executeScript(importerDir, studyDir, outfile):
    import os
    import sys
    sys.path.append(importerDir)
    
    from importer import validateData
    
    args = validateData.interface(["-s"+studyDir+"/", "-n", "-v", "-html", outfile])
    #return args
    return validateData.main_validate(args)


