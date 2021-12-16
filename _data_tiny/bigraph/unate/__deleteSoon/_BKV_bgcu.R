# file = _BKV_bgcu.R ;# best-known-values of minimum cover for this directory
# Franc Brglez
# dateLast = "Sat Mar 20 15:10:10 2021"
#

# make key note
glob[['bg_urn2_9_16.cnfU']]       = 3
glob[['team_u_5_3.cnfU']]         = 2  # solutionBestCnt = 4
glob[['school_5_6.cnfU']]         = 3  # solutionBestCnt = 1
glob[['school_5_6.cnfW']]         = 5  # solutionBestCnt = 1
glob[['school_5_6.txt']]          = 5  # solutionBestCnt = 1
glob[['school_6_7.cnfU']]         = 3  # solutionBestCnt = 1
glob[['school_7_8.cnfU']]         = 3  # solutionBestCnt = 1
glob[['team_u_9_10.cnfU']]        = 4  # solutionBestCnt = 2
glob[['school_9_16_5_1.cnfU']]    = 5  # solutionBestCnt = 1
glob[['affil_9_12_5418.cnfU']]    = 4  # solutionBestCnt = 3
glob[['affil_9_12_5418_man.cnf']] = 6  # solutionBestCnt = 1
glob[['ruler_10_21_6_15.cnfU']]   = 6

glob[['school_17_18.cnfW']]  = 10.5 
glob[['school_17_18.txt']]   = 10.5 
glob[['school_19_20.txt']]   = 100 

### FOR TODAY:
# -- convert school_17_18.cnfW to school_17_18.txt
# -- convert school_19_20.cnfW to school_19_20.txt
# -- run brkga with instanceDef=school_17_18.txt  (it should stop on valueTarget = 10.5)
# -- run brkga with instanceDef=school_19_20.txt  (it should stop on valueTarget < 100)
# -- run brkga with instanceDef=school_19_20.txt  AND instance valueTarget=valueBest from previous run
# -- run brkga with instanceDef=school_19_20.txt  AND instance valueTarget=valueBest from previous run
#    ... etc etc
# -- then update THIS file with
#    glob[['school_17_20.txt']]    = best-known-value
#    glob[['school_19_20.cnfW']]   = best-known-value
#    
# — then run your ga-version on both
#    school_17_18.txt and school_19_20.txt
# with valueTarget from THIS file 
#   
   
### after we discuss, add all MISSING vaules from thisDir/*.cnfU, thisDir/*.cnfW, ihisDir/*.txtU, thisDir/*.txt

## action item for LATER (function OFb_bgcu_exh(instanceDef):

# neither file school_19_20.cnfU not school_19_20.cnfW
# completes and must be aborted after 1 hour ...
# (old code with OLD data structures would complete for nCoords = 2^20 !!!)
# see below ...
# >   instanceDef = "../../_data_tiny/bigraph/unate/school_19_20.cnfW" 
# >   OFb_bgcu_exh(instanceDef)
# .. initialized instance as datatable 'dataTb' under global array glob
# .. initialized instance as incidence 'matrixI' under global array glob
# .. reading all binary coordinates from file
# ./bina_19.txt 
# Read 524288 items
