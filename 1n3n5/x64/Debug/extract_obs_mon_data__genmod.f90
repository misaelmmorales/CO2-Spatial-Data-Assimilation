        !COMPILER-GENERATED INTERFACE MODULE: Sat Jan 16 09:34:52 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE EXTRACT_OBS_MON_DATA__genmod
          INTERFACE 
            SUBROUTINE EXTRACT_OBS_MON_DATA(D_RAW,DF)
              USE GLOBAL_VARIABLES, ONLY :                              &
     &          ND,                                                     &
     &          NT,                                                     &
     &          NTA,                                                    &
     &          OBS_FLAG,                                               &
     &          ASSIM_TSTEPS,                                           &
     &          NDATA,                                                  &
     &          ORIG_LOC,                                               &
     &          OBS_LOC
              REAL(KIND=8) :: D_RAW(NT,ND+1)
              REAL(KIND=8) :: DF(NDATA,1)
            END SUBROUTINE EXTRACT_OBS_MON_DATA
          END INTERFACE 
        END MODULE EXTRACT_OBS_MON_DATA__genmod
