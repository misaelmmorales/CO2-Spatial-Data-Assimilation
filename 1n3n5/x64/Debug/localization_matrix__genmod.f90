        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:15 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LOCALIZATION_MATRIX__genmod
          INTERFACE 
            SUBROUTINE LOCALIZATION_MATRIX(RHO)
              USE GLOBAL_VARIABLES, ONLY :                              &
     &          NACT,                                                   &
     &          ND,                                                     &
     &          NG,                                                     &
     &          LOCAL_VAR,                                              &
     &          D_LOC,                                                  &
     &          NX,                                                     &
     &          NY,                                                     &
     &          NZ,                                                     &
     &          DX,                                                     &
     &          DY,                                                     &
     &          ACTINDX,                                                &
     &          N_E
              REAL(KIND=8), INTENT(OUT) :: RHO(NACT,ND)
            END SUBROUTINE LOCALIZATION_MATRIX
          END INTERFACE 
        END MODULE LOCALIZATION_MATRIX__genmod
