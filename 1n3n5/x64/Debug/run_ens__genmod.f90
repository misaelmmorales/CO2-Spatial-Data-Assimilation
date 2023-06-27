        !COMPILER-GENERATED INTERFACE MODULE: Sun Jun 27 10:28:37 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RUN_ENS__genmod
          INTERFACE 
            SUBROUTINE RUN_ENS(N,N_E,ND,NT,NTA,M_UC,DF,A)
              USE GLOBAL_VARIABLES, ONLY :                              &
     &          N_PROCESSOR,                                            &
     &          NDATA,                                                  &
     &          NA,                                                     &
     &          CASENAME,                                               &
     &          GENERIC_DIR_NAME,                                       &
     &          MODELNAME,                                              &
     &          DATAASSIM_TYPE
              INTEGER(KIND=4), INTENT(IN) :: NT
              INTEGER(KIND=4), INTENT(IN) :: ND
              INTEGER(KIND=4), INTENT(IN) :: N_E
              INTEGER(KIND=4), INTENT(IN) :: N
              INTEGER(KIND=4), INTENT(IN) :: NTA
              REAL(KIND=8) :: M_UC(N,N_E)
              REAL(KIND=8) :: DF(NDATA,N_E)
              INTEGER(KIND=4) :: A
            END SUBROUTINE RUN_ENS
          END INTERFACE 
        END MODULE RUN_ENS__genmod
