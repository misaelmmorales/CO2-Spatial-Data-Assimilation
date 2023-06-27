        !COMPILER-GENERATED INTERFACE MODULE: Sun Jun 27 10:28:37 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RUN_PREDICTION__genmod
          INTERFACE 
            SUBROUTINE RUN_PREDICTION(N,N_E,ND,NT,M_UC,A)
              INTEGER(KIND=4), INTENT(IN) :: NT
              INTEGER(KIND=4), INTENT(IN) :: ND
              INTEGER(KIND=4), INTENT(IN) :: N_E
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=8) :: M_UC(N,N_E)
              INTEGER(KIND=4) :: A
            END SUBROUTINE RUN_PREDICTION
          END INTERFACE 
        END MODULE RUN_PREDICTION__genmod
