        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:10 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE WRITE_PARAM__genmod
          INTERFACE 
            SUBROUTINE WRITE_PARAM(N,N_E,M,A)
              INTEGER(KIND=4), INTENT(IN) :: N_E
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=8) :: M(N,N_E)
              INTEGER(KIND=4) :: A
            END SUBROUTINE WRITE_PARAM
          END INTERFACE 
        END MODULE WRITE_PARAM__genmod
