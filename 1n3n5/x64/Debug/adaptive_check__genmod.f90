        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:19 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ADAPTIVE_CHECK__genmod
          INTERFACE 
            SUBROUTINE ADAPTIVE_CHECK(N_D,N_E,CDD,S_D,DF,D_UC,ALPHA_I,  &
     &COND)
              INTEGER(KIND=4) :: N_E
              INTEGER(KIND=4) :: N_D
              REAL(KIND=8) :: CDD(N_D,N_D)
              REAL(KIND=8) :: S_D(N_D)
              REAL(KIND=8) :: DF(N_D,N_E)
              REAL(KIND=8) :: D_UC(N_D,N_E)
              REAL(KIND=8) :: ALPHA_I
              LOGICAL(KIND=4) :: COND
            END SUBROUTINE ADAPTIVE_CHECK
          END INTERFACE 
        END MODULE ADAPTIVE_CHECK__genmod
