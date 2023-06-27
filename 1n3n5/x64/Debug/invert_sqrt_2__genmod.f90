        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:11 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INVERT_SQRT_2__genmod
          INTERFACE 
            SUBROUTINE INVERT_SQRT_2(CDD,TR,M,N,ALPHA_I,S_D,P_MAX)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=8) :: CDD(M,M)
              REAL(KIND=8) :: TR(N,N)
              REAL(KIND=8) :: ALPHA_I
              REAL(KIND=8) :: S_D(M)
              INTEGER(KIND=4) :: P_MAX
            END SUBROUTINE INVERT_SQRT_2
          END INTERFACE 
        END MODULE INVERT_SQRT_2__genmod
