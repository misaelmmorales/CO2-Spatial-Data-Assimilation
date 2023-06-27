        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:11 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INVERT_SQRT__genmod
          INTERFACE 
            SUBROUTINE INVERT_SQRT(IMAT,TR,M,N,P,ALPHA_I)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=8) :: IMAT(N,M)
              REAL(KIND=8) :: TR(N,N)
              INTEGER(KIND=4) :: P
              REAL(KIND=8) :: ALPHA_I
            END SUBROUTINE INVERT_SQRT
          END INTERFACE 
        END MODULE INVERT_SQRT__genmod
