        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:11 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INVERT_SVD_DUC__genmod
          INTERFACE 
            SUBROUTINE INVERT_SVD_DUC(M,N,P_MAX,ALPHA_I,CDD,S_D)
              INTEGER(KIND=4) :: M
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: P_MAX
              REAL(KIND=8) :: ALPHA_I
              REAL(KIND=8) :: CDD(M,M)
              REAL(KIND=8) :: S_D(M)
            END SUBROUTINE INVERT_SVD_DUC
          END INTERFACE 
        END MODULE INVERT_SVD_DUC__genmod
