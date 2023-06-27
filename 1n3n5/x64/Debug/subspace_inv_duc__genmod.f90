        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:11 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SUBSPACE_INV_DUC__genmod
          INTERFACE 
            SUBROUTINE SUBSPACE_INV_DUC(DDF,M,N,S_D,CDD,ALPHA_I,T,P_MAX)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=8) :: DDF(M,N)
              REAL(KIND=8) :: S_D(M)
              REAL(KIND=8) :: CDD(M,M)
              REAL(KIND=8) :: ALPHA_I
              REAL(KIND=8) :: T
              INTEGER(KIND=4) :: P_MAX
            END SUBROUTINE SUBSPACE_INV_DUC
          END INTERFACE 
        END MODULE SUBSPACE_INV_DUC__genmod
