        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:11 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SUBSPACE_ADAPTIVE__genmod
          INTERFACE 
            SUBROUTINE SUBSPACE_ADAPTIVE(DDF,M,N,S_D,ALPHA_I,T,P_MAX)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=8) :: DDF(M,N)
              REAL(KIND=8) :: S_D(M)
              REAL(KIND=8) :: ALPHA_I
              REAL(KIND=8) :: T
              INTEGER(KIND=4) :: P_MAX
            END SUBROUTINE SUBSPACE_ADAPTIVE
          END INTERFACE 
        END MODULE SUBSPACE_ADAPTIVE__genmod
