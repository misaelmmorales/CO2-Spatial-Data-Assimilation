        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:11 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SVD_TRUNCATED_U__genmod
          INTERFACE 
            SUBROUTINE SVD_TRUNCATED_U(M,N,A,UN,SN,V)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=8) :: A(M,N)
              REAL(KIND=8) :: UN(M,N)
              REAL(KIND=8) :: SN(N)
              REAL(KIND=8) :: V(N,N)
            END SUBROUTINE SVD_TRUNCATED_U
          END INTERFACE 
        END MODULE SVD_TRUNCATED_U__genmod
