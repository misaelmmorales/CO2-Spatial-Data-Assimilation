        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:11 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SVD__genmod
          INTERFACE 
            SUBROUTINE SVD(M,N,A,U,SM,VM)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=8) :: A(M,N)
              REAL(KIND=8) :: U(M,M)
              REAL(KIND=8) :: SM(M)
              REAL(KIND=8) :: VM(M,N)
            END SUBROUTINE SVD
          END INTERFACE 
        END MODULE SVD__genmod
