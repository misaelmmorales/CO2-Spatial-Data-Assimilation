        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:08 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CHOLDC__genmod
          INTERFACE 
            SUBROUTINE CHOLDC(N,A,AA)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(0:N-1,0:N-1)
              REAL(KIND=8) :: AA(0:N-1,0:N-1)
            END SUBROUTINE CHOLDC
          END INTERFACE 
        END MODULE CHOLDC__genmod
