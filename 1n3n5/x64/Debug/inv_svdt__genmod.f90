        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:11 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INV_SVDT__genmod
          INTERFACE 
            SUBROUTINE INV_SVDT(A,N,T,P_MAX)
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=8) :: A(N,N)
              REAL(KIND=8) :: T
              INTEGER(KIND=4) :: P_MAX
            END SUBROUTINE INV_SVDT
          END INTERFACE 
        END MODULE INV_SVDT__genmod
