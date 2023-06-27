        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:19 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CHECK_CONV__genmod
          INTERFACE 
            SUBROUTINE CHECK_CONV(N_D,N_E,S_D,D_OBS,DF,TAU,ETTA,NOLAST)
              INTEGER(KIND=4) :: N_E
              INTEGER(KIND=4) :: N_D
              REAL(KIND=8) :: S_D(N_D)
              REAL(KIND=8) :: D_OBS(N_D,1)
              REAL(KIND=8) :: DF(N_D,N_E)
              REAL(KIND=8) :: TAU
              REAL(KIND=8) :: ETTA
              LOGICAL(KIND=4) :: NOLAST
            END SUBROUTINE CHECK_CONV
          END INTERFACE 
        END MODULE CHECK_CONV__genmod
