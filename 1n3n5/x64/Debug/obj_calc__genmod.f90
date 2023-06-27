        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:05 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OBJ_CALC__genmod
          INTERFACE 
            SUBROUTINE OBJ_CALC(N_D,N_E,D_OBS,DF,S_D,NOFUNC,ENNOFUNC)
              INTEGER(KIND=4) :: N_E
              INTEGER(KIND=4) :: N_D
              REAL(KIND=8) :: D_OBS(N_D,1)
              REAL(KIND=8) :: DF(N_D,N_E)
              REAL(KIND=8) :: S_D(N_D)
              REAL(KIND=8) :: NOFUNC
              REAL(KIND=8) :: ENNOFUNC(N_E,1)
            END SUBROUTINE OBJ_CALC
          END INTERFACE 
        END MODULE OBJ_CALC__genmod
