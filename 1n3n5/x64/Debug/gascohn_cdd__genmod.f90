        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:15 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GASCOHN_CDD__genmod
          INTERFACE 
            SUBROUTINE GASCOHN_CDD(ND,NT,RHO,D_LOC,AX,AY,AZ,ANGLE)
              INTEGER(KIND=4), INTENT(IN) :: NT
              INTEGER(KIND=4), INTENT(IN) :: ND
              REAL(KIND=8), INTENT(OUT) :: RHO(ND*NT,ND*NT)
              REAL(KIND=8), INTENT(IN) :: D_LOC(ND,3)
              REAL(KIND=8), INTENT(IN) :: AX
              REAL(KIND=8), INTENT(IN) :: AY
              REAL(KIND=8), INTENT(IN) :: AZ
              REAL(KIND=8), INTENT(IN) :: ANGLE
            END SUBROUTINE GASCOHN_CDD
          END INTERFACE 
        END MODULE GASCOHN_CDD__genmod
