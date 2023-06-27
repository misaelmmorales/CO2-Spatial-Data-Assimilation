        !COMPILER-GENERATED INTERFACE MODULE: Sun Jun 27 10:28:37 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE READ_INITENS__genmod
          INTERFACE 
            SUBROUTINE READ_INITENS(NE,NP,M_UC,DIR_MODEL)
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: NE
              REAL(KIND=8) :: M_UC(NP,NE+1)
              CHARACTER(LEN=400) :: DIR_MODEL
            END SUBROUTINE READ_INITENS
          END INTERFACE 
        END MODULE READ_INITENS__genmod
