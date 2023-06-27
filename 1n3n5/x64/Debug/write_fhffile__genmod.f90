        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:10 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE WRITE_FHFFILE__genmod
          INTERFACE 
            SUBROUTINE WRITE_FHFFILE(ND,NT,D,E,FHF_ID)
              INTEGER(KIND=4) :: NT
              INTEGER(KIND=4) :: ND
              REAL(KIND=8) :: D(NT,ND+1)
              INTEGER(KIND=4) :: E
              INTEGER(KIND=4) :: FHF_ID
            END SUBROUTINE WRITE_FHFFILE
          END INTERFACE 
        END MODULE WRITE_FHFFILE__genmod
