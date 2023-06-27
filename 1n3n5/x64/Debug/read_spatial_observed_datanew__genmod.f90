        !COMPILER-GENERATED INTERFACE MODULE: Sat Jan 16 09:34:52 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE READ_SPATIAL_OBSERVED_DATANEW__genmod
          INTERFACE 
            SUBROUTINE READ_SPATIAL_OBSERVED_DATANEW(N,NS,DOBS,FNAME1,  &
     &FNAME2,FNAME3)
              INTEGER(KIND=4), INTENT(IN) :: NS
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=8), INTENT(INOUT) :: DOBS(NS,N+1)
              CHARACTER(LEN=400) :: FNAME1
              CHARACTER(LEN=400) :: FNAME2
              CHARACTER(LEN=400) :: FNAME3
            END SUBROUTINE READ_SPATIAL_OBSERVED_DATANEW
          END INTERFACE 
        END MODULE READ_SPATIAL_OBSERVED_DATANEW__genmod
