        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 22 11:39:19 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ES_ADAP_MINE__genmod
          INTERFACE 
            SUBROUTINE ES_ADAP_MINE(N_E,N_D,NP,D_OBS,DF,M_UC,S_D,ALPHA_I&
     &,SVD_IMP,DUMY,LOCAL)
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: N_D
              INTEGER(KIND=4) :: N_E
              REAL(KIND=8) :: D_OBS(N_D,1)
              REAL(KIND=8) :: DF(N_D,N_E)
              REAL(KIND=8) :: M_UC(NP,N_E)
              REAL(KIND=8) :: S_D(N_D)
              REAL(KIND=8), INTENT(IN) :: ALPHA_I
              REAL(KIND=8), INTENT(IN) :: SVD_IMP
              INTEGER(KIND=4) :: DUMY
              LOGICAL(KIND=4) :: LOCAL
            END SUBROUTINE ES_ADAP_MINE
          END INTERFACE 
        END MODULE ES_ADAP_MINE__genmod
