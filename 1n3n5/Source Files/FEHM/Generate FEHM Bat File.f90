    subroutine FEHM_BAT_GEN()
    use global_variables, ONLY: N_e, generic_dir_name, fehm_ver, casename, fehmNname, fehm_exe, cwd
    implicit none
    INTEGER                                     :: unt_bat
    INTEGER                                     :: CERR,e
    INTEGER                                     :: irc
    LOGICAL                                     :: FERR
    CHARACTER*256                               :: fehm_bat, fehm_ver_new, fehm_exe_new, generic_dir_name_new
    CHARACTER*20                                :: L1,L2

    do e=1, N_e 
        if (e<10) then
            write(L1,'(I1)') e
        elseif (e<100) then
            write(L1,'(I2)') e
        else
            write(L1,'(I3)') e
        end if
        
        if (e<10) then
            write(L2,'(I1)') e
            L2  = '000'//trim(L2)
        elseif (e<100) then
            write(L2,'(I2)') e
            L2  = '00'//trim(L2)
        elseif (e<1000) then
            write(L2,'(I3)') e
            L2  = '0'//trim(L2)
        else
            write(L2,'(I4)') e
        end if
        
        unt_bat = e
        fehm_bat = 'Fehm_batch\'//'RunFehm_'//trim(L1)//'.bat'
        fehm_ver_new = trim(fehm_ver)//'_'//trim(L1)
        fehm_exe_new = trim(cwd)//'\fehm_exe\'//trim(fehm_ver)//'_'//trim(L1)//'.exe'
        generic_dir_name_new = trim(generic_dir_name)//'\SimFold\'//'Ensemble_'//trim(L2)
        
        CERR     = 0
        irc      = 0
        FERR     = .FALSE.
        OPEN(unit=unt_bat,file=fehm_bat ,status='unknown',iostat=irc)
        IF (irc.NE.0) THEN
            CERR = CERR + 1
            FERR = .TRUE.
            WRITE(*,'(A)') '       : '
            WRITE(*,'(A)') '-------:------------------------------------------------------------------'
            WRITE(*,'(A)') 'ERROR  : UNABLE TO OPEN FILE ' //TRIM(fehm_bat) // '!'
            WRITE(*,'(A)') '-------:------------------------------------------------------------------'
            WRITE(*,'(A)') '       : '
            GOTO 001
        END IF
        
        WRITE(unt_bat, 2000) generic_dir_name_new(1:len_trim(generic_dir_name_new)),&
                  fehm_ver_new (1:len_trim(fehm_ver_new )),&
                  fehm_ver_new (1:len_trim(fehm_ver_new )),&
                  casename(1:len_trim(casename)),&
                  fehm_exe_new (1:len_trim(fehm_exe_new)),&
                  fehmNname(1:len_trim(fehmNname))//'.files',&
                  cwd(1:len_trim(cwd))
        CLOSE(unt_bat)    
    End do
001 IF (FERR) THEN
        WRITE(*,*) CERR
        PAUSE '       : PRESS KEY TO FINISH.'
        STOP
    ENDIF
    
2000 FORMAT( &
     'cd ',A                                                         ,/&
     '@echo off'                                                     ,/&
     'set FEHMVER=',A                                                 ,/&
     'echo -----------------------------------------------------'    ,/&
     'echo  FEHM version ', A,' dataset ',A                          ,/&
     'echo -----------------------------------------------------'    ,/&
      A,' ',A                                                        ,/&
     'cd ',A)
    end subroutine FEHM_BAT_GEN