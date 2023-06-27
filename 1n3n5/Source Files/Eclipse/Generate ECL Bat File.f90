    subroutine ECL_BAT_GEN()
    use global_variables, ONLY: ecl_bat, generic_dir_name, ecl_ver, casename, ecl_exe, cwd
    implicit none
    INTEGER                                     :: unt_bat
    INTEGER                                     :: CERR
    INTEGER                                     :: irc
    LOGICAL                                     :: FERR

    
    unt_bat = 1
    CERR     = 0
    irc      = 0
    FERR     = .FALSE.
    OPEN(unit=unt_bat,file=ecl_bat ,status='unknown',iostat=irc)
    IF (irc.NE.0) THEN
        CERR = CERR + 1
        FERR = .TRUE.
        WRITE(*,'(A)') '       : '
        WRITE(*,'(A)') '-------:------------------------------------------------------------------'
        WRITE(*,'(A)') 'ERROR  : UNABLE TO OPEN FILE ' //TRIM(ecl_bat) // '!'
        WRITE(*,'(A)') '-------:------------------------------------------------------------------'
        WRITE(*,'(A)') '       : '
        GOTO 001
    END IF
    WRITE(unt_bat, 2000) generic_dir_name(1:len_trim(generic_dir_name)),&
                  ecl_ver (1:len_trim(ecl_ver )),&
                  ecl_ver (1:len_trim(ecl_ver )),&
                  ecl_ver (1:len_trim(ecl_ver )),&
                  casename(1:len_trim(casename)),&
                  ecl_exe (1:len_trim(ecl_exe)),&
                  casename(1:len_trim(casename)),&
                  cwd(1:len_trim(cwd))
    CLOSE(unt_bat)
001 IF (FERR) THEN
        WRITE(*,*) CERR
        PAUSE '       : PRESS KEY TO FINISH.'
        STOP
    ENDIF
    
2000 FORMAT( &
     'cd ',A                                                         ,/&
     '@echo off'                                                     ,/&
     'call c:\ecl\home\$eclrc.bat'                                   ,/&
     'set ECLVER=',A                                                 ,/&
     'set TEMP_PATH=%PATH%'                                          ,/&
     'set PATH=c:\ecl\',A,'\lib\pc;%PATH%'                           ,/&
     'echo -----------------------------------------------------'    ,/&
     'echo  30/1/2012 22:30:45'                                      ,/&
     'echo  ECLIPSE version ', A,' dataset ',A                       ,/&
     'echo -----------------------------------------------------'    ,/&
      A,' ',A                                                        ,/&
     'set PATH=%TEMP_PATH%'                                          ,/&
     'set TEMP_PATH='                                                ,/&
     'cd ',A)
    end subroutine ECL_BAT_GEN