subroutine RUN_ECL()
    USE global_variables, ONLY: NETWORKRUN
    implicit none
    CHARACTER*256                           :: NETWORK
    CHARACTER*256                           :: command
    
    NETWORK    = 'enslic'
    command = 'RunEclipse.bat' // ' >nul'

    !   runs the ecl bat
    !   -----------------
    IF (NETWORKRUN.EQ.1) THEN          
        CALL ping(NETWORK)
    ENDIF
    CALL system (TRIM(command))
end subroutine