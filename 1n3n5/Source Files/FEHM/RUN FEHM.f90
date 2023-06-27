subroutine RUN_FEHM(e)
    USE global_variables, ONLY: NETWORKRUN
    implicit none
    CHARACTER*256                           :: NETWORK
    CHARACTER*256                           :: command,batfile
    Integer                                 :: e
    CHARACTER*20                                :: L1
    
    NETWORK    = 'enslic'    
    
    if (e<10) then
        write(L1,'(I1)') e
    elseif (e<100) then
        write(L1,'(I2)') e
    else
        write(L1,'(I3)') e
    end if
    batfile = 'Fehm_batch\'//'RunFehm_'//trim(L1)//'.bat'
    command = trim(batfile) // ' >nul'

    !   runs the fehm bat
    !   -----------------
    IF (NETWORKRUN.EQ.1) THEN          
        CALL ping(NETWORK)
    ENDIF
    CALL system (TRIM(command))
end subroutine