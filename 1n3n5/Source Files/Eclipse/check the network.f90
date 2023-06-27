SUBROUTINE ping(net)
!-----------------------------------------------------------------------
! Subrouting to check if server is available before calling simulator
!-----------------------------------------------------------------------
! Originally provided by Emil Nurmandov
! Adapted by Diego Oliveira
!-----------------------------------------------------------------------
    IMPLICIT NONE
!-----------------------------------------------------------------------
! IN/OUT Variables
    CHARACTER ::  net*256
!-----------------------------------------------------------------------
! General Variables
    INTEGER   ::  stat,i=0
    CHARACTER ::  chline*200
    CHARACTER ::  syscommand*200
!-----------------------------------------------------------------------
    OPEN(335,file='network.dat',access='append')
    syscommand = 'ping ' // trim(net) // ' > network.out'
10  CALL system(syscommand)
    OPEN(334,file='network.out',iostat=stat)
    
    read(334,'(A80)',iostat=stat)chline
    
    IF(chline(:32)=='Ping request could not find host')then
        i=i+1
        WRITE(335,*),'ATTEMPT     ',i
        CLOSE(334)
        CALL system('del network.out >nul')
        GOTO 10
    ELSE
        CLOSE(334)
        GOTO 20
    ENDIF
    
    !#####CHECK IF THERE'S REPLY OR NOT#######

20  OPEN(334,file='network.out',iostat=stat)
    DO
        READ(334,'(A80)',iostat=stat)chline
        READ(334,'(A80)',iostat=stat)chline
        READ(334,'(A80)',iostat=stat)chline
        READ(334,'(A80)',iostat=stat)chline
        IF(chline(:10)=='Reply from')then
            CLOSE(334)
            CALL system('del network.out >nul')
            EXIT
        ELSE
            i=i+1
            WRITE(335,*),'ATTEMPT     ',i 
            CLOSE(334)
            CALL system('del network.out >nul')
            GOTO 10
        ENDIF
     
     ENDDO
    CLOSE(335)

    WRITE(*,'(A)') '       : NETWORK OK!                                                      '
!-----------------------------------------------------------------------
END SUBROUTINE ping
!--------------------------------------------------------------------------------------------------
!  TU-OPS Project. (2014)
!  The University of Tulsa
!  Petroleum Reservoir Exploitation Projects - TUPREP.
!  Diego Felipe Oliveira, Ph.D.
!--------------------------------------------------------------------------------------------------
