
SUBROUTINE Success_Ecl(info)
    USE global_variables, ONLY: generic_dir_name, casename
    ! In the case of RUN with RESTRAT 2, NR=2
    IMPLICIT NONE
    CHARACTER*256                                   :: FILENAME,tempchar
    integer                                         :: info,i
    FILENAME=trim(generic_dir_name) // '\'//trim(casename)//'.ECLEND'    
    OPEN(1, file=FILENAME, status='old')
    do i=1,4
        read(1,*) tempchar
    end do
    read(1,*) tempchar,info
    close(1)
end subroutine
    