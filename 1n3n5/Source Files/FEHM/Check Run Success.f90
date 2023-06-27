
SUBROUTINE Success_Fehm(info)
    USE global_variables, ONLY: generic_dir_name, casename
    ! In the case of RUN with RESTRAT 2, NR=2
    IMPLICIT NONE
    CHARACTER*256                                   :: FILENAME,tempchar
    integer                                         :: info,i,find_string
    !FILENAME=trim(generic_dir_name) // '\'//trim(casename)//'.ECLEND' 
    FILENAME=trim(generic_dir_name) // '\'//'run.out' 
    OPEN(1, file=FILENAME, status='old')
    do
        read(1,'(A)') tempchar
        find_string=index(tempchar, 'total code time(timesteps) =')
        if (find_string .NE. 0) then
            info=0
            exit
        endif
    end do
    !read(1,*) tempchar,info
    close(1)
end subroutine
    