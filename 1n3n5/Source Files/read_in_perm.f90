subroutine read_in_perm(filename)
    USE global_variables
    implicit none
    CHARACTER*400                               :: filename,fname
    CHARACTER*40                                :: chline, temp1, temp2
    integer                                     :: ierr,i,j,k,l,m,n
    open(unit=1, file = trim(filename), status='old', iostat = ierr)
    if(ierr/=0) then
        write(*,*) 'ERROR: Unable to open input file:' , filename
        stop
    endif
    do while (.true.)
        read(1,*,end=981) chline
        if(trim(chline)=='PERM_index') then
            read(1,*) N_perm
            allocate(perm_index(N_perm),d_perm(N_perm),s_d_perm(N_perm))
            read(1,*) (perm_index(i),i=1,N_perm)
            read(1,*) temp1
            read(1,*) (d_perm(i),i=1,N_perm)
            read(1,*) temp2
            read(1,*) (s_d_perm(i),i=1,N_perm)
        endif
    enddo !
981     close(1)
    end subroutine