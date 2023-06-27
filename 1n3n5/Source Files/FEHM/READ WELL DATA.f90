SUBROUTINE READ_OBSERVED_DATANEW(N,NS,dobs,fname1,fname2)
    USE global_variables, ONLY: generic_dir_name, casename,cwd,N_location,Ndatatype
    IMPLICIT NONE
    CHARACTER*400                                   :: FILENAME1,FILENAME2,fname1,fname2
    INTEGER, INTENT(IN)                             :: N      ! number of observed data
    INTEGER, INTENT(IN)                             :: NS     ! number of time steps
    REAL*8, DIMENSION(NS,N+1),INTENT(INOUT)         :: DOBS
    CHARACTER*80                                    :: A1
    INTEGER                                         :: i,k,l,f,N2,j,nskip
    LOGICAL                                         :: FEXIST
    double precision                                :: tmp,time
    nskip = 5
    FILENAME1=trim(cwd) // '\'//trim(fname1)
    INQUIRE(FILE=FILENAME1, EXIST=FEXIST)
        IF (FEXIST) THEN
            OPEN(1, file=FILENAME1, status='old')    
            do i=1,nskip
                read(1,*) A1
            end do
            do j=1,NS
                read(1,*) (dobs(j,i),i=1,N_location+1)
            end do
            close(1)
            FILENAME1 = 'del '//trim(filename1)
            !call system(FILENAME1)
        ELSE
            print *, 'File not found: *.his'
        END IF
            
    FILENAME2=trim(cwd) // '\'//trim(fname2)
    INQUIRE(FILE=FILENAME2, EXIST=FEXIST)
        IF (FEXIST) THEN
            OPEN(2, file=FILENAME2, status='old')
            do i=1,nskip
                read(2,*) A1
            end do
            do j=1,NS
                read(2,*) time, (dobs(j,i),i=N_location+2,N_location*2+1)
            end do
            close(2)
            FILENAME2 = 'del '//trim(filename2)
            !call system(FILENAME2)
        ELSE
            print *, 'File not found: *.his'
        END IF
            
    END SUBROUTINE READ_OBSERVED_DATANEW

SUBROUTINE READ_Spatial_OBSERVED_DATANEW(N,NS,dobs,fname1,fname2,fname3)
    USE global_variables, ONLY: generic_dir_name, casename,cwd,N_location,Ndatatype,Nx,NY
    IMPLICIT NONE
    CHARACTER*400                                   :: FILENAME1,FILENAME2,FILENAME3,fname1,fname2,fname3
    INTEGER, INTENT(IN)                             :: N      ! number of observed data
    INTEGER, INTENT(IN)                             :: NS     ! number of time steps
    REAL*8, DIMENSION(NS,N+1),INTENT(INOUT)         :: DOBS
    CHARACTER*80                                    :: A1
    INTEGER                                         :: i,k,l,f,N2,j,nskip
    LOGICAL                                         :: FEXIST
    double precision                                :: tmp,time
    REAL*8, dimension(:,:), allocatable             :: maps1, maps2, maps3
    
    Allocate(maps1(Nx*NY,6),maps2(Nx*NY,6),maps3(Nx*NY,6))
    
    nskip = 26011
    FILENAME1=trim(cwd) // '\'//trim(fname1)
    INQUIRE(FILE=FILENAME1, EXIST=FEXIST)
        IF (FEXIST) THEN
            OPEN(1, file=FILENAME1, status='old')    
            do i=1,nskip
                read(1,*) A1
            end do
            do j=1,Nx*NY
                read(1,*) (maps1(j,i),i=1,6)
            end do
            close(1)
        ELSE
            print *, 'File not found: *.his'
        END IF
    
        dobs(1,2:Nx*NY+1)=maps1(:,5)
        dobs(1,1)=365.25
        
    FILENAME2=trim(cwd) // '\'//trim(fname2)
    INQUIRE(FILE=FILENAME2, EXIST=FEXIST)
        IF (FEXIST) THEN
            OPEN(2, file=FILENAME2, status='old')    
            do i=1,nskip
                read(2,*) A1
            end do
            do j=1,Nx*NY
                read(2,*) (maps2(j,i),i=1,6)
            end do
            close(2)
        ELSE
            print *, 'File not found: *.his'
        END IF
    
        dobs(2,2:Nx*NY+1)=maps2(:,5)
        dobs(2,1)=1095.75 
        
    FILENAME3=trim(cwd) // '\'//trim(fname3)
    INQUIRE(FILE=FILENAME3, EXIST=FEXIST)
        IF (FEXIST) THEN
            OPEN(3, file=FILENAME3, status='old')    
            do i=1,nskip
                read(3,*) A1
            end do
            do j=1,Nx*NY
                read(3,*) (maps3(j,i),i=1,6)
            end do
            close(3)
        ELSE
            print *, 'File not found: *.his'
        END IF
    
        dobs(3,2:Nx*NY+1)=maps3(:,5)
        dobs(3,1)=1826.25
    
        !IF (FEXIST) THEN
        !    OPEN(1, file=FILENAME1, status='old')    
        !    do i=1,nskip
        !        read(1,*) A1
        !    end do
        !    do j=1,NS
        !        read(1,*) (dobs(j,i),i=1,N_location+1)
        !    end do
        !    close(1)
        !    FILENAME1 = 'del '//trim(filename1)
        !    !call system(FILENAME1)
        !ELSE
        !    print *, 'File not found: *.his'
        !END IF
            
    END SUBROUTINE READ_Spatial_OBSERVED_DATANEW

    subroutine extract_obs_mon_data(d_raw,df)
        USE global_variables, ONLY: Nd,Nt,Nta,obs_flag,assim_tsteps,Ndata,orig_loc, obs_loc
        implicit none
        double precision, dimension(Ndata,1)     :: df,df_tmp   ! ensemble response
        double precision, dimension(Nt,Nd+1) :: d_raw                    ! 
        integer                                   :: i,j,k,nta_i
        nta_i = 1
        k =0
        do i = 1,Nt
            if (d_raw(i,1)==assim_tsteps(nta_i)) then
                
                do j = 1,Nd
                    if (obs_flag(j,nta_i)==1) then
                        k = k +1
                        df_tmp(k,1) = d_raw(i,j+1)
                    end if
                end do       
                nta_i = nta_i + 1
            end if
        end do
        do k = 1,Ndata
            df(k,1) = df_tmp(orig_loc(k,1),1)
        end do
       
    end subroutine extract_obs_mon_data
    
    SUBROUTINE READ_OBSERVED_DATA(N,NS,dobs)
    USE global_variables, ONLY: generic_dir_name, casename
    ! In the case of RUN with RESTRAT 2, NR=2
    IMPLICIT NONE
    CHARACTER*256                                   :: FILENAME
    CHARACTER*256                                   :: FROUT
    INTEGER, INTENT(IN)                             :: N      ! number of observed data
    INTEGER, INTENT(IN)                             :: NS     ! number of time steps
    INTEGER                                         :: NR     ! In the case of RUN with RESTRAT 2, NR=2
    REAL*8, DIMENSION(NS,N),INTENT(INOUT)           :: DOBS
    REAL*8,  DIMENSION(N)                           :: TEMP
    CHARACTER*80                                    :: A1,NUM
    INTEGER                                         :: i,k,l,f,reason
    LOGICAL                                         :: FEXIST, NO_MINISTEP
    
    NO_MINISTEP = .TRUE.
    
    
    NR = 1
    FROUT=trim(generic_dir_name) // '\'//trim(casename)//'.A000'
    k=0
    DO l = NR,NS
        IF (l<10) THEN
            Write(NUM,'(I1)') l
            FILENAME = trim(FROUT) // trim(NUM)
        ELSEIF (l<100) THEN
            FROUT=trim(generic_dir_name) // '\'//trim(casename)//'.A00'
            Write(NUM,'(I2)') l
            FILENAME = trim(FROUT) // trim(NUM)
        ELSE
            FROUT=trim(generic_dir_name) // '\'//trim(casename)//'.A0'
            Write(NUM,'(I3)') l
            FILENAME = trim(FROUT) // trim(NUM)
        END IF

        INQUIRE(FILE=FILENAME, EXIST=FEXIST)
        IF (FEXIST) THEN
            OPEN(1, file=FILENAME, status='old')
            DO WHILE (.not. eof(1))
                READ(1,*) A1
                IF (trim(A1)=='PARAMS') THEN
                    READ(1,*) (TEMP(i),i=1,N)
                    READ (1,*,IOSTAT = reason)  A1 ! reason > 0 something is wrong, reason <0 end of file
                    IF (reason == 0) BACKSPACE(1)                    
                    IF (.NOT.(TRIM(A1) == 'MINISTEP' .AND. NO_MINISTEP)) THEN
                        k= k+1
                        DOBS(k,1:N) = TEMP(1:N)
                    END IF
                END IF
            END DO
            close(1)
            FILENAME = 'del '//trim(filename)
            call system(FILENAME)
        ELSE
            print *, 'File not found: *.'// FILENAME(len_trim(FILENAME)-3:len_trim(FILENAME))
        END IF
    END DO
END SUBROUTINE READ_OBSERVED_DATA