program ObsGen
    CHARACTER*400                                   :: FILENAME1, FILENAME2
    INTEGER                             :: N_mesh      ! number of observed data
    INTEGER                             :: NS     ! number of time steps
    REAL*8, dimension(:,:), allocatable          :: maps
    REAL*8, dimension(:), allocatable          :: Sat_map
    CHARACTER*80                                    :: A1
    INTEGER                                         :: i,k,l,f,N2,j,nskip
    LOGICAL                                         :: FEXIST
    double precision                                :: tmp,time
    N_mesh = 2601
    Allocate(Sat_map(N_mesh), maps(N_mesh,6))
    nskip = 26011
    FILENAME1='run.00013_sca_node.dat'
    INQUIRE(FILE=FILENAME1, EXIST=FEXIST)
        IF (FEXIST) THEN
            OPEN(1, file=FILENAME1, status='old')    
            do i=1,nskip
                read(1,*) A1
            end do
            do j=1,N_mesh
                read(1,*) (maps(j,i),i=1,6)
            end do
            close(1)
        ELSE
            print *, 'File not found: *.his'
        END IF
    
        !Sat_map=maps(:,5)  ! 5 fore saturation
        Sat_map=maps(:,2)  ! 2 for pressure
        
        !FILENAME2='Sat_5yr.dat'
        FILENAME2='Pres_1yr.dat'
        open(unit = 33, file = FILENAME2, action = "write")
        do j=1,N_mesh
            write(33, "(F20.10)") Sat_map(j)
        end do
        close(unit = 33)
end program ObsGen