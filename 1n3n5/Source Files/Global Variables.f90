Module global_variables
    implicit none
    !CHARACTER*256                               :: fehm_bat          != 'RunFehm.bat'
    CHARACTER*256                               :: generic_dir_name != 'Models\3D-DualPoro'
    CHARACTER*256                               :: fehm_ver          != '2013.1'
    CHARACTER*256                               :: casename         != 'case01'
    CHARACTER*256                               :: fehmNname        !=fehmn.files
    CHARACTER*256                               :: modelname        != 'BRUGGE'
    CHARACTER*256                               :: fehm_exe         != 'C:\LocalAccount\Project\NRAP\ES-MDA-code_FEHM\fehm_exe\FEHM_V3.3.0w64.21Sep17.exe'
    CHARACTER*256                               :: cwd              != 'C:...\ES-MDA\ESMDA-frac'
    CHARACTER*256                               :: obs_file         != 'RunFehm.bat'
    INTEGER                                     :: NETWORKRUN       != 0
    INTEGER                                     :: N_d      ! number of observed data + time
    INTEGER                                     :: N_de     ! number of observed data + time
    INTEGER                                     :: Nd       ! number of observed data
    INTEGER                                     :: Nt       ! number of total time steps
    INTEGER                                     :: Nta      ! number of assimilation time steps
    INTEGER                                     :: N_perm   ! direct measurements
    INTEGER                                     :: Ndata,Ndata_new    ! total number of avilable data
    INTEGER                                     :: Np_g     ! number of parameters per gridblocks
    INTEGER                                     :: Np_gl    ! number of global parameters
    INTEGER                                     :: Np       ! total number of parameters = NACT*NP_G+Np_Gl
    INTEGER                                     :: N_e      ! number of ensemble members
    INTEGER                                     :: N_node
    logical                                     :: local,nolast, prediction_post, prediction_prior,cond_perm
    double precision,dimension(:,:),allocatable :: m_all,local_var
    integer                                     :: method  ! 0 - predetermined alpha, 1 - known # of assimilation steps, 2 - adaptive Duc Le, 3 - adaptive Iglesias, 4 - adaptive alpha = max singular value
    integer                                     :: Na,nact,N_location,Ndatatype,N_processor,DataAssim_type
    double precision, dimension(:), allocatable :: alpha
    double precision, dimension(:,:), allocatable :: SAT_VALS, PC_VALS
    integer, dimension(:), allocatable          :: NSAT_TAB
    integer                                     :: NTABLES
    double precision, dimension(:), allocatable :: XCOORD,YCOORD,ZCOORD
    integer, dimension(:), allocatable          :: ACTNUM,ACTLIST
    
    integer, dimension(:,:), allocatable        :: ACTINDX
    Integer                                     :: NX,NY,NZ,NG
    double precision                            :: Dx,DY,DZ, alpha_sum,max_alpha
    double precision, dimension(:), allocatable :: assim_tsteps
    double precision, dimension(:,:), allocatable                :: d_obs,d_tmp, d_obs_new,d_tmp_new                    ! vector of observed data
    double precision, dimension(:), allocatable                :: d_perm
    integer, dimension(:), allocatable                :: perm_index
    double precision, dimension(:), allocatable                  :: s_d,s_tmp,s_d_new,s_d_perm,time_vec                      ! vector of measurement errors
    integer, dimension(:,:), allocatable        :: d_loc,obs_flag,obs_loc,tmp_obsloc,orig_loc,loc_indx
    character*40, dimension(:,:),allocatable    :: par_name
    double precision, dimension(:,:), allocatable                :: par_limit, rho_md
    ! ONLY FOR ADAPTIVE METHODS
    double precision,allocatable,dimension(:,:)  :: ADAP_u,ADAP_v,ADAP_ui!s(m), u(m,m), v(m,n),ui(m,1)
    double precision,allocatable,dimension(:)    :: ADAP_s
    double precision                             :: rho, tau, svd_imp
    
contains
    subroutine read_data(filename)
        implicit none
        CHARACTER*400                               :: filename,fname
        CHARACTER*40                                :: chline
        integer                                     :: ierr,i,j,k,l,m,n
        open(unit=1, file = trim(filename), status='old', iostat = ierr)
        if(ierr/=0) then
            write(*,*) 'ERROR: Unable to open input file:' , filename
            stop
        endif
        svd_imp = 0.99d0
        do while (.true.)
           read(1,*,end=981) chline
           if(trim(chline)=='NOBSDATA') then
              read(1,*) Nd
              N_d = Nd + 2   ! time in day and year
              allocate(loc_indx(Nd+1,1))
           elseif (trim(chline)=='COND_PERM') then
               cond_perm = .true.
           elseif (trim(chline)=='NPERM') then
              read(1,*) N_perm
           elseif (trim(chline)=='NProcessor') then
              read(1,*) N_processor
           elseif (trim(chline)=='NLOC') then
              read(1,*) N_location
           elseif (trim(chline)=='DA_type') then
              read(1,*) DataAssim_type
           elseif (trim(chline)=='NDATATYPE') then
              read(1,*) Ndatatype
           elseif (trim(chline)=='NTOTSTEP') then
              read(1,*) Nt
           elseif (trim(chline)=='NASSIMSTEP') then 
               read(1,*) Nta
               allocate(assim_tsteps(Nta))
           elseif (trim(chline)=='NPARAMPERG') then
               read(1,*) Np_g, np_gl
               if (np_gl>0) then
                   allocate(par_name(2,Np_g+1),par_limit(2,Np_g+np_gl))
                   read(1,*) (par_name(1,i),i=1,Np_g+1)
                   read(1,*) (par_name(2,i),i=1,Np_g+1)
               else
                    allocate(par_name(2,Np_g),par_limit(2,Np_g))
                    read(1,*) (par_name(1,i),i=1,Np_g)
                    read(1,*) (par_name(2,i),i=1,Np_g)
               end if
           elseif (trim(chline)=='DIMENS') then
               read(1,*) Nx,NY,NZ
           elseif (trim(chline)=='SIZES') then
               read(1,*) Dx,DY,DZ
           elseif (trim(chline)=='N_NODE_RES_Z') then
               read(1,*) N_node
           elseif (trim(chline)=='NENSEM') then
               read(1,*) N_e
           elseif (trim(chline)=='FEHMEXE') then
               read(1,*) fehm_exe
           !elseif (trim(chline)=='FEHMBAT') then
           !    read(1,*) fehm_bat    
           elseif (trim(chline)=='FEHMVER') then
               read(1,*) fehm_ver
           elseif (trim(chline)=='MODELDIR') then
               read(1,*) generic_dir_name
           elseif (trim(chline)=='MODELNAME') then
               read(1,*) casename
           elseif (trim(chline)=='FEHMFILENAME') then
               read(1,*) fehmNname
           elseif (trim(chline)=='CURRENTDIR') then
               read(1,*) cwd
           elseif (trim(chline)=='NETWORKRUN') then
               read(1,*) NETWORKRUN
           elseif (trim(chline)=='LOCAL') then
               local = .true.
           elseif (trim(chline)=='PREDICTION_Post') then
               prediction_post = .true.
           elseif (trim(chline)=='PREDICTION_Prior') then
               prediction_prior = .true.
           elseif (trim(chline)=='SVDTR') then
               read(1,*) svd_imp
           elseif (trim(chline)=='METHOD') then
              read(1,*) method
              if (method==0) then
                  read(1,*) Na
                  allocate(alpha(Na))
                  read(1,*) (alpha(i),i=1,Na)
              elseif(method==1) then
                  read(1,*) Na,max_alpha
                  allocate(alpha(Na))
                  alpha = 0.0d0
              elseif(method==2) then ! apdative duc
                  read(1,*) rho,tau
              elseif(method==3) then ! apdative IGL
                  read(1,*) rho,tau
              elseif(method==4) then ! apdative mine
                  read(1,*) rho, max_alpha
                  tau = 1.0d0/rho
              end if
           endif
        enddo !
981     close(1)
        NG = NX*NY*NZ
        
        ALLOCATE(ACTNUM(NG))
        filename = trim(cwd)//'\'//trim(generic_dir_name) // '\ACTNUM.dat'
        open(unit = 101, file = trim(filename),status='old')
        READ(101,*) CHLINE
        read(101,*) (actnum(j),j=1,Ng)
        close(101)
        nact = sum(actnum)
        allocate(ACTLIST(nact),ACTINDX(nact,4))
        
        m=0
        do k=1,Nz
            do j = 1,Ny
                do i = 1,Nx
                    l = (k-1)*Nx*Ny + (j-1)*Nx + i
                    if (actnum(l) == 1) then
                        m = m + 1
                        ACTLIST(m) = l
                        ACTINDX(m,1) = i
                        ACTINDX(m,2) = j
                        ACTINDX(m,3) = k
                        ACTINDX(m,4) = l
                    end if
                end do
            end do
        end do
        np = nact*np_g + np_gl
        allocate(m_all(Np_g*NG + np_gl,N_e+1))
        allocate(obs_flag(nd,nta),d_tmp(nd*nta,1),s_tmp(nd*nta),tmp_obsloc(Nd*Nta,1))
        if (np_gl>0) then
            filename = trim(cwd)//'\ensemble\'//trim(modelname)//'\'//trim(par_name(1,np_g+1))//'.dat'
            open(unit=1, file = trim(filename), status='old')
            do while (.not. eof(1))
                read(1,*) chline
                if( trim(chline)==trim(par_name(2,np_g+1)) ) then
                     read(1,*) NTABLES,k
                     allocate(SAT_VALS(NTABLES,k), PC_VALS(NTABLES,k),NSAT_TAB(NTABLES))
                     SAT_VALS =0.0d0
                     PC_VALS  =0.0d0
                     do i = 1,NTABLES
                         read(1,*) NSAT_TAB(i)
                         do j=1, NSAT_TAB(i)
                             read(1,*) SAT_VALS(i,j),PC_VALS(i,j)
                         end do
                     end do
                     exit
                end if
            end do
            close(1)
        end if
            
        obs_flag = 0; d_tmp = 0.0d0; s_tmp = 0.0d0; assim_tsteps = 0.0d0
                   
        obs_file = '.\'//trim(modelname)//'_OBSERVED.dat'
        call read_obs_and_err(obs_file,nd,nta,assim_tsteps,obs_flag,d_tmp,s_tmp,Ndata,tmp_obsloc)
        allocate(s_d(Ndata), d_obs(Ndata,1),obs_loc(ndata,1),orig_loc(ndata,1),time_vec(ndata))
        
        k =0
        loc_indx(1,1) = 1
        do i = 1,Nd
            do j= 1,ndata
                if (tmp_obsloc(j,1) == i) then
                    k = k + 1
                    obs_loc(k,1) = i
                    s_d(k)     = s_tmp(j)
                    d_obs(k,1)   = d_tmp(j,1)
                    tmp_obsloc(j,1) = -1
                    orig_loc(k,1)     = j
                end if
            end do
            loc_indx(i+1,1) = k+1
        end do
        k=0
        do i=1,Nd
            do j=1,Nta
                if (obs_flag(i,j)==1) then
                    k = k+1
                    time_vec(k) = assim_tsteps(j)
                end if
            end do
        end do
        filename = 'del '//trim(cwd)//'\outputs_'//trim(modelname)//'\*.txt'
        call system(trim(filename))
        if (local) then
            allocate(rho_md(Nact,Nd),d_loc(nd,3),local_var(3,Nz))
            filename = '.\'//trim(modelname)//'_LOCAL.dat'
            open(1,file = trim(filename),status='old')
            do while (.not. eof(1))
                read(1,*) chline
                if(trim(chline)=='DATALOC') then
                    do i =1,nd
                        read(1,*) (d_loc(i,j),j=1,2)
                    end do
                elseif(trim(chline)=='PRIORVAR') then
                    do i =1,3
                        read(1,*) (local_var(i,j),j=1,Nz)
                    end do
                end if
            end do
            close(1)
            call LOCALIZATION_MATRIX(rho_md)
        end if
        
        deallocate(d_tmp,s_tmp,tmp_obsloc)
        filename = trim(cwd)//'\outputs_'//trim(modelname) // '\time_vec.txt'
        open(1,file = trim(filename),status='unknown')
        write(1,'(f8.2)') (time_vec(j),j=1,Ndata)
        close(1)
        filename = trim(cwd)//'\outputs_'//trim(modelname) // '\obs_indx.txt'
        open(1,file = trim(filename),status='unknown')
        write(1,'(I5)') (loc_indx(j,1),j=1,Nd+1)
        close(1)
        filename = trim(cwd)//'\outputs_'//trim(modelname) // '\obs_data.txt'
        open(1,file = trim(filename),status='unknown')
        write(1,'(f16.4)') (d_obs(j,1),j=1,Ndata)
        close(1)

        
        if (method==2 .or. method == 3) then
            m = Ndata
            n = N_e
            l = n
            if (m<n) l = m
            allocate(adap_s(l),adap_u(m,l),adap_v(l,n),adap_ui(m,1))
        end if
    end subroutine
    
    subroutine read_obs_and_err(fname,nd,nta,assim_tsteps,obs_flag,d_tmp,s_tmp,ndata,obs_loc)
        implicit none
        character*256                           :: fname,chline
        integer, intent(in)                     :: nd
        integer, intent(in)                     :: nta
        integer                                 :: ndata
        double precision, dimension(nta)        :: assim_tsteps
        double precision, dimension(nta*nd,1)   :: d_tmp
        double precision, dimension(nta*nd)     :: s_tmp
        integer, dimension(nd,nta)              :: obs_flag
        integer, dimension(nd*nta,1)            :: obs_loc
        integer                                 :: i,j,k
        double precision                        :: tmp,tmp1,tmp2
        
        OPEN (105, FILE=trim(fname), STATUS='OLD')
        i = 0
        Ndata = 0
        obs_loc = 0
        do while (.true.)
            read(105,*,end=982) chline,tmp
            if (trim(chline)=='TIME') then
                i = i + 1
                assim_tsteps(i) = tmp
                do j =1,nd
                    read(105,*) obs_flag(j,i),tmp1, tmp2
                    if (obs_flag(j,i) == 1) then
                        Ndata = ndata +1
                        d_tmp(ndata,1) = tmp1
                        s_tmp(ndata) = tmp2
                        obs_loc(ndata,1) = j
                    end if
                end do
            end if
        end do
982     close(105)
        
    end subroutine

    
    
end module global_variables