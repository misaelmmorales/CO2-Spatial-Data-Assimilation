!    !  ESMDA.f90 
!    !
!    !  FUNCTIONS:
!    !  ESMDA - Entry point of console application.
!    !
!
!    !****************************************************************************
!    !
!    !  PROGRAM: ESMDA
!    !
!    !  PURPOSE:  Entry point for the console application.
!    !
!    !****************************************************************************
!
!    program ESMDA
!    USE global_variables, ONLY: cwd
!    implicit none
!
!    ! Variables
!    real,external                                      :: gasdev
!    integer, parameter                                 :: N_d = 1 + 2              ! number of observed data + time
!    integer, parameter                                 :: N_de= 3                  ! number of observed data + time
!    integer, parameter                                 :: Nd  = N_d - 2            ! number of observed data
!    integer, parameter                                 :: Nt = 25                  ! number of total time steps
!    integer, parameter                                 :: Nta = 12                 ! number of assimilation time steps
!    integer, parameter                                 :: Np = 1                   ! number of parameters per grid block
!    integer, parameter                                 :: N_a = 10                 ! number of data assimilation steps
!    integer, parameter                                 :: N_e = 100                ! number of ensemble members
!    integer, parameter                                 :: Nx = 31                  ! number of gridblocks in x direction
!    integer, parameter                                 :: Ny = 1                   ! number of gridblocks in y direction
!    integer, parameter                                 :: Nz = 1                   ! number of gridblocks in z direction
!    integer, parameter                                 :: N  =Nx*Ny*Nz             ! number of gridblocks
!
!    !double precision, dimension(Nt,N_d+N_de)           :: d_raw                    ! raw observed data
!    double precision, dimension(Nta*Nd,1)              :: d_obs                    ! observed data
!    !double precision, dimension(Nt*N_de,N_e+1)         :: d_mon                    ! monitored data
!    !double precision, dimension(Nt*Nd,1)               :: d_op                     ! observed data + predicted
!    double precision, dimension(Nta*Nd,N_e)            :: d_uc,df                  ! observed data
!    !double precision, dimension(Nt*Nd,N_e)             :: df_op                    ! observed data + predicted
!    double precision, dimension(Nd*Nta)                :: s_d                      ! observed data variance
!    double precision, dimension(Nd)                    :: pcnt_d                   ! observed data variance
!    double precision, dimension(Nz)                    :: Dz
!    double precision, dimension(Np*N,1)                :: mpr, Zm, mdum
!    double precision, dimension(Np*N,N_e+1)            :: m_uc,m_duc
!    integer, dimension(Nd,3)                           :: d_loc
!    
!    double precision                                   :: Dx, Dy, ax, ay, az, angle,c
!    double precision, allocatable, dimension(:,:)      :: L
!    integer                                            :: i,j,k,e,a, dumy=1642474681,info,kk
!    character*200                                       :: FILENAME, L1,L2,FMT
!    double precision                                   :: tmp,alpha_i
!    LOGICAL                                            :: FEXIST
!    double precision,dimension(N_a)                    :: alpha
!
!    double precision, dimension(Nta*Nd,1)              :: df_b,tmp_d
!    double precision, dimension(Np*N,1)                 :: mf_b,tmp_m
!    double precision, dimension(Nta*Nd,Nta*Nd)         :: C_D
!    double precision, dimension(Nta*Nd,Nta*Nd)         :: Cf_DD, CDD
!    double precision, dimension(Nta*Nd,Nta*Nd)         :: rho_dd
!    double precision, dimension(Np*N,Nta*Nd)            :: Cf_MD
!    double precision, dimension(Np*N,Nta*Nd)            :: rho_md             ! Gaspari-Cohn correlation matrix
!    integer, dimension(Nta*Nd)                         :: ipiv
!    double precision                                   :: err_lnk, err_phi, O_Nd
!    double precision,dimension(Np)                     :: s_par, m_par,err_par
!    double precision,dimension(Np,Np)                  :: r_par
!    logical                                            :: LOCAL,ensemble
!    character*400                                      :: command
!    integer                                            :: UNIT_MSG
!    ensemble = .true.
!    LOCAL = .FALSE.
!    UNIT_MSG  = 110
!    
!    
!    command = 'del '//trim(cwd)//'\outputs\*.txt'
!    call system(trim(command))
!    s_par = 1.0d0
!    m_par = 5.0d0
!    r_par = 1.0d0
!    open(UNIT_MSG,file='.\outputs\MSG.txt',status='unknown')
!    open(2,file='.\seed.dat',status='unknown')
!    READ (2,*,IOSTAT = info)  dumy ! info > 0 something is wrong, info <0 end of file
!    close(2)
!    open(2,file='.\outputs\seed.dat',status='unknown')
!    Write (2,*)  dumy
!    close(2)
!
!    Dz(1)    = 50.0d0
!    Dx       = 50.0d0
!    Dy       = 50.0d0
!    
!    ax = 10.0d0*Dx 
!    ay = 7.0d0*Dy
!    az = 5.0d0*sum(dz)/Nz
!    angle = 0.0d0
!    c = 1.0d0
!    
!    DATA (pcnt_d(i),i=1,Nd) /2.0d-2/
!
!    !DATA (alpha(i), i=1,N_a) /9.3333d0,7.0d0,4.0d0,2.0d0/
!    !DATA (alpha(i), i=1,N_a) /20.0d0,10.0d0,5.0d0,1.5385d0/
!    DATA (alpha(i), i=1,N_a) /57.017d0,35.0d0,25.0d0,20.0d0,18.0d0,15.0d0,12.0d0,8.0d0,5.0d0,3.0d0/
!
!
!    d_loc(1,1)= 16;  d_loc(1,2)= 1;  d_loc(1,3) = 1;
!
!    call ECL_BAT_GEN    
!    
!    if (LOCAL) then
!        rho_md =0.0d0
!        rho_dd =0.0d0
!        call GASCOHN_CMD(Nd,Nta,rho_md,d_loc,Nx,Ny,Nz,Dx,Dy,Dz,c*ax, c*ay, c*az, angle)
!        call GASCOHN_CDD(Nd,Nta,rho_dd,d_loc,Nx,Ny,Nz,Dx,Dy,Dz,c*ax, c*ay, c*az, angle)
!    end if
!
!    ! generating the random ensembles! =====================================================================================
!    if (ensemble) then
!        INQUIRE(FILE='.\ensemble\true.dat', EXIST=FEXIST)
!        if (FEXIST) then
!            open(1,file='.\ensemble\true.dat',status='old')
!            read(1,*) (m_uc(i,N_e+1),i=1,Np*N)
!            close(1)
!        else
!            print *, 'true.dat file not found'
!        end if
!        do i=1, N_e
!            if (i<10) then
!                write(L2,'(I1)') i
!                command = '.\ensemble\PERMI_00'//trim(L2)//'.inc'
!                
!            elseif (i<100) then
!                write(L2,'(I2)') i
!                command = '.\ensemble\PERMI_0'//trim(L2)//'.inc'
!            else
!                write(L2,'(I3)') i
!                command = '.\ensemble\PERMI_'//trim(L2)//'.inc'
!            end if
!            open(1,file=trim(command),status='old')
!            do
!                read(1,*,IOSTAT = info) L2
!                if (info<0) then
!                    print *, 'end of file: ' // command
!                    exit
!                elseif (trim(L2) == 'PERMI') then
!                    read(1,*) (mpr(j,1), j=1,N)
!                    exit
!                end if
!            end do
!            m_uc(1:N,i)= log(mpr(1:N,1))
!        end do
!            
!            
!            
!    else
!        allocate(L(Np*N,Np*N)); L = 0.0d0
!        call CML(L,Nx,Ny,Nz,Np,Dx,Dy,Dz,s_par,r_par,ax,ay,az,angle) ! L is the Choleskey decomposition of the covariance matrix
!
!        do i=1,N
!            do j=  1,Np
!                mpr((j-1)*N + i,1)   = m_par(j)
!            end do
!        end do
!        INQUIRE(FILE='.\true.dat', EXIST=FEXIST)
!        if (FEXIST) then
!            open(1,file='.\true.dat',status='old')
!            read(1,*) (m_uc(i,N_e+1),i=1,Np*N)
!            close(1)
!            do e=1, N_e
!                do i=1,Np*N
!                    Zm(i,1)=    gasdev(dumy)
!                end do
!                mdum=mpr+MATMUL(L,Zm)
!                m_uc(1:Np*N,e)=mdum(1:Np*N,1)
!            end do
!        else
!
!        do e=1, N_e + 1
!            do i=1,Np*N
!                Zm(i,1)=    gasdev(dumy)
!            end do
!            mdum=mpr+MATMUL(L,Zm)
!            m_uc(1:Np*N,e)=mdum(1:Np*N,1)
!        end do
!        open(1,file='.\true.dat',status='new')
!        do i=1,Np*N
!            write(1,'(E18.8)') m_uc(i,N_e+1)
!        end do
!        end if
!    end if
!
!
!
!    write(L2,'(I)') N_e
!    FMT = '('//trim(L2)//'E16.8)'
!
!    OPEN (1, FILE='.\outputs\par_init.txt', STATUS='UNKNOWN')
!    
!    err_par = 0.0d0
!    do k = 1,Np
!        do i = 1,N
!            write(1,FMT) (m_uc((k-1)*N+i,j),j=1,N_e)
!            do e = 1,N_e
!                err_par(k) = err_par(k) + (m_uc((k-1)*N+i  ,e)-m_uc((k-1)*N+i  ,N_e+1))**2 / N_e / N
!            end do
!        end do
!    end do
!    
!    write(*,'(A26,10E16.8)') "Initial ensemble MSE:     ", (err_par(k),k=1,Np)
!
!
!    write(L2,'(I)') N_e+2
!    FMT = '('//trim(L2)//'E16.8)'
!
!    ! Running the true model=====================================================================================================
!    
!    call run_ens(N,1,Nd,N_d,N_de,Nt,Nta,m_uc(1:N,N_e+1),d_obs,s_d,dumy,0)
!
!    
!    
!!================================================================================================================================    
!!================================================================================================================================       
!!================================================================================================================================       
!
!    open(2,file='.\seed.dat',status='unknown')
!    write(2,*) dumy
!    close(2)
!
!    ! constructing measurement error covariance
!    DO i=1,Nd
!        do j=1,Nta
!            k = (i-1)*Nta + j
!            C_d (k,k) = s_d(k)**2
!        end do
!    END DO
!    do a = 1, N_a
!        alpha_i = alpha(a)
!        !a = a+1
!        O_Nd = 0.0d0
!        ! RUN the ensemble from time zero  ======================================================================================
!        call run_ens(N,N_e,Nd,N_d,N_de,Nt,Nta,m_uc(1:N,1:N_e),df,s_d,dumy,a)
!        do e = 1, N_e !  ========================================================================================================  
!            tmp_d(1:,1) = df(1:,e) - d_obs(1:,1)
!            do i=1,Nd*Nta
!                O_Nd = O_Nd + tmp_d(i,1)**2 / s_d(i)**2 / (N_e * Nd * Nta)
!            end do
!        end do !=================================================================================================================
!        !alpha_i = 0.25d0 * O_Nd        
!        write(*,'(A26,E16.8)') "Objective function value: ", O_Nd
!        write(UNIT_MSG,'(10E16.8)') (err_par(k),k=1,Np),O_Nd                
!
!        !Calculation of MEAN of the MF and DF
!        do i=1,Nta*Nd
!            df_b(i,1) = sum(df(i,1:N_e)) / N_e
!        end do
!
!        do i=1,Np*N
!            mf_b(i,1) = sum(m_uc(i,1:N_e)) / N_e
!        end do
!
!        !    - Calculate Cf_MD , Cf_DD using matrix multiplication
!        Cf_DD = 0.0d0
!        Cf_MD = 0.0d0
!
!        do e=1,N_e
!            tmp_d(1:,1) = df(1:,e)  -df_b(1:,1)
!            tmp_m(1:,1) = m_uc(1:,e)-mf_b(1:,1)
!            Cf_DD = Cf_DD + MATMUL(tmp_d,transpose(tmp_d))
!            Cf_MD = Cf_MD + MATMUL(tmp_m,transpose(tmp_d))
!        end do
!
!        Cf_DD = Cf_DD / (N_e-1.0d0)
!        Cf_MD = Cf_MD / (N_e-1.0d0)
!        
!        !open(1,file='.\CMD.txt',status='unknown')
!        !do i=1,Np*N
!        !    write(1,'(12E20.8)') (Cf_MD(i,j),j=1,Nd*Nta)
!        !end do
!        !close(1)
!        
!        
!        if (LOCAL) then
!        ! Applying Gaspari-Cohn correlation on Cf_MD
!            do i=1,Np*N
!                do j=1,Nd*Nta
!                    Cf_MD(i,j) = Cf_MD(i,j)*ABS(RHO_md(i,j))
!                end do
!            end do
!            do i=1,Nd*Nta
!                do j=1,Nd*Nta
!                    Cf_DD(i,j) = Cf_DD(i,j)*ABS(RHO_dd(i,j))
!                end do
!            end do
!        end if
!        
!
!
!201     err_par = 0.0d0
!        do e=1,N_e   
!            !    - Solve for X vector
!            !      (Cf_DD + alpha_i*C_D)*X = (d_uc_e - df_j)
!            do i=1,Nd
!                do j=1,Nta
!                    k = (i-1)*Nta + j
!                    tmp = gasdev(dumy) ! tmp ~ N(0,1)
!                    d_uc(k,e) = d_obs(k,1) + sqrt(alpha_i)*s_d(k)*tmp
!                    if (d_uc(k,e) < 0.0d0) d_uc(k,e) = 0.0d0
!                end do
!            end do
!            info = 0
!            ipiv = 0
!            tmp_d(1:,1) = d_uc(1:,e) - df(1:,e)
!            CDD = Cf_DD + alpha_i*C_D
!            call dgesv( nd*nta, 1, CDD, nd*nta, ipiv,tmp_d(1:,1) ,nd*nta, info )
!            tmp_m(1:,1) = MATMUL(Cf_MD,tmp_d(1:,1))            
!            m_uc(1:,e)  = m_uc(1:,e) + tmp_m(1:,1)
!            do i =1, N
!                if (m_uc(i  ,e) < 0.0d0)then
!                    print *, i,'th parameter is negative, it is changed to 2.0'
!                    m_uc(i  ,e) = 2.0d0
!                end if
!                do k=1,Np
!                    err_par(k) = err_par(k) + (m_uc((k-1)*N+i  ,e)-m_uc((k-1)*N+i  ,N_e+1))**2 / N_e / N
!                end do
!            end do
!        end do
!        write(*,'(A16,I2,A8,2E16.8)') "Assimilation:   ", a,"  MSE:  ", (err_par(k),k=1,Np)
!        open(2,file='.\seed.dat',status='unknown')
!        write(2,*) dumy
!        close(2)
!        
!        if (a<=10) then 
!            write(L2,'(I1)') a-1
!        elseif (a<=100) then
!            write(L2,'(I2)') a-1
!        else
!            write(L2,'(I3)') a-1
!        endif
!        write(L1,'(I)') N_e
!        FMT = '('//trim(L1)//'E16.8)'
!        FILENAME='.\outputs\par_'//trim(L2)//'.txt'
!        OPEN (1, FILE=trim(FILENAME), STATUS='UNKNOWN')
!        do k= 1,Np
!            do i = 1,N
!                write(1,FMT) (m_uc((k-1)*N+i,j),j=1,N_e)
!            end do
!        end do
!        close(1)
!
!    end do
!    
!    
!    ! print parameters========================================================================================================
!    write(L1,'(I)') N_e
!    FMT = '('//trim(L1)//'E16.8)'
!
!    OPEN (1, FILE='.\outputs\par_end.txt', STATUS='UNKNOWN')
!
!    do k= 1,Np
!        do i = 1,N
!            write(1,FMT) (m_uc((k-1)*N+i,j),j=1,N_e)
!        end do
!    end do
!    close(1)
!    
!    call run_ens(N,N_e,Nd,N_d,N_de,Nt,Nta,m_uc(1:N,1:N_e),df,s_d,dumy,a+1)
!    
!    O_Nd = 0.0d0
!    do e = 1, N_e ! ========================================================================================================
!        tmp_d(1:,1) = df(1:,e) - d_obs(1:,1)
!        do i=1,Nd*Nta
!           O_Nd = O_Nd + tmp_d(i,1)**2 / s_d(i)**2 / (N_e * Nd * Nta)
!        end do
!    end do
!    
!    write(*,'(A26,E16.8)') "Objective function value: ", O_Nd
!    write(UNIT_MSG,'(10E16.8)') (err_par(k),k=1,Np),O_Nd
!
!end program ESMDA
!
