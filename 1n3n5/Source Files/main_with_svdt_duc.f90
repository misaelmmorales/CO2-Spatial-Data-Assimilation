!program main_esmda
!    USE global_variables, ONLY: esmda_dir
!    !USE module_hmoutputs
!    implicit none
!    real,external                                      :: gasdev
!    integer, parameter                                 :: N_d = 1 + 2              ! number of observed data + time
!    integer, parameter                                 :: N_de= 1                  ! number of monitored data
!    integer, parameter                                 :: Nd  = N_d - 2            ! number of observed data
!    integer, parameter                                 :: Nt = 80                  ! number of total time steps
!    integer, parameter                                 :: Nta = 50                 ! number of assimilation time steps   ! 150
!    integer, parameter                                 :: Np = 3                   ! number of parameters per grid block
!    integer, parameter                                 :: N_a = 10                 ! number of data assimilation steps
!    integer, parameter                                 :: N_e = 30                 ! number of ensemble members
!    integer, parameter                                 :: Nx = 35                  ! number of gridblocks in x direction
!    integer, parameter                                 :: Ny = 35                  ! number of gridblocks in y direction
!    integer, parameter                                 :: Nz = 1                   ! number of gridblocks in z direction
!    integer, parameter                                 :: N  =1                    ! number of gridblocks
!    
!    
!    double precision, dimension(Nta*Nd,1)              :: d_obs                    ! vector of observed data
!    double precision, dimension(Nta*Nd,N_e)            :: d_uc,df                  ! observed data for ensemble members
!    double precision, dimension(Nd*Nta)                :: s_d                      ! observed data variance
!    double precision, dimension(Nd)                    :: pcnt_d                   ! observed data variance
!    double precision, dimension(Nz)                    :: Dz
!    double precision, dimension(Np*N,1)                :: mpr, Zm, mdum
!    double precision, dimension(Np*N,N_e+1)            :: m_uc,m_duc
!    integer, dimension(Nd,3)                           :: d_loc                    ! location of observed data for localization
!    
!    double precision                                   :: Dx, Dy, ax, ay, az, angle,c
!    double precision, allocatable, dimension(:,:)      :: L
!    integer                                            :: i,j,k,e,a, dumy=1642474681,info,kk
!    character*400                                       :: FILENAME, L1,L2,FMT
!    double precision                                   :: tmp,alpha_i
!    LOGICAL                                            :: FEXIST
!    double precision,dimension(N_a)                    :: alpha
!    
!    double precision, dimension(Nta*Nd,1)              :: df_b,tmp_d
!    double precision, dimension(Np*N,1)                :: mf_b,tmp_m
!    double precision, dimension(Nta*Nd,Nta*Nd)         :: C_D
!    double precision, dimension(Nta*Nd,Nta*Nd)         :: Cf_DD, CDD
!    double precision, dimension(Nta*Nd,Nta*Nd)         :: rho_dd
!    double precision, dimension(Np*N,Nta*Nd)           :: Cf_MD,CMD
!    double precision, dimension(Np*N,Nta*Nd)           :: rho_md             ! Gaspari-Cohn correlation matrix
!    integer, dimension(Nta*Nd)                         :: ipiv
!    double precision                                   :: err_lnk, err_phi, O_Nd,rhs,lhs,rho
!    double precision,dimension(Np)                     :: s_par, m_par,err_par
!    double precision,dimension(Np,Np)                  :: r_par
!    logical                                            :: LOCAL,ensemble,nolast,method1
!    character*400                                      :: command, dir_model
!    integer                                            :: UNIT_MSG
!    
!    dir_model = esmda_dir
!    
!    ensemble = .true.
!    LOCAL    = .FALSE.
!    method1  = .TRUE.
!    UNIT_MSG = 110
!    
!    rho = 0.2d0 ! for method2
!    
!    command = 'del '//trim(dir_model)//'\outputs\*.txt'
!    call system(trim(command))
!    s_par = 1.0d0
!    m_par = 5.0d0
!    r_par = 1.0d0
!    command = trim(dir_model)//'\outputs\MSG.txt'
!    open(UNIT_MSG,file=trim(command), status='unknown')
!    command = trim(dir_model)//'\seed.dat'
!    open(2,file=trim(command),status='unknown')
!    READ (2,*,IOSTAT = info)  dumy ! info > 0 something is wrong, info <0 end of file
!    close(2)
!    
!    command = trim(dir_model)//'\outputs\seed.dat'
!    open(2,file=trim(command),status='unknown')
!    Write (2,*)  dumy
!    close(2)
!    
!    
!    DATA (pcnt_d(i),i=1,Nd) /2.0d-2/
!    DATA (alpha(i), i=1,N_a) /57.017d0,35.0d0,25.0d0,20.0d0,18.0d0,15.0d0,12.0d0,8.0d0,5.0d0,3.0d0/
!    DATA (s_par(i), i=1,Np) /12.0d0,15.0d0,0.0005d0/
!    
!    
!    
!    ! generating the random ensembles! =====================================================================================
!    if (ensemble) then
!        command = trim(dir_model)//'\ensemble\true.dat'
!        INQUIRE(FILE=trim(command), EXIST=FEXIST)
!        if (FEXIST) then
!            open(1,file=trim(command),status='old')
!            read(1,*) (m_uc(i,N_e+1),i=1,Np*N)
!            close(1)
!        else
!            print *, 'true.ens file not found'
!            pause
!            stop
!        end if
!        !do i=1,N_e
!        !    if (i<10) then
!        !        write(L2,'(I1)') i
!        !        command = trim(dir_model)//'\ensemble\ENSEM_00'//trim(L2)//'.inc'
!        !        
!        !    elseif (i<100) then
!        !        write(L2,'(I2)') i
!        !        command = trim(dir_model)//'\ensemble\ENSEM_0'//trim(L2)//'.inc'
!        !    else
!        !        write(L2,'(I3)') i
!        !        command = trim(dir_model)//'\ensemble\ENSEM_'//trim(L2)//'.inc'
!        !    end if
!        !    open(1,file=trim(command),status='old')
!        !    read(1,*) (m_uc(j,i),j=1,N*Np)
!        !end do
!        
!        command = trim(dir_model)//'\ensemble\ens.dat'
!        open(1,file=trim(command),status='old')
!        do j=1,N*Np
!            read(1,*) (m_uc(j,i),i=1,N_e)
!        end do
!        close(1)
!    end if
!    
!    
!    
!    write(L2,'(I)') N_e
!    FMT = '('//trim(L2)//'E16.8)'
!    command = trim(dir_model)//'\outputs\par_0.txt'
!    OPEN (1, FILE=trim(command), STATUS='UNKNOWN')
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
!    !write(*,'(A26,10E16.8)') "Initial ensemble MSE:     ", (err_par(k),k=1,Np)
!
!
!    write(L2,'(I)') N_e+2
!    FMT = '('//trim(L2)//'E16.8)'
!
!    ! Running the true model=====================================================================================================
!    
!    call run_ens(N*Np,1,Nd,N_d,N_de,Nt,Nta,m_uc(1:N*Np,N_e+1),d_obs,s_d,dumy,0)
!    ! or read the observed data
!    
!    
!!================================================================================================================================    
!!================================================================================================================================       
!!================================================================================================================================       
!    command = trim(dir_model)//'\seed.dat'
!    OPEN (2, FILE=trim(command), STATUS='UNKNOWN')
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
!    a=0;
!    nolast = .true.
!    alpha = 0.0d0
!    do !a = 1, N_a
!        !alpha_i = alpha(a)
!        a = a+1
!        O_Nd = 0.0d0
!        ! RUN the ensemble from time zero  ======================================================================================
!        call run_ens(N*np,N_e,Nd,N_d,N_de,Nt,Nta,m_uc(1:N*Np,1:N_e),df,s_d,dumy,a)
!        do e = 1, N_e !  ========================================================================================================  
!            tmp_d(1:,1) = df(1:,e) - d_obs(1:,1)
!            do i=1,Nd*Nta
!                O_Nd = O_Nd + tmp_d(i,1)**2 / s_d(i)**2 
!            end do
!        end do !=================================================================================================================
!        O_Nd = O_Nd / (2.0d0 * N_e * Nd * Nta)
!        if (nolast) then
!            alpha_i = 0.5d0 * o_nd
!        endif
!        
!        if ((alpha(1) + 1.0d0 / alpha_i) >= 1.0d0) then
!            alpha_i = 1.0d0 / ( 1.0d0 - alpha(1))
!            nolast = .false.
!        end if
!        
!        write(*,'(A26,E16.8)') "Objective function value: ", O_Nd
!                
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
!        CMD = Cf_MD
!201     err_par = 0.0d0
!        
!        CDD = Cf_DD
!        DO k=1,Nd*Nta
!            CDD (k,k) = Cf_DD(k,k) + alpha_i*s_d(k)**2
!        END DO
!        
!        call inv_svdt(CDD,Nd*Nta,0.98d0)
!        Cf_MD = MATMUL(CMD,CDD)
!
!        
!        do e=1,N_e   
!            do i=1,Nd
!                do j=1,Nta
!                    k = (i-1)*Nta + j
!                    tmp = gasdev(dumy) ! tmp ~ N(0,1)
!                    d_uc(k,e) = d_obs(k,1) + sqrt(alpha_i)*s_d(k)*tmp
!                    if (d_uc(k,e) < 0.0d0) d_uc(k,e) = 0.0d0
!                end do
!            end do
!            tmp_d(1:,1) = d_uc(1:,e) - df(1:,e)
!            tmp_m(1:,1) = MATMUL(Cf_MD,tmp_d(1:,1))
!            
!            tmp_d(1:,1) = MATMUL(CDD,tmp_d(1:,1))
!            
!            lhs = 0.0d0
!            rhs = 0.0d0
!            do i=1,nd*nta
!                lhs = lhs + (d_uc(i,e)-df(i,e))**2 / s_d(i)
!                rhs = rhs + (tmp_d(i,1)**2 * s_d(i))
!            end do
!            
!            lhs = lhs * rho**2
!            rhs = rhs * alpha_i**2
!            
!            
!            
!            if (nolast .and. method1) then
!                do i=1,Np!if (abs(tmp_m(1,1)) > 2.0d0 .or. abs(tmp_m(2,1)) > 20.0d0 / 6.0d0) then
!                    if (abs(tmp_m(i,1)) > 2.5*s_par(i)) then
!                        alpha_i = alpha_i * 2.0d0
!                        write(*,'(a26,e16.8)') , "not satisfied m1 -------: ", alpha_i
!                        goto 201 
!                    end if
!                end do
!                !end do
!            elseif (nolast) then
!                if (lhs > rhs) then
!                   alpha_i = alpha_i * 2.0d0
!                   write(*,'(a26,e16.8)') , "not satisfied m2 -------: ", alpha_i
!                   goto 201 
!                end if
!            end if
!            m_duc(1:,e)  = m_uc(1:,e) + tmp_m(1:,1)
!            do i =1, N
!                if (m_uc(1  ,e) < 0.0d0)then
!                    print *, 1,'th parameter is negative, it is changed to 2.0'
!                    m_uc(1  ,e) = 2.0d0
!                end if
!                do k=1,Np
!                    err_par(k) = err_par(k) + (m_duc((k-1)*N+i  ,e)-m_uc((k-1)*N+i  ,N_e+1))**2 / N_e / N
!                end do
!            end do
!        end do
!        
!        
!        alpha(1) = alpha(1) + 1.0d0 / alpha_i
!        write(unit_msg,'(10e16.8)') (err_par(k),k=1,np),o_nd,alpha_i,alpha(1)
!        
!        m_uc(1:,1:n_e) = m_duc(1:,1:n_e)
!        
!        write(*,'(A16,I2,A8,2E16.8)') "Assimilation:   ", a,"  MSE:  ", o_nd
!        command = trim(dir_model)//'\seed.dat'
!        OPEN (2, FILE=trim(command), STATUS='UNKNOWN')
!        write(2,*) dumy
!        close(2)
!        
!        if (a<10) then 
!            write(L2,'(I1)') a
!        elseif (a<100) then
!            write(L2,'(I2)') a
!        else
!            write(L2,'(I3)') a
!        endif
!        write(L1,'(I)') N_e
!        FMT = '('//trim(L1)//'E16.8)'
!        FILENAME = trim(dir_model)//'\outputs\par_'//trim(L2)//'.txt'
!        OPEN (1, FILE=trim(FILENAME), STATUS='UNKNOWN')
!        do k= 1,Np
!            do i = 1,N
!                write(1,FMT) (m_uc((k-1)*N+i,j),j=1,N_e)
!            end do
!        end do
!        close(1)
!
!        if (.not. nolast) exit
!        
!    end do
!    
!    
!!     print parameters========================================================================================================
!    call run_ens(N*Np,N_e,Nd,N_d,N_de,Nt,Nta,m_uc(1:N*Np,1:N_e),df,s_d,dumy,a+1)
!    
!    write(L1,'(I)') N_e
!    FMT = '('//trim(L1)//'E16.8)'
!    FILENAME = trim(dir_model)//'\outputs\par_end.txt'
!    OPEN (1, FILE=trim(FILENAME), STATUS='UNKNOWN')
!    do k= 1,Np
!        do i = 1,N
!            write(1,FMT) (m_uc((k-1)*N+i,j),j=1,N_e)
!        end do
!    end do
!    close(1)
!    
!end program main_esmda
!    