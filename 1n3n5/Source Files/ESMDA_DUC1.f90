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
!
!    implicit none
!
!    ! Variables
!    real,external                                      :: gasdev
!    integer, parameter                                 :: N_d = 11                 ! number of observed data + time
!    integer, parameter                                 :: Nd  = N_d - 2            ! number of observed data + time
!    integer, parameter                                 :: Nt = 150                 ! number of total time steps
!    integer, parameter                                 :: Nta = 100                ! number of assimilation time steps
!    integer, parameter                                 :: Np = 2                   ! number of parameters per grid block
!    integer, parameter                                 :: N_a = 9                  ! number of data assimilation steps
!    integer, parameter                                 :: N_e = 200                ! number of ensemble members
!    integer, parameter                                 :: Nx = 30                  ! number of gridblocks in x direction
!    integer, parameter                                 :: Ny = 30                  ! number of gridblocks in y direction
!    integer, parameter                                 :: Nz = 1                   ! number of gridblocks in z direction
!    integer, parameter                                 :: N  =Nx*Ny*Nz             ! number of gridblocks
!
!    double precision, dimension(Nt,N_d)                :: d_raw                    ! raw observed data
!    double precision, dimension(Nta*Nd,1)              :: d_obs                    ! observed data
!    double precision, dimension(Nt*Nd,1)               :: d_op                     ! observed data + predicted
!    double precision, dimension(Nta*Nd,N_e)            :: d_uc,df                  ! observed data
!    double precision, dimension(Nt*Nd,N_e)             :: df_op                    ! observed data + predicted
!    double precision, dimension(Nd*Nta)                :: s_d                      ! observed data variance
!    double precision, dimension(Nd)                    :: pcnt_d                   ! observed data variance
!    double precision, dimension(Nz)                    :: Dz
!    double precision, dimension(2*N,1)                 :: mpr, Zm, mdum
!    double precision, dimension(2*N,N_e+1)             :: m_uc,m_duc
!    integer, dimension(Nd,3)                           :: d_loc
!
!    double precision                                   :: Dx, Dy, ax, ay, az, angle,c
!    double precision, allocatable, dimension(:,:)      :: L
!    integer                                            :: i,j,k,e,a, dumy=1642474681,info,kk
!    character*20                                       :: FILENAME, L1,L2,FMT
!    double precision                                   :: tmp,alpha_i
!    LOGICAL                                            :: FEXIST
!    double precision,dimension(N_a)                    :: alpha
!
!    double precision, dimension(Nta*Nd,1)              :: df_b,tmp_d
!    double precision, dimension(2*N,1)                 :: mf_b,tmp_m
!    double precision, dimension(Nta*Nd,Nta*Nd)         :: C_D
!    double precision, dimension(Nta*Nd,Nta*Nd)         :: Cf_DD, CDD
!    double precision, dimension(Nta*Nd,Nta*Nd)         :: rho_dd
!    double precision, dimension(2*N,Nta*Nd)            :: Cf_MD
!    double precision, dimension(2*N,Nta*Nd)            :: rho_md             ! Gaspari-Cohn correlation matrix
!    integer, dimension(Nta*Nd)                         :: ipiv
!    double precision                                   :: err_lnk, err_phi, O_Nd, s_lnk, s_phi
!    
!    s_lnk = 1.0d0
!    s_phi = 0.03d0
!
!
!
!    open(2,file='.\seed.dat',status='unknown')
!    READ (2,*,IOSTAT = info)  dumy ! info > 0 something is wrong, info <0 end of file
!    close(2)
!    open(2,file='.\seed2.dat',status='unknown')
!    Write (2,*)  dumy
!    close(2)
!
!    Dz(1)    = 10.0d0
!    Dx       = 80.0d0
!    Dy       = 80.0d0
!    
!    ax = 20.0d0*Dx 
!    ay = 7.0d0*Dy
!    az = 5.0d0*sum(dz)/Nz
!    angle = 30.0d0
!    c = 1.0d0
!    
!    DATA (pcnt_d(i),i=1,Nd) /3*2.0d-2,3*3.0d-2,3*2.0d-2/
!
!    !DATA (alpha(i), i=1,N_a) /9.3333d0,7.0d0,4.0d0,2.0d0/
!    !DATA (alpha(i), i=1,N_a) /20.0d0,10.0d0,5.0d0,1.5385d0/
!    DATA (alpha(i), i=1,N_a) /N_a*N_a/
!
!    d_loc(1,1)= 1;  d_loc(1,2)= 1;  d_loc(1,3) = 1;
!    d_loc(2,1)= 18; d_loc(2,2)= 26; d_loc(2,3) = 1;
!    d_loc(3,1)= 26; d_loc(3,2)= 8;  d_loc(3,3) = 1;
!    d_loc(4,1)= 1;  d_loc(4,2)= 1;  d_loc(4,3) = 1;
!    d_loc(5,1)= 18; d_loc(5,2)= 26; d_loc(5,3) = 1;
!    d_loc(6,1)= 26; d_loc(6,2)= 8;  d_loc(6,3) = 1;
!    d_loc(7,1)= 17; d_loc(7,2)= 1;  d_loc(7,3) = 1;
!    d_loc(8,1)= 30; d_loc(8,2)= 30; d_loc(8,3) = 1;
!    d_loc(9,1)= 1 ; d_loc(9,2)= 15; d_loc(9,3) = 1;
!
!
!    d_loc(1,1)= 5;  d_loc(1,2)= 14;  d_loc(1,3) = 1;
!    d_loc(2,1)= 18; d_loc(2,2)= 16; d_loc(2,3) = 1;
!    d_loc(3,1)= 15; d_loc(3,2)= 5;  d_loc(3,3) = 1;
!    d_loc(4,1)= 5;  d_loc(4,2)= 14;  d_loc(4,3) = 1;
!    d_loc(5,1)= 18; d_loc(5,2)= 16; d_loc(5,3) = 1;
!    d_loc(6,1)= 15; d_loc(6,2)= 5;  d_loc(6,3) = 1;
!    d_loc(7,1)= 7; d_loc(7,2)= 7;  d_loc(7,3) = 1;
!    d_loc(8,1)= 15; d_loc(8,2)= 27; d_loc(8,3) = 1;
!    d_loc(9,1)= 26 ; d_loc(9,2)= 10; d_loc(9,3) = 1;
!
!    call ECL_BAT_GEN    
!    allocate(L(Np*N,Np*N)); L = 0.0d0
!    rho_md =0.0d0
!    rho_dd =0.0d0
!    call GASCOHN_CMD(Nd,Nta,rho_md,d_loc,Nx,Ny,Nz,Dx,Dy,Dz,c*ax, c*ay, c*az, angle)
!    call GASCOHN_CDD(Nd,Nta,rho_dd,d_loc,Nx,Ny,Nz,Dx,Dy,Dz,c*ax, c*ay, c*az, angle)
!
!    ! generating the random ensembles! =====================================================================================
!
!    call CML(L,Nx,Ny,Nz,Dx,Dy,Dz,ax,ay,az,angle) ! L is the Choleskey decomposition of the covariance matrix
!
!    do i=1,N
!        mpr(i,1)   = 5.0d0
!        mpr(N+i,1) = 0.20d0
!    end do
!    INQUIRE(FILE='.\true.txt', EXIST=FEXIST)
!    if (FEXIST) then
!        open(1,file='.\true.txt',status='old')
!        read(1,*) (m_uc(i,N_e+1),i=1,2*N)
!        close(1)
!        do e=1, N_e
!            do i=1,N
!                Zm(i,1)=    gasdev(dumy)
!                Zm(N+i,1)=  gasdev(dumy)
!            end do
!            mdum=mpr+MATMUL(L,Zm)
!            m_uc(1:2*N,e)=mdum(1:2*N,1)
!        end do
!    else
!
!    do e=1, N_e + 1
!        do i=1,N
!            Zm(i,1)=    gasdev(dumy)
!            Zm(N+i,1)=  gasdev(dumy)
!        end do
!        mdum=mpr+MATMUL(L,Zm)
!        m_uc(1:2*N,e)=mdum(1:2*N,1)
!    end do
!    open(1,file='.\true.txt',status='new')
!    do i=1,2*N
!        write(1,'(E18.8)') m_uc(i,N_e+1)
!    end do
!    end if
!
!
!
!    write(L2,'(I)') N_e
!    FMT = '('//trim(L2)//'E16.8)'
!
!    OPEN (1, FILE='.\par_init.txt', STATUS='UNKNOWN')
!    
!    err_phi = 0.0d0
!    err_lnk = 0.0d0
!    do i = 1,N
!        write(1,FMT) (m_uc(i,j),j=1,N_e)
!        do e = 1,N_e
!            err_lnk = err_lnk + (m_uc(i  ,e)-m_uc(i  ,N_e+1))**2 / N_e / N
!        end do
!    end do
!    do i = 1,N
!        write(1,FMT) (m_uc(i+N,j),j=1,N_e)
!        do e = 1,N_e
!            err_phi = err_phi + (m_uc(i+N,e)-m_uc(i+N,N_e+1))**2 / N_e / N
!        end do
!    end do
!    
!    write(*,'(A26,2E16.8)') "Initial ensemble MSE:     ", err_lnk,err_phi
!
!
!    write(L2,'(I)') N_e+2
!    FMT = '('//trim(L2)//'E16.8)'
!
!    ! Running the true model=====================================================================================================
!    INQUIRE(FILE='.\true_obs.txt', EXIST=FEXIST)
!    if (FEXIST) then
!        open(1,FILE='.\true_obs.txt', status = 'old')
!        read(1,*) (d_op(k,1),k=1,Nd*Nt)
!        close(1)
!        kk = 0
!        do i=1,Nd
!            do j=1,Nt
!
!            k = (i-1)*Nt + j
!            if (j<=Nta) then
!                kk = kk+1
!                s_d(kk) = pcnt_d(i)*d_op(k,1)
!                d_obs(kk,1)  = d_op(k,1)
!                if (i>6) s_d(kk) = min(pcnt_d(i)*d_op(k,1),10.0d0)
!            end if
!            end do
!        end do
!    else
!        FILENAME='INC\POROSITY.inc'
!        L1='PORO'
!        CALL W_INC_FILE (m_uc(N+1:2*N,N_e+1),N,5,FILENAME,L1)             ! POROSITY INCLUDE FILE
!
!        FILENAME='INC\PERMX.inc'
!        L1='PERMX'
!        CALL W_INC_FILE (EXP(m_uc(1:N,N_e+1)),N,5,FILENAME,L1)             ! POROSITY INCLUDE FILE
!
!101     call RUN_ECL
!        info = 0
!        call Success_Ecl(info)
!        if (info .NE. 0) then
!            print *, 'Simulation Run Unsuccessful'
!            goto 101
!        end if
!        call READ_OBSERVED_DATA(N_d,Nt,d_raw)
!        ! perturbing the raw data to get observation data
!        kk=0
!        do i=1,Nd
!            do j=1,Nt
!                k = (i-1)*Nt + j
!                tmp = gasdev(dumy)*pcnt_d(i)*d_raw(j,i+2)
!                if (i>6) tmp = gasdev(dumy)*min(pcnt_d(i)*d_raw(j,i+2),10.0d0)
!                d_op(k,1) = d_raw(j,i+2) + tmp
!                if (d_op(k,1) < 0.0d0) d_op(k,1) = 0.0d0
!                if (j<=Nta) then
!                    kk=kk+1
!                    s_d(kk) = pcnt_d(i)*d_raw(j,i+2)
!                    if (i>6) s_d(kk) = min(pcnt_d(i)*d_raw(j,i+2),5.0d0)
!                    d_obs(kk,1) = d_op(k,1)
!                end if
!            end do
!        end do
!        open(1,FILE='.\true_obs.txt', status = 'new')
!        do k=1,Nd*Nt
!            write(1,'(E20.8)') d_op(k,1)
!        end do
!        close(1)
!    end if
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
!    alpha(1) = 0.0d0
!    a = 0
!    do !a = 1, N_a
!        !alpha_i = alpha(a)
!        a = a+1
!        O_Nd = 0.0d0
!        ! RUN the ensemble from time zero  ======================================================================================
!        do e = 1, N_e !  ========================================================================================================
!
!            ! Write the model parameters in the model file
!            FILENAME='INC\POROSITY.inc'
!            L1='PORO'
!            CALL W_INC_FILE (m_uc(N+1:2*N,e),N,5,FILENAME,L1)             ! POROSITY INCLUDE FILE
!
!            FILENAME='INC\PERMX.inc'
!            L1='PERMX'
!            CALL W_INC_FILE (EXP(m_uc(1:N,e)),N,5,FILENAME,L1)             ! POROSITY INCLUDE FILE
!            ! Run the forward model (eclipse)
!102         call RUN_ECL
!            info = 0
!            call Success_Ecl(info)
!            if (info .NE. 0) then
!                print *, 'Simulation Run Unsuccessful'
!                goto 102
!            end if
!            ! read the observated data
!            call READ_OBSERVED_DATA(N_d,Nt,d_raw)
!            ! Perturb the obsevation vector using:
!            !     d_uc = d_obs +  C_D^0.5 Z_d, where Z_d ~ N(0, I_Nd)
!            do i=1,Nd
!                do j=1,Nta
!                    k = (i-1)*Nta + j
!                    !tmp = gasdev(dumy) ! tmp ~ N(0,1)
!                    df(k,e) = d_raw(j,i+2) !+ s_d(k)*tmp
!                    if (df(k,e) < 0.0d0) df(k,e) = 0.0d0
!                end do
!            end do
!            do i=1,Nd
!                do j=1,Nt
!                    k = (i-1)*Nt + j
!                    !tmp = gasdev(dumy) ! tmp ~ N(0,1)
!                    df_op(k,e) = d_raw(j,i+2) !+ s_d(k)*tmp
!                    if (df_op(k,e) < 0.0d0) df_op(k,e) = 0.0d0
!                end do
!            end do
!            tmp_d(1:,1) = df(1:,e) - d_obs(1:,1)
!            do i=1,Nd*Nta
!                O_Nd = O_Nd + tmp_d(i,1)**2 / s_d(i)**2 / (N_e * Nd * Nta)
!            end do
!        end do !=================================================================================================================
!        alpha_i = 0.25d0 * O_Nd        
!        write(*,'(A26,E16.8)') "Objective function value: ", O_Nd
!        
!        if (a<=10) then 
!            write(L2,'(I1)') a-1
!        elseif (a<=100) then
!            write(L2,'(I2)') a-1
!        else
!            write(L2,'(I3)') a-1
!        endif
!        
!        write(L1,'(I)') N_e+2
!        FMT = '('//trim(L1)//'E16.8)'
!        
!        FILENAME='.\output_'//trim(L2)//'.txt'
!        OPEN (1, FILE=trim(FILENAME), STATUS='UNKNOWN')
!        do i = 1,Nd
!            do j = 1, Nt
!                k = (i-1)*Nt+j
!                write(1,FMT) d_raw(j,1),d_op(k,1), (df_op(k,e),e=1,N_e)
!            end do
!        end do
!        close(1)                
!
!        !Calculation of MEAN of the MF and DF
!        do i=1,Nta*Nd
!            df_b(i,1) = sum(df(i,1:N_e)) / N_e
!        end do
!
!        do i=1,2*N
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
!        ! Applying Gaspari-Cohn correlation on Cf_MD
!        do i=1,2*N
!            do j=1,Nd*Nta
!                Cf_MD(i,j) = Cf_MD(i,j)*ABS(RHO_md(i,j))
!            end do
!        end do
!        do i=1,Nd*Nta
!            do j=1,Nd*Nta
!                Cf_DD(i,j) = Cf_DD(i,j)*ABS(RHO_dd(i,j))
!            end do
!        end do
!
!
!201     err_lnk = 0.0d0
!        err_phi = 0.0d0
!        do e=1,N_e   
!            !    - Solve for X vector
!            !      (Cf_DD + alpha_i*C_D)*X = (d_uc_e - df_j)
!
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
!            do i = 1,N
!                if (abs(tmp_m(i,1)) > 2.0d0*s_lnk .OR. abs(tmp_m(i+N,1)) > 2.0d0*s_phi) then
!                    alpha_i = alpha_i * 2.0d0
!                    Write(*,'(A26,E16.8)') , "Not satisfied-------: ",alpha_i
!                    goto 201
!                end if
!            end do
!            m_duc(1:,e)  = m_uc(1:,e) + tmp_m(1:,1)
!            do i =1, N
!                if (m_duc(i  ,e) < 0.0d0) m_duc(i  ,e) = 2.3d0
!                if (m_duc(i+N,e) < 0.0d0) m_duc(i+N,e) = 0.05d0
!                err_lnk = err_lnk + (m_duc(i  ,e)-m_uc(i  ,N_e+1))**2 / N_e / N
!                err_phi = err_phi + (m_duc(i+N,e)-m_uc(i+N,N_e+1))**2 / N_e / N
!            end do
!            !m_uc(1:,e)  = m_uc(1:,e) + tmp_m(1:,1)
!            !do i =1, N
!            !    if (m_uc(i  ,e) < 0.0d0) m_uc(i  ,e) = 2.3d0
!            !    if (m_uc(i+N,e) < 0.0d0) m_uc(i+N,e) = 0.05d0
!            !    err_lnk = err_lnk + (m_uc(i  ,e)-m_uc(i  ,N_e+1))**2 / N_e / N
!            !    err_phi = err_phi + (m_uc(i+N,e)-m_uc(i+N,N_e+1))**2 / N_e / N
!            !end do
!        end do
!        m_uc(1:,1:N_e) = m_duc(1:,1:N_e)
!        alpha(1) = alpha(1) + 1.0d0 / alpha_i
!        if (alpha(1)>1) exit
!        write(*,'(A16,I2,A8,2E16.8)') "Assimilation:   ", a,"  MSE:  ", err_lnk,err_phi
!        open(2,file='.\seed.dat',status='unknown')
!        write(2,*) dumy
!        close(2)
!        
!        write(L1,'(I)') N_e
!        FMT = '('//trim(L1)//'E16.8)'
!        FILENAME='.\par_'//trim(L2)//'.txt'
!        OPEN (1, FILE=trim(FILENAME), STATUS='UNKNOWN')
!        do i = 1,N
!            write(1,FMT) (m_uc(i,j),j=1,N_e)
!        end do
!        do i = 1,N
!            write(1,FMT) (m_uc(i+N,j),j=1,N_e)
!        end do
!        close(1)
!
!    end do
!
!    ! printing the outputs
!    if (a<=10) then 
!            write(L2,'(I1)') a-1
!    elseif (a<=100) then
!            write(L2,'(I2)') a-1
!    else
!            write(L2,'(I3)') a-1
!    endif
!    FILENAME='.\output_'//trim(L2)//'.txt'
!    OPEN (1, FILE=trim(FILENAME), STATUS='UNKNOWN')
!    do i = 1,Nd
!        do j = 1, Nt
!            k = (i-1)*Nt+j
!            write(1,FMT) d_raw(j,1),d_op(k,1), (df_op(k,e),e=1,N_e)
!        end do
!    end do
!    close(1)
!    
!    
!    ! print parameters========================================================================================================
!    write(L1,'(I)') N_e
!    FMT = '('//trim(L1)//'E16.8)'
!
!    OPEN (1, FILE='.\par_end.txt', STATUS='UNKNOWN')
!
!    do i = 1,N
!        write(1,FMT) (m_uc(i,j),j=1,N_e)
!    end do
!    do i = 1,N
!        write(1,FMT) (m_uc(i+N,j),j=1,N_e)
!    end do
!    close(1)
!
!    do e = 1, N_e ! ========================================================================================================
!        ! Write the model parameters in the model file
!        FILENAME='INC\POROSITY.inc'
!        L1='PORO'
!        CALL W_INC_FILE (m_uc(N+1:2*N,e),N,5,FILENAME,L1)             ! POROSITY INCLUDE FILE
!
!        FILENAME='INC\PERMX.inc'
!        L1='PERMX'
!        CALL W_INC_FILE (EXP(m_uc(1:N,e)),N,5,FILENAME,L1)             ! POROSITY INCLUDE FILE
!        ! Run the forward model (eclipse)
!103     call RUN_ECL
!        info = 0
!        call Success_Ecl(info)
!        if (info .NE. 0) then
!            print *, 'Simulation Run Unsuccessful'
!            goto 103
!        end if
!        ! read the observated data
!        call READ_OBSERVED_DATA(N_d,Nt,d_raw)
!        do i=1,Nd
!            do j=1,Nta
!                k = (i-1)*Nta + j
!                !tmp = gasdev(dumy) ! tmp ~ N(0,1)
!                df(k,e) = d_raw(j,i+2) !+ s_d(k)*tmp
!                if (df(k,e) < 0.0d0) df(k,e) = 0.0d0
!            end do
!        end do
!        do i=1,Nd
!            do j=1,Nt
!                k = (i-1)*Nt + j
!                df_op(k,e) = d_raw(j,i+2)
!            end do
!        end do
!        
!        tmp_d(1:,1) = df(1:,e) - d_obs(1:,1)
!        do i=1,Nd*Nta
!           O_Nd = O_Nd + tmp_d(i,1)**2 / s_d(i)**2 / (N_e * Nd * Nta)
!        end do
!    end do
!    
!    write(*,'(A26,E16.8)') "Objective function value: ", O_Nd
!    
!    write(L2,'(I)') N_e+2
!    FMT = '('//trim(L2)//'E16.8)'
!    write(L2,'(I1)') 5
!    FILENAME='.\output_'//trim(L2)//'.txt'
!    OPEN (1, FILE=FILENAME, STATUS='UNKNOWN')
!
!    do i = 1,Nd
!        do j = 1, Nt
!            k = (i-1)*Nt+j
!            write(1,FMT) d_raw(j,1),d_op(k,1), (df_op(k,e),e=1,N_e)
!        end do
!    end do
!    close(1)
!
!
!
!
!    end program ESMDA
!
!    ! Body of ESMDA ========================================================================================================
!    ! 1) choose the number of data assilimilation, N_a, and the coefficients alpha_i
!    ! 2) do i = 1, N_a
!    !    a) Run the ensemble from time zero
!    !    b) For each ensemble member, perturb the observation vector using:
!    !       d_uc = d_obs + sqrt(alpha_i) * C_D^0.5 Z_d, where Z_d ~ N(0, I_Nd)
!    !    c) Update  the ensemble
!    !       ma_j = mf_j + Cf_MD (Cf_DD + alpha_i*C_D)^(-1) *(d_uc_j - df_j)
!    !=======================================================================================================================
!
!    !do i = 1, N_a
!    !    do j = 1, N_e 
!    !        ! a) Run the ensemble member "j" from time zero
!    !        !    - Write the parameter in the model file
!    !        !    - Run the forward model
!    !        !    - Read the observed data and copy in the observation vector
!    !        ! b) Perturb the obsevation vector using:
!    !        !         d_uc = d_obs + sqrt(alpha_i) * C_D^0.5 Z_d, where Z_d ~ N(0, I_Nd)
!    !    end do
!    !    
!    !    do j =1, N_e
!    !        ! c) Update the ensemble member 
!    !        !    - Calculate Cf_MD , Cf_DD using matrix multiplication
!    !        !    - Sovlve for X vector
!    !        !          (Cf_DD + alpha_i*C_D)*X = (d_uc_j - df_j)
!    !        !    - Update the ensemble member
!    !        !          ma_j = mf_j + Cf_MD*X
!    !    end do 
!    !end do
!
