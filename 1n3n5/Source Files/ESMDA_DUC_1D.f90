!    !  esmda.f90 
!    !
!    !  functions:
!    !  esmda - entry point of console application.
!    !
!
!    !****************************************************************************
!    
!    !  program: esmda
!    !
!    !  purpose:  entry point for the console application.
!    !
!    !****************************************************************************
!
!    program esmda
!    use global_variables, only: cwd
!    implicit none
!
!    ! variables
!    real,external                                      :: gasdev
!    integer, parameter                                 :: n_d = 1 + 2              ! number of observed data + time
!    integer, parameter                                 :: n_de= 3                  ! number of observed data + time
!    integer, parameter                                 :: nd  = n_d - 2            ! number of observed data + time
!    integer, parameter                                 :: nt = 25                  ! number of total time steps
!    integer, parameter                                 :: nta = 12                 ! number of assimilation time steps
!    integer, parameter                                 :: np = 1                   ! number of parameters per grid block
!    integer, parameter                                 :: n_a = 1                  ! number of data assimilation steps
!    integer, parameter                                 :: n_e = 100                ! number of ensemble members
!    integer, parameter                                 :: nx = 31                  ! number of gridblocks in x direction
!    integer, parameter                                 :: ny = 1                   ! number of gridblocks in y direction
!    integer, parameter                                 :: nz = 1                   ! number of gridblocks in z direction
!    integer, parameter                                 :: n  =nx*ny*nz             ! number of gridblocks
!
!    double precision, dimension(nt,n_d+n_de)           :: d_raw                    ! raw observed data
!    double precision, dimension(nta*nd,1)              :: d_obs                    ! observed data
!    double precision, dimension(nt*n_de,n_e+1)         :: d_mon                    ! monitored data
!    double precision, dimension(nt*nd,1)               :: d_op                     ! observed data + predicted
!    double precision, dimension(nta*nd,n_e)            :: d_uc,df                  ! observed data
!    double precision, dimension(nt*nd,n_e)             :: df_op                    ! observed data + predicted
!    double precision, dimension(nd*nta)                :: s_d                      ! observed data variance
!    double precision, dimension(nd)                    :: pcnt_d                   ! observed data variance
!    double precision, dimension(nz)                    :: dz
!    double precision, dimension(np*n,1)                :: mpr, zm, mdum
!    double precision, dimension(np*n,n_e+1)            :: m_uc,m_duc
!    integer, dimension(nd,3)                           :: d_loc
!    
!    double precision                                   :: dx, dy, ax, ay, az, angle,c
!    double precision, allocatable, dimension(:,:)      :: l
!    integer                                            :: i,j,k,e,a, dumy=1642474681,info,kk
!    character*200                                      :: filename, l1,l2,fmt
!    double precision                                   :: tmp,alpha_i
!    logical                                            :: fexist
!    double precision,dimension(n_a)                    :: alpha
!
!    double precision, dimension(nta*nd,1)              :: df_b,tmp_d
!    double precision, dimension(np*n,1)                :: mf_b,tmp_m
!    double precision, dimension(nta*nd,nta*nd)         :: c_d
!    double precision, dimension(nta*nd,nta*nd)         :: cf_dd, cdd
!    double precision, dimension(nta*nd,nta*nd)         :: rho_dd
!    double precision, dimension(np*n,nta*nd)           :: cf_md
!    double precision, dimension(np*n,nta*nd)           :: rho_md             ! gaspari-cohn correlation matrix
!    integer, dimension(nta*nd)                         :: ipiv
!    double precision                                   :: err_lnk, err_phi, o_nd, rhs,lhs, rho
!    double precision,dimension(np)                     :: s_par, m_par,err_par
!    double precision,dimension(np,np)                  :: r_par
!    logical                                            :: local, nolast,method1,ensemble
!    character*400                                      :: command
!    integer                                            :: unit_msg
!    
!    ensemble   = .true.
!    local     = .false.
!    method1   = .true.
!    unit_msg  = 110
!    
!    rho = 0.2d0 ! for method 2
!    
!    command = 'del '//trim(cwd)//'\outputs\*.txt'
!    call system(trim(command))
!    s_par = 1.0d0
!    m_par = 5.0d0
!    r_par = 1.0d0
!    open(unit_msg,file='.\outputs\msg.txt',status='unknown')
!    open(2,file='.\seed.dat',status='unknown')
!    read (2,*,iostat = info)  dumy ! info > 0 something is wrong, info <0 end of file
!    close(2)
!    open(2,file='.\outputs\seed2.dat',status='unknown')
!    write (2,*)  dumy
!    close(2)
!
!    dz(1)    = 50.0d0
!    dx       = 50.0d0
!    dy       = 50.0d0
!    
!    ax = 10.0d0*dx 
!    ay = 7.0d0*dy
!    az = 5.0d0*sum(dz)/nz
!    angle = 0.0d0
!    c = 1.0d0
!    
!    DATA (pcnt_d(i),i=1,Nd) /2.0d-2/
!
!    !DATA (alpha(i), i=1,N_a) /9.3333d0,7.0d0,4.0d0,2.0d0/
!    !DATA (alpha(i), i=1,N_a) /20.0d0,10.0d0,5.0d0,1.5385d0/
!    DATA (alpha(i), i=1,N_a) /N_a*N_a/
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
!    write(l2,'(i)') n_e
!    fmt = '('//trim(l2)//'e16.8)'
!
!    open (1, file='.\outputs\par_init.txt', status='unknown')
!    
!    err_par = 0.0d0
!    do k = 1,np
!        do i = 1,n
!            write(1,fmt) (m_uc((k-1)*n+i,j),j=1,n_e)
!            do e = 1,n_e
!                err_par(k) = err_par(k) + (m_uc((k-1)*n+i  ,e)-m_uc((k-1)*n+i  ,n_e+1))**2 / n_e / n
!            end do
!        end do
!    end do
!    
!    write(*,'(a26,10e16.8)') "initial ensemble mse:     ", (err_par(k),k=1,np)
!
!
!    write(l2,'(i)') n_e+2
!    fmt = '('//trim(l2)//'e16.8)'
!
!    
!        filename='inc\permx.inc'
!        l1='permx'
!        call w_inc_file (exp(m_uc(1:n,n_e+1)),n,filename,l1)             ! porosity include file
!
!101     call run_ecl
!        info = 0
!        call success_ecl(info)
!        if (info .ne. 0) then
!            print *, 'simulation run unsuccessful'
!            goto 101
!        end if
!        call read_observed_data(n_d+n_de,nt,d_raw)
!        ! perturbing the raw data to get observation data
!        kk=0
!        do i=1,nd
!            do j=1,nt
!                k = (i-1)*nt + j
!                tmp = gasdev(dumy)*1.0d0
!                !if (i>6) tmp = gasdev(dumy)*min(pcnt_d(i)*d_raw(j,i+2),10.0d0)
!                d_op(k,1) = d_raw(j,i+2) + tmp
!                if (d_op(k,1) < 0.0d0) d_op(k,1) = 0.0d0
!                if (j<=nta) then
!                    kk=kk+1
!                    s_d(kk) = 1.0d0
!                    d_obs(kk,1) = d_op(k,1)
!                end if
!            end do
!        end do
!        if (n_de > 0) then
!            do i=1,n_de
!                d_mon((i-1)*nt+1:i*nt,n_e+1) = d_raw(1:nt,n_d+i)
!            end do
!        end if
!        open(1,file='.\outputs\true_obs.txt', status = 'unknown')
!        do k=1,nd*nt
!            write(1,'(e20.8)') d_op(k,1)
!        end do
!        do k=1,n_de*nt
!            write(1,'(e20.8)') d_mon(k,n_e+1)
!        end do
!        close(1)
!    !end if
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
!    do i=1,nd
!        do j=1,nta
!            k = (i-1)*nta + j
!            c_d (k,k) = s_d(k)**2
!        end do
!    end do
!    a=0;
!    nolast = .true.
!    alpha = 0.0d0
!    do !a = 1, n_a
!        !alpha_i = alpha(a)
!        a = a+1
!        o_nd = 0.0d0
!        ! run the ensemble from time zero  ======================================================================================
!        do e = 1, n_e !  ========================================================================================================
!            ! write the model parameters in the model file
!            !filename='inc\porosity.inc'
!            !l1='poro'
!            !call w_inc_file (m_uc(n+1:2*n,e),n,filename,l1)             ! porosity include file
!
!            filename='inc\permx.inc'
!            l1='permx'
!            call w_inc_file (exp(m_uc(1:n,e)),n,filename,l1)             ! porosity include file
!            ! run the forward model (eclipse)
!102         call run_ecl
!            info = 0
!            call success_ecl(info)
!            if (info .ne. 0) then
!                print *, 'simulation run unsuccessful'
!                goto 102
!            end if
!            ! read the observated data
!            call read_observed_data(n_d+n_de,nt,d_raw)
!            do i=1,nd
!                do j=1,nta
!                    k = (i-1)*nta + j
!                    df(k,e) = d_raw(j,i+2)
!                    if (df(k,e) < 0.0d0) df(k,e) = 0.0d0
!                end do
!            end do
!            do i=1,nd
!                do j=1,nt
!                    k = (i-1)*nt + j
!                    df_op(k,e) = d_raw(j,i+2)
!                    if (df_op(k,e) < 0.0d0) df_op(k,e) = 0.0d0
!                end do
!            end do
!            
!            if (n_de > 0) then
!                do i=1,n_de
!                    d_mon((i-1)*nt+1:i*nt,e) = d_raw(1:nt,n_d+i)
!                end do
!            end if
!            
!            tmp_d(1:,1) = df(1:,e) - d_obs(1:,1)
!            do i=1,nd*nta
!                o_nd = o_nd + tmp_d(i,1)**2 / s_d(i)**2 / (n_e * nd * nta)
!            end do
!        end do !=================================================================================================================
!        if (nolast) then
!            alpha_i = 0.25d0 * o_nd
!        endif
!        
!        write(*,'(a26,e16.8)') "objective function value: ", o_nd
!        !write(unit_msg,'(10e16.8)') (err_par(k),k=1,np),o_nd
!        if (a<=10) then 
!            write(l2,'(i1)') a-1
!        elseif (a<=100) then
!            write(l2,'(i2)') a-1
!        else
!            write(l2,'(i3)') a-1
!        endif
!        
!        write(l1,'(i)') n_e+2
!        fmt = '('//trim(l1)//'e16.8)'
!        
!        filename='.\outputs\output_'//trim(l2)//'.txt'
!        open (1, file=trim(filename), status='unknown')
!        do i = 1,nd
!            do j = 1, nt
!                k = (i-1)*nt+j
!                write(1,fmt) d_raw(j,1),d_op(k,1), (df_op(k,e),e=1,n_e)
!            end do
!        end do
!        do i = 1,n_de
!            do j = 1,nt
!               k =(i-1)*nt+j
!               write(1,fmt) d_raw(j,1),d_mon(k,n_e+1), (d_mon(k,e),e=1,n_e)
!            end do
!        end do
!        close(1)                
!
!        !calculation of mean of the mf and df
!        do i=1,nta*nd
!            df_b(i,1) = sum(df(i,1:n_e)) / n_e
!        end do
!
!        do i=1,np*n
!            mf_b(i,1) = sum(m_uc(i,1:n_e)) / n_e
!        end do
!
!        !    - calculate cf_md , cf_dd using matrix multiplication
!        cf_dd = 0.0d0
!        cf_md = 0.0d0
!
!        do e=1,n_e
!            tmp_d(1:,1) = df(1:,e)  -df_b(1:,1)
!            tmp_m(1:,1) = m_uc(1:,e)-mf_b(1:,1)
!            cf_dd = cf_dd + matmul(tmp_d,transpose(tmp_d))
!            cf_md = cf_md + matmul(tmp_m,transpose(tmp_d))
!        end do
!
!        cf_dd = cf_dd / (n_e-1.0d0)
!        cf_md = cf_md / (n_e-1.0d0)
!        if (local) then
!        ! applying gaspari-cohn correlation on cf_md
!            do i=1,2*n
!                do j=1,nd*nta
!                    cf_md(i,j) = cf_md(i,j)*abs(rho_md(i,j))
!                end do
!            end do
!            do i=1,nd*nta
!                do j=1,nd*nta
!                    cf_dd(i,j) = cf_dd(i,j)*abs(rho_dd(i,j))
!                end do
!            end do
!        end if
!        
!
!
!201     err_par = 0.0d0
!        do e=1,n_e   
!            !    - solve for x vector
!            !      (cf_dd + alpha_i*c_d)*x = (d_uc_e - df_j)
!            do i=1,nd
!                do j=1,nta
!                    k = (i-1)*nta + j
!                    tmp = gasdev(dumy) ! tmp ~ n(0,1)
!                    d_uc(k,e) = d_obs(k,1) + sqrt(alpha_i)*s_d(k)*tmp
!                    if (d_uc(k,e) < 0.0d0) d_uc(k,e) = 0.0d0
!                end do
!            end do
!            info = 0
!            ipiv = 0
!            tmp_d(1:,1) = d_uc(1:,e) - df(1:,e)
!            cdd = cf_dd + alpha_i*c_d
!            call dgesv( nd*nta, 1, cdd, nd*nta, ipiv,tmp_d(1:,1) ,nd*nta, info )
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
!            tmp_m(1:,1) = matmul(cf_md,tmp_d(1:,1))
!            if (nolast .and. method1) then
!                do i = 1,n
!                    do j=1,np
!                        if (abs(tmp_m((j-1)*n+i,1)) > 2.0d0*s_par(j)) then
!                           alpha_i = alpha_i * 2.0d0
!                           write(*,'(a26,e16.8)') , "not satisfied m1 -------: ", alpha_i
!                           goto 201 
!                        end if
!                    end do
!                end do
!            elseif (nolast) then
!                if (lhs > rhs) then
!                   alpha_i = alpha_i * 2.0d0
!                   write(*,'(a26,e16.8)') , "not satisfied m2 -------: ", alpha_i
!                   goto 201 
!                end if
!            end if
!            
!            m_duc(1:,e)  = m_uc(1:,e) + tmp_m(1:,1)
!            do i =1, n
!                if (m_uc(i  ,e) < 0.0d0)then
!                    print *, i,'th parameter is negative, it is changed to 2.0'
!                    m_uc(i  ,e) = 2.0d0
!                end if
!                do k=1,np
!                    err_par(k) = err_par(k) + (m_duc((k-1)*n+i  ,e)-m_uc((k-1)*n+i  ,n_e+1))**2 / n_e / n
!                end do
!            end do
!        end do !=============== loop on ensemble members ==================================================
!        if ((alpha(1) + 1.0d0 / alpha_i) > 1.0d0) then
!            alpha_i = 1.0d0 / ( 1.0d0 - alpha(1))
!            nolast = .false.
!            go to 201
!        end if
!        
!        m_uc(1:,1:n_e) = m_duc(1:,1:n_e)
!        
!        write(unit_msg,'(10e16.8)') (err_par(k),k=1,np),o_nd,alpha_i
!        alpha(1) = alpha(1) + 1.0d0 / alpha_i
!        write(*,'(a16,i2,a8,2e16.8)') "assimilation:   ", a,"  mse:  ", (err_par(k),k=1,np)
!        open(2,file='.\seed.dat',status='unknown')
!        write(2,*) dumy
!        close(2)
!        
!        write(l1,'(i)') n_e
!        fmt = '('//trim(l1)//'e16.8)'
!        filename='.\outputs\par_'//trim(l2)//'.txt'
!        open (1, file=trim(filename), status='unknown')
!        do k= 1,np
!            do i = 1,n
!                write(1,fmt) (m_uc((k-1)*n+i,j),j=1,n_e)
!            end do
!        end do
!        close(1)
!        
!        if (.not. nolast) exit
!            
!        if (alpha(1) > 0.8) then
!            alpha_i = 1.0d0 / ( 1.0d0 - alpha(1))
!            nolast = .false.
!        end if
!        
!
!    end do
!
!    ! printing the outputs
!    if (a<=10) then 
!            write(l2,'(i1)') a-1
!    elseif (a<=100) then
!            write(l2,'(i2)') a-1
!    else
!            write(l2,'(i3)') a-1
!    endif
!    write(l1,'(i)') n_e+2
!    fmt = '('//trim(l1)//'e16.8)'
!    filename='.\outputs\output_'//trim(l2)//'.txt'
!    open (1, file=trim(filename), status='unknown')
!    do i = 1,nd
!        do j = 1, nt
!            k = (i-1)*nt+j
!            write(1,fmt) d_raw(j,1),d_op(k,1), (df_op(k,e),e=1,n_e)
!        end do
!    end do
!    do i = 1,n_de
!        do j = 1,nt
!           k =(i-1)*nt+j
!           write(1,fmt) d_raw(j,1),d_mon(k,n_e+1), (d_mon(k,e),e=1,n_e)
!        end do
!    end do
!    close(1)
!    
!    
!    ! print parameters========================================================================================================
!    write(l1,'(i)') n_e
!    fmt = '('//trim(l1)//'e16.8)'
!
!    open (1, file='.\outputs\par_end.txt', status='unknown')
!
!    do k= 1,np
!        do i = 1,n
!            write(1,fmt) (m_uc((k-1)*n+i,j),j=1,n_e)
!        end do
!    end do
!    close(1)
!    
!    o_nd = 0.0d0
!    do e = 1, n_e ! ========================================================================================================
!        ! write the model parameters in the model file
!        !filename='inc\porosity.inc'
!        !l1='poro'
!        !call w_inc_file (m_uc(n+1:2*n,e),n,filename,l1)             ! porosity include file
!
!        filename='inc\permx.inc'
!        l1='permx'
!        call w_inc_file (exp(m_uc(1:n,e)),n,filename,l1)             ! porosity include file
!        ! run the forward model (eclipse)
!103     call run_ecl
!        info = 0
!        call success_ecl(info)
!        if (info .ne. 0) then
!            print *, 'simulation run unsuccessful'
!            goto 103
!        end if
!        ! read the observated data
!        call read_observed_data(n_d+n_de,nt,d_raw)
!        do i=1,nd
!            do j=1,nta
!                k = (i-1)*nta + j
!                df(k,e) = d_raw(j,i+2)
!                if (df(k,e) < 0.0d0) df(k,e) = 0.0d0
!            end do
!        end do
!        do i=1,nd
!            do j=1,nt
!                k = (i-1)*nt + j
!                df_op(k,e) = d_raw(j,i+2)
!            end do
!        end do
!        
!        if (n_de > 0) then
!            do i=1,n_de
!                d_mon((i-1)*nt+1:i*nt,e) = d_raw(1:nt,n_d+i)
!            end do
!        end if
!        
!        tmp_d(1:,1) = df(1:,e) - d_obs(1:,1)
!        do i=1,nd*nta
!           o_nd = o_nd + tmp_d(i,1)**2 / s_d(i)**2 / (n_e * nd * nta)
!        end do
!    end do
!    
!    write(*,'(a26,e16.8)') "objective function value: ", o_nd
!    write(unit_msg,'(10e16.8)') (err_par(k),k=1,np),o_nd,alpha_i
!    write(l2,'(i)') n_e+2
!    fmt = '('//trim(l2)//'e16.8)'
!    filename='.\outputs\output_final.txt'
!    open (1, file=filename, status='unknown')
!
!    do i = 1,nd
!        do j = 1, nt
!            k = (i-1)*nt+j
!            write(1,fmt) d_raw(j,1),d_op(k,1), (df_op(k,e),e=1,n_e)
!        end do
!    end do
!    do i = 1,n_de
!        do j = 1,nt
!           k =(i-1)*nt+j
!           write(1,fmt) d_raw(j,1),d_mon(k,n_e+1), (d_mon(k,e),e=1,n_e)
!        end do
!    end do
!    close(1)
!
!
!
!
!    end program esmda
!
