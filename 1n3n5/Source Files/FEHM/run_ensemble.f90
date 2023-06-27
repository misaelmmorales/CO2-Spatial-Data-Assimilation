subroutine run_ens(N,N_e,Nd,Nt,Nta,m_uc,df,a)
    USE global_variables,ONLY: ndata,Na,casename,generic_dir_name,modelname
    implicit none
    real,external                            :: gasdev
    integer, intent(in) :: N                 ! is the number of parameters
    integer, intent(in) :: N_e               ! number of ensemble members
    integer, intent(in) :: Nd                ! number of observed data
    integer, intent(in) :: Nt                ! number of timesteps (assimiliation + prediction)
    integer, intent(in) :: Nta               ! number of assimilation timesteps
    integer             :: a                 ! if ==0, N_e must be 1, to run the simulator for the true model
    double precision, dimension(N,N_e)       :: m_uc ! is the model parameters
    double precision, dimension(Ndata,N_e)  :: df   ! ensemble response
    double precision, dimension(Nt,Nd+2) :: d_raw                    ! 
    integer            :: i,j,k, info,e
    double precision                         :: tmp,tmp2
    character*400      :: FILENAME, L1,L2,FMT
    logical            :: del_files, rerun
    del_files = 1
    rerun     = 0
    df = 0.0d0
    if (a == 0 ) then
        if (rerun) then
            del_files = 0
        end if
        call make_dir(a,del_files)
    end if
    ! RUN the ensemble from time zero  ======================================================================================
    do e = 1, N_e !  ========================================================================================================
        ! Run the forward model (eclipse)
        if (a==0 .and. rerun) then

            write(L1,'(I4.4)') e

            filename = 'outputs_'//trim(modelname)//'\out_000\'//trim(casename)//trim(L1)//'.RSM'
            call READ_OBSERVED_DATANEW(Nd,Nt,d_raw,filename)
            ! extract observed and monitored data
            call extract_obs_mon_data(d_raw,df(1:,e))
        else
            CALL W_INC_FILE (e)             ! INCLUDE FILE
102         call RUN_ECL
            info = 0
            call Success_Ecl(info)
            if (info .NE. 0) then
                print *, 'Simulation Run Unsuccessful'
                goto 102
            end if
            Write (*,*) 'Simulation #', e, '... done'
            ! read the observated data
            filename = trim(generic_dir_name) // '\'//trim(casename)//'.RSM'
            call READ_OBSERVED_DATANEW(Nd,Nt,d_raw,filename)
            ! extract observed and monitored data
            call extract_obs_mon_data(d_raw,df(1:,e))
            if (a==0) then
                call save_outputs(e,a)
            end if
        end if
    end do !=================================================================================================================
        call write_outputs(Ndata,N_e,df,a)  
end subroutine
subroutine run_prediction(N,N_e,Nd,Nt,m_uc)
    USE global_variables,ONLY: ndata,Na,casename,cwd,modelname,generic_dir_name
    implicit none
    real,external                            :: gasdev
    integer, intent(in) :: N                 ! is the number of parameters
    integer, intent(in) :: N_e               ! number of ensemble members
    integer, intent(in) :: Nd                ! number of observed data
    integer, intent(in) :: Nt                ! number of timesteps (assimiliation + prediction)
    double precision, dimension(N,N_e)       :: m_uc ! is the model parameters
    double precision, dimension(Nt,Nd+2) :: d_raw                    ! 
    integer            :: i,j,k, info,e,fhf_id
    double precision                         :: tmp,tmp2
    character*400      :: FILENAME, L1,L2,FMT
    filename = trim(cwd) // '\outputs_'//trim(modelname)//'\'//trim(casename)//'.fhf'
    fhf_id = 290387
    open(fhf_id,FILE=trim(filename),STATUS='UNKNOWN')
    ! RUN the ensemble from time zero  ======================================================================================
    do e = 1, N_e !  ========================================================================================================
        ! Run the forward model (eclipse)
        CALL W_INC_FILE (e)             ! INCLUDE FILE
102         call RUN_ECL
        info = 0
        call Success_Ecl(info)
        if (info .NE. 0) then
            print *, 'Simulation Run Unsuccessful'
            goto 102
        end if
        Write (*,*) 'Simulation #', e, '... done'
        ! read the observated data
        filename = trim(generic_dir_name) // '\'//trim(casename)//'.RSM'
        call READ_OBSERVED_DATANEW(Nd,Nt,d_raw,filename)
        call write_fhffile(Nd,Nt,d_raw,e,fhf_id)
    end do !=================================================================================================================
    close(fhf_id)
end subroutine
    
    
subroutine save_outputs(e,i)
    use global_variables, ONLY: generic_dir_name, casename,cwd,modelname
    implicit none
    integer         :: e,i
    character*20    :: L2,L1
    character*1000 :: command
    if (i<10) then
        write(L2,'(I1)') i
        L2  = '00'//trim(L2)
    elseif (i<100) then
        write(L2,'(I2)') i
        L2  = '0'//trim(L2)
    else
        write(L2,'(I3)') i
    end if
    command = 'MOVE /Y "'//trim(cwd)//'\'//trim(generic_dir_name)//'\'//trim(casename)//'.RSM" "'//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'" >nul'
    call system(trim(command))
    if (e<10) then
        write(L1,'(I1)') e
        L1  = '000'//trim(L1)
    elseif (e<100) then
        write(L1,'(I2)') e
        L1  = '00'//trim(L1)
    elseif (e<1000) then
        write(L1,'(I3)') e
        L1  = '0'//trim(L1)
    else
        write(L1,'(I4)') e
    end if
    command = 'ren "'//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'\'//trim(casename)//'.RSM" "'//trim(casename)//trim(L1)//'.RSM"'
    call system(trim(command))
end subroutine
   
subroutine make_dir(i,file_del)
    use global_variables, ONLY: cwd,modelname
    implicit none
    integer         :: i
    character*20    :: L2
    character*1000 :: command
    logical :: dir_e,file_del
    if (i<10) then
        write(L2,'(I1)') i
        L2  = '00'//trim(L2)
    elseif (i<100) then
        write(L2,'(I2)') i
        L2  = '0'//trim(L2)
    else
        write(L2,'(I3)') i
    end if
    command  = trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)
    inquire(DIRECTORY=trim(command), exist=dir_e)

    if ( dir_e ) then
        if (file_del) then
        command = 'del '//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'\*.RSM'
        call system(trim(command))
        end if
    else
        command = 'mkdir '//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)
        call system(trim(command))
    end if
    
end subroutine
subroutine read_initens(Ne,Np, m_uc, dir_model)
    use global_variables, ONLY: NG, ACTLIST,M_ALL,nact,np_g, par_name, par_limit,modelname,np_gl
    implicit none
    integer :: Ne,Np
    double precision, dimension(Np,Ne+1) :: m_uc
    double precision, dimension(Np,1) :: mt
    character*400 :: dir_model,command, L2,chline
    integer :: i, j,k
    LOGICAL                                         :: FEXIST
    double precision                                :: tmp_min, tmp_max,tmp_min1, tmp_max1
        
    !! reading ensemble
    do i=1,Ne
        if (i<10) then
            write(L2,'(I1)') i
            !L2  = '00'//trim(L2)
        elseif (i<100) then
            write(L2,'(I2)') i
            !L2  = '0'//trim(L2)
        else
            write(L2,'(I3)') i
        end if
        do k = 1, np_g
            command = trim(dir_model)//'\ensemble\'//trim(modelname)//'\'//trim(par_name(1,k))//trim(L2)//'.inc'
            open(105,file=trim(command),status='old')
            do while (.not. eof(105))
                read(105,*) chline
                if (trim(chline)==trim(par_name(2,k))) then
                    read(105,*) (m_all((k-1)*Ng+j,i),j=1,NG)
                    exit
                end if
            end do
            close(105)
        end do
    end do
    do i=1,Nact
        do k = 1,Np_g
            if (par_name(2,k)(1:4) == 'PERM') then
                m_uc((k-1)*Nact + i,1:Ne) = LOG(m_all((k-1)*Ng + ACTLIST(i),1:Ne))
            else
                m_uc((k-1)*Nact + i,1:Ne) = m_all((k-1)*Ng + ACTLIST(i),1:Ne)
            end if
        end do
    end do
       
    !calc the maximum and minimum of each parameter
    do k = 1,Np_g
        tmp_min = m_uc((k-1)*Nact+1,1)
        tmp_max = m_uc((k-1)*Nact+1,1)
        do i = 1, Ne
            tmp_min1 = minval(m_uc((k-1)*Nact+1:k*Nact,i))
            tmp_max1 = maxval(m_uc((k-1)*Nact+1:k*Nact,i))
            if (tmp_min1 < tmp_min)  tmp_min = tmp_min1
            if (tmp_max1 > tmp_max)  tmp_max = tmp_max1
        end do
        par_limit(1,k) = tmp_min
        par_limit(2,k) = tmp_max
    end do
    
    if (np_gl>0) then
        command = trim(dir_model)//'\ensemble\'//trim(modelname)//'\'//trim(par_name(1,np_g+1))//'.dat'
        open(105,file=trim(command),status='old')
        do i=1,Ne
            read(105,*) (m_all(np_g*Ng + j,i),j=1,np_gl)
            do j=1,np_gl
                m_uc(np_g*Nact+j,i) = m_all(np_g*Ng + j,i)
            end do
        end do
        close(105)
        do i=1,np_gl
            par_limit(1,np_g+i)= minval(m_uc(np_g*Nact+i,1:Ne))
            par_limit(2,np_g+i)= maxval(m_uc(np_g*Nact+i,1:Ne))
        end do
    end if
    command = trim(dir_model)//'\outputs_'//trim(modelname)//'\limits.txt'
    open(105,file=trim(command),status='unknown')
    do k = 1,Np_g+np_gl
        write(105,'(2f16.8)') par_limit(1,k),par_limit(2,k)
    end do
    close(105)
end subroutine read_initens