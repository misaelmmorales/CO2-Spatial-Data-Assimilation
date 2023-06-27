subroutine run_ens(N,N_e,Nd,Nt,Nta,m_uc,df,a)
    USE global_variables,ONLY: N_processor,ndata,Na,casename,generic_dir_name,modelname,DataAssim_type
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
    double precision, dimension(Nt,Nd+1) :: d_raw                    ! 
    integer            :: i,j,k, info,e
    double precision                         :: tmp,tmp2
    character*400      :: FILENAME, L1,L2,FMT,filename1,filename2,filename3
    logical            :: del_files, rerun
    del_files = 1
    rerun     = 0
    df = 0.0d0
    if (a == Na) then
        if (rerun) then
            del_files = 0
        end if
        call make_dir(a,del_files,N_e)
    end if
    ! RUN the ensemble from time zero  ======================================================================================
    call W_INC_FILE ()
    call omp_set_num_threads(N_processor)
    
    !$OMP parallel
    
    !$OMP DO    
    do e = 1, N_e !  ========================================================================================================
        call RUN_FEHM(e)
        Write (*,*) 'Simulation #', e, '... done'
    end do !=================================================================================================================
    !$OMP End DO
    
    !$OMP end parallel    
    
    Do e = 1, N_e
        if (e<10) then
            write(L2,'(I1)') e
            L2  = '000'//trim(L2)
        elseif (e<100) then
            write(L2,'(I2)') e
            L2  = '00'//trim(L2)
        elseif (e<1000) then
            write(L2,'(I3)') e
            L2  = '0'//trim(L2)
        else
            write(L2,'(I4)') e
        end if
        
        if (DataAssim_type==1) then
            ! read the observated data
            filename1 = trim(generic_dir_name) //'\SimFold\'//'Ensemble_'//trim(L2)//'\'//'run_presCO2.his'
            filename2 = trim(generic_dir_name) //'\SimFold\'//'Ensemble_'//trim(L2)//'\'//'run_co2sl.his'
            call READ_OBSERVED_DATANEW(Nd,Nt,d_raw,filename1,filename2)
            ! extract observed and monitored data
            call extract_obs_mon_data(d_raw,df(1:,e))
        elseif (DataAssim_type==2) then
            filename1 = trim(generic_dir_name) //'\SimFold\'//'Ensemble_'//trim(L2)//'\'//'run.00013_sca_node.dat'
            filename2 = trim(generic_dir_name) //'\SimFold\'//'Ensemble_'//trim(L2)//'\'//'run.00037_sca_node.dat'
            filename3 = trim(generic_dir_name) //'\SimFold\'//'Ensemble_'//trim(L2)//'\'//'run.00061_sca_node.dat'
            call READ_Spatial_OBSERVED_DATANEW(Nd,Nt,d_raw,filename1,filename2,filename3)
            ! extract observed and monitored data
            call extract_obs_mon_data(d_raw,df(1:,e))
        end if        

        if (a==0) then
            call save_outputs(e,a)
        end if
    End do
    
end subroutine
    
subroutine run_prediction(N,N_e,Nd,Nt,m_uc,a)
    USE global_variables,ONLY: N_processor,ndata,Na,casename,cwd,modelname,generic_dir_name,DataAssim_type
    implicit none
    real,external                            :: gasdev
    integer, intent(in) :: N                 ! is the number of parameters
    integer, intent(in) :: N_e               ! number of ensemble members
    integer, intent(in) :: Nd                ! number of observed data
    integer, intent(in) :: Nt                ! number of timesteps (assimiliation + prediction)
    integer             :: a                 ! if ==0, N_e must be 1, to run the simulator for the true model
    double precision, dimension(N,N_e)       :: m_uc ! is the model parameters
    double precision, dimension(Nt,Nd+1) :: d_raw                    ! 
    integer            :: i,j,k, info,e,fhf_id
    double precision                         :: tmp,tmp2
    character*400      :: FILENAME, L1,L2,FMT,filename1,filename2,filename3
    
    ! RUN the ensemble from time zero  ======================================================================================
    Write (*,*) ' '  
    Write (*,*) '--Prediction Run--'
    
    call W_INC_FILE ()
    call omp_set_num_threads(N_processor)
    
    !$OMP parallel
    
    !$OMP DO    
    do e = 1, N_e !  ========================================================================================================
        call RUN_FEHM(e)
        Write (*,*) 'Simulation #', e, '... done'
    end do !=================================================================================================================
    !$OMP End DO
    
    !$OMP end parallel
    
    filename = trim(cwd) // '\outputs_'//trim(modelname)//'\'//trim(casename)//'.fhf'
    fhf_id = 290387
    open(fhf_id,FILE=trim(filename),STATUS='UNKNOWN')
    
    Do e =1, N_e
        
        if (e<10) then
            write(L2,'(I1)') e
            L2  = '000'//trim(L2)
        elseif (e<100) then
            write(L2,'(I2)') e
            L2  = '00'//trim(L2)
        elseif (e<1000) then
            write(L2,'(I3)') e
            L2  = '0'//trim(L2)
        else
            write(L2,'(I4)') e
        end if
        
        !! read the observated data
        !filename1 = trim(generic_dir_name) //'\SimFold\'//'Ensemble_'//trim(L2)//'\'//'run_presCO2.his'
        !filename2 = trim(generic_dir_name) //'\SimFold\'//'Ensemble_'//trim(L2)//'\'//'run_co2sl.his'
        !call READ_OBSERVED_DATANEW(Nd,Nt,d_raw,filename1,filename2)
        !call write_fhffile(Nd,Nt,d_raw,e,fhf_id)
        !if (a==0 .or. a==Na) then
        !    call save_sca_outputs(e,a,Nt)
        !end if
        
        if (DataAssim_type==1) then
            ! read the observated data
            filename1 = trim(generic_dir_name) //'\SimFold\'//'Ensemble_'//trim(L2)//'\'//'run_presCO2.his'
            filename2 = trim(generic_dir_name) //'\SimFold\'//'Ensemble_'//trim(L2)//'\'//'run_co2sl.his'
            call READ_OBSERVED_DATANEW(Nd,Nt,d_raw,filename1,filename2)
            call write_fhffile(Nd,Nt,d_raw,e,fhf_id)
        elseif (DataAssim_type==2) then
            filename1 = trim(generic_dir_name) //'\SimFold\'//'Ensemble_'//trim(L2)//'\'//'run.00013_sca_node.dat'
            filename2 = trim(generic_dir_name) //'\SimFold\'//'Ensemble_'//trim(L2)//'\'//'run.00037_sca_node.dat'
            filename3 = trim(generic_dir_name) //'\SimFold\'//'Ensemble_'//trim(L2)//'\'//'run.00061_sca_node.dat'
            call READ_Spatial_OBSERVED_DATANEW(Nd,Nt,d_raw,filename1,filename2,filename3)
            !call write_fhffile(Nd,Nt,d_raw,e,fhf_id)
        end if
        
        if (a==0 .or. a==Na) then
            call save_sca_outputs(e,a,Nt)
        end if

    End do
    
    close(fhf_id)
    
end subroutine

subroutine save_sca_outputs(e,i,Nsca)
    use global_variables, ONLY: generic_dir_name, casename,cwd,modelname
    implicit none
    integer         :: e,i,sca
    character*20    :: L2,L1,L3
    character*1000 :: command
    integer, intent(in) :: Nsca                ! number of timesteps (assimiliation + prediction)
    
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
    
    if (i<10) then
        write(L2,'(I1)') i
        L2  = '00'//trim(L2)
    elseif (i<100) then
        write(L2,'(I2)') i
        L2  = '0'//trim(L2)
    else
        write(L2,'(I3)') i
    end if
    
    do sca = 1, Nsca+1  
        if (sca==1 .and. sca==13 .and. sca==37 .and. sca==61 .and. sca==121 .and. sca==181) then
        
            if (sca<10) then
                write(L3,'(I1)') sca
                L3  = '0000'//trim(L3)
            elseif (sca<100) then
                write(L3,'(I2)') sca
                L3  = '000'//trim(L3)
            else
                write(L3,'(I3)') sca
                L3  = '00'//trim(L3)
            end if
        
            !command = 'MOVE /Y "'//trim(cwd)//'\'//trim(generic_dir_name)//'\'//'run_presCO2.his" "'//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'" >nul'
            command = 'MOVE /Y "'//trim(cwd)//'\'//trim(generic_dir_name)//'\SimFold\'//'Ensemble_'//trim(L1)//'\'//'run.'//trim(L3)//'_sca_node.dat" "'//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'\Ensemble_'//trim(L1)//'" >nul'
            call system(trim(command))
        end if
    end do
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
    
    !command = 'MOVE /Y "'//trim(cwd)//'\'//trim(generic_dir_name)//'\'//trim(casename)//'.RSM" "'//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'" >nul'
    command = 'MOVE /Y "'//trim(cwd)//'\'//trim(generic_dir_name)//'\SimFold\'//'Ensemble_'//trim(L1)//'\'//'run_presCO2.his" "'//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'" >nul'
    call system(trim(command))
    command = 'MOVE /Y "'//trim(cwd)//'\'//trim(generic_dir_name)//'\SimFold\'//'Ensemble_'//trim(L1)//'\'//'run_co2sl.his" "'//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'" >nul'
    call system(trim(command))
    command = 'MOVE /Y "'//trim(cwd)//'\'//trim(generic_dir_name)//'\SimFold\'//'Ensemble_'//trim(L1)//'\'//'run.00013_sca_node.dat" "'//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'" >nul'
    call system(trim(command))

    command = 'ren "'//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'\'//'run_presCO2.his" "'//'run_presCO2_'//trim(L1)//'.his"'
    call system(trim(command))
    command = 'ren "'//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'\'//'run_co2sl.his" "'//'run_co2sl_'//trim(L1)//'.his"'
    call system(trim(command))
    command = 'ren "'//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'\'//'run.00037_sca_node.dat" "'//'run.00013_sca_node_'//trim(L1)//'.dat"'
    call system(trim(command))
end subroutine
   
subroutine make_dir(i,file_del,N_e)
    use global_variables, ONLY: cwd,modelname
    implicit none
    integer         :: e,i
    character*20    :: L1,L2
    character*1000 :: command
    logical :: dir_e,file_del   
    integer, intent(in) :: N_e               ! number of ensemble members
    
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
            !command = 'del '//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'\*.his'
            command = 'del '//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'\'
            call system(trim(command))
        end if
        do e = 1, N_e
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
            
            command = 'mkdir '//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'\Ensemble_'//trim(L1)
            call system(trim(command))
        end do
    else
        do e = 1, N_e
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
            
            command = 'mkdir '//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'\Ensemble_'//trim(L1)
            call system(trim(command))
        end do
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
            if (par_name(2,k)(1:4) == 'perm') then
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