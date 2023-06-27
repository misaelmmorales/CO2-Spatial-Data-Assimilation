program fehm_esmda
    USE global_variables
    implicit none
    real,external                                                :: gasdev
    double precision, dimension(:,:), allocatable                :: df,df_perm,df_new                  ! observed data for ensemble members
    double precision, dimension(:,:), allocatable                :: m_uc
    double precision, dimension(:,:), allocatable                :: E_OND
    integer                                                      :: i,j,k,e,a, dumy=1642474681,info,kk,N_a,dum_uc
    character*400                                                :: FILENAME, L1,L2,FMT, casename_temp, fehmNname_temp
    double precision                                             :: tmp,alpha_i
    LOGICAL                                                      :: FEXIST,ensemble
    character*400                                                :: command, dir_model
    integer                                                      :: UNIT_MSG, N,UNIT_ALP, Nt_pred
    double precision                                             :: O_Nd,etta
    logical                                                      :: del_files, rerun
        

    
    
    open(1,FILE='.\INPUT.dat',STATUS='UNKNOWN')
    read(1,*) modelname
    close(1)
    filename = '.\'//trim(modelname)//'_INPUT.dat'
    call read_data(filename)
    dir_model = cwd
    ensemble = .true.
    UNIT_MSG = 110
    UNIT_ALP = 114
    N = Np_g;
    Nt_pred = Nt
    Nt      = Nta
    command = trim(dir_model)//'\outputs_'//trim(modelname)//'\MSG.txt'
    open(UNIT_MSG,file=trim(command), status='unknown')
    command = trim(dir_model)//'\outputs_'//trim(modelname)//'\alpha.dat'
    open(UNIT_ALP,FILE=trim(command), status = 'unknown')
    command = trim(dir_model)//'\outputs_'//trim(modelname)//'\seed.dat'
    open(2,file=trim(command),status='unknown')
    READ (2,*,IOSTAT = info)  dumy ! info > 0 something is wrong, info <0 end of file
    close(2)
    command = trim(dir_model)//'\outputs_'//trim(modelname)//'\seed.dat'
    open(2,file=trim(command),status='unknown')
    write (2,*)  dumy ! info > 0 something is wrong, info <0 end of file
    close(2)
    a=0
    allocate(df(Ndata,N_e), m_uc(Np,N_e + 1),E_OND(N_e,1))
    
    ! read the initial ensemble of model parameters
    call read_initens(N_e,Np,m_uc(1:Np,1:N_e+1),dir_model)
    call write_param(Np,N_e,m_uc(1:Np,1:N_e),a)
    
    ! Create folder for ensemble simulation runs
    call make_dir_modelrun()
    CALL CopyTplFIles()  ! copy template files to each simulation run folder
    
    ! Create folder for output at DA=0
    rerun=0
    if (a == 0) then
        if (rerun) then
            del_files = 0
        end if
        call make_dir(a,del_files,N_e)
    end if    
    
    ! Generate N_e Fehm_exe for simulation runs
    !call fehm_exe_gen()
    
    ! Predictions based on prior models
    if (prediction_prior) then
        casename_temp=trim(casename)
        fehmNname_temp= trim(fehmNname)
        casename = trim(casename)//'_prior'
        fehmNname= trim(fehmNname)//'_prior'
        ! creating FEHM bat file
        call FEHM_BAT_GEN()
        call run_prediction(Np,N_e,Nd,Nt_pred,m_uc(1:Np,1:N_e),a)
        
        casename = trim(casename_temp)
        fehmNname= trim(fehmNname_temp)
    end if

    ! run the initial ensemble
    call FEHM_BAT_GEN()   ! creating FEHM bat file   
    call run_ens(Np,N_e,Nd,Nt,Nta,m_uc(1:Np,1:N_e),df,a)
    

    ! If conditioning to permeability
    if (cond_perm) then
        filename = '.\'//trim(modelname)//'_INPUT.dat'
        call read_in_perm(filename)
        Ndata_new=Ndata+N_perm        
        allocate(d_obs_new(Ndata_new,1))
        allocate(df_new(Ndata_new,N_e),df_perm(N_perm,N_e))
        allocate(s_d_new(Ndata_new))
        
        d_obs_new(1:Ndata,:)=d_obs        
        d_obs_new(Ndata+1:Ndata_new,1)=d_perm
        df_new(1:Ndata,:)=df
        do i=1,N_e
            df_perm(:,i)=d_perm   ! values for all prior ensemble are the same
        end do
        df_new(Ndata+1:Ndata_new,:)=df_perm
        s_d_new(1:Ndata)=s_d
        s_d_new(Ndata+1:Ndata_new)=s_d_perm
    else
        Ndata_new=Ndata
        d_obs_new=d_obs
        df_new=df
        s_d_new=s_d
    end if
        
    call obj_calc(Ndata_new,N_e,d_obs_new,df_new,s_d_new,O_ND,E_OND)
    call write_objectives(N_e,E_OND,a)
    write(unit_msg,'(I3,10E20.8)') a,o_nd
    alpha_sum = 0.0d0
    
    if ( method==2 ) then          ! adaptive
                nolast = .true.
                do
                    a= a + 1
                    alpha_i = O_ND/8.0d0
                    if (alpha_i < 1.0d0) alpha_i = 1.0d0/ (1.0d0-alpha_sum)
                    if (alpha_sum + 1/alpha_i > 1.0d0) alpha_i = 1.0d0/ (1.0d0-alpha_sum)
                    ! ESMDA UPDATE
                    call ES_UPDATE_ADAPT_DUC(N_e,Ndata,Np,d_obs,df,m_uc(1:Np,1:N_e),s_d,alpha_i,svd_imp,dumy,local)
                    ! check the limits of the updated parameters
                    call check_limit(Np,N_e,m_uc(1:Np,1:N_e))
        
                    ! write the updated parameters
                    call write_param(Np,N_e,m_uc(1:Np,1:N_e),a)
                    write(UNIT_ALP,'(10E20.8)') alpha_i
                    
                    ! run the ensemble
                    call run_ens(Np,N_e,Nd,Nt,Nta,m_uc(1:Np,1:N_e),df,a)
        
                    alpha_sum = alpha_sum + 1.0d0 / alpha_i
                    call obj_calc(Ndata,N_e,d_obs,df,s_d,O_ND,E_OND)
                    write(unit_msg,'(I3,10E20.8)') a,o_nd,alpha_i,alpha_sum
                    if (alpha_sum == 1.0d0) exit    
                end do
    elseif ( method==3 ) then          ! Iglesias method
                etta = ndata
                etta = sqrt(etta)
                nolast = .true.
                do
                    a= a + 1
                    alpha_i = 1.0d0
                    ! ESMDA UPDATE
                    call ES_UPDATE_ADAPT_IGL(N_e,Ndata,Np,d_obs,df,m_uc(1:Np,1:N_e),s_d,alpha_i,svd_imp,dumy,local)
                    call check_limit(Np,N_e,m_uc(1:Np,1:N_e))
                    ! write the updated parameters
                    call write_param(Np,N_e,m_uc(1:Np,1:N_e),a)
                    write(UNIT_ALP,'(10E20.8)') alpha_i
                    ! run the ensemble
                    call run_ens(Np,N_e,Nd,Nt,Nta,m_uc(1:Np,1:N_e),df,a)
                    call check_conv(Ndata,N_e,s_d,d_obs,df,tau,etta,nolast)
                    alpha_sum = alpha_sum + 1.0d0 / alpha_i
                    call obj_calc(Ndata,N_e,d_obs,df,s_d,O_ND,E_OND)
                    write(unit_msg,'(I3,10E20.8)') a,o_nd,alpha_i,alpha_sum
                    if (.not. nolast) then
                        write(unit_msg,'(A)') "Converged"
                        exit    
                    end if
                end do
                
    elseif (method == 4) then
                etta = ndata
                etta = sqrt(etta)
                nolast = .true.
                do  
                    a = a+1
                    ! ESMDA UPDATE
                    call ES_ADAP_MINE(N_e,Ndata,Np,d_obs,df,m_uc(1:Np,1:N_e),s_d,alpha_i,svd_imp,dumy,local)
                   
                    write(UNIT_ALP,'(10E20.8)') alpha_i
                    ! check the limits of the updated parameters
                    call check_limit(Np,N_e,m_uc(1:Np,1:N_e))
        
                    ! write the updated parameters
                    call write_param(Np,N_e,m_uc(1:Np,1:N_e),a)
        
                    ! run the ensemble
                    call run_ens(Np,N_e,Nd,Nt,Nta,m_uc(1:Np,1:N_e),df,a)
                    
                    call check_conv(Ndata,N_e,s_d,d_obs,df,tau,etta,nolast)
                    alpha_sum = alpha_sum + 1.0d0 / alpha_i
                    call obj_calc(Ndata,N_e,d_obs,df,s_d,O_ND,E_OND)
                    write(unit_msg,'(I3,10E20.8)') a,o_nd,alpha_i,alpha_sum
                    if ((.not. nolast) .or. (abs(alpha_sum-1.0d0)<0.0001)) then
                        write(unit_msg,'(A)') "Converged"
                        exit    
                    end if
                end do  
    else
                do a=1,Na
                    alpha_i = alpha(a)
                    ! ESMDA UPDATE
                    call ES_UPDATE4(N_e,Ndata_new,Np,d_obs_new,df_new,m_uc(1:Np,1:N_e),s_d_new,alpha_i,svd_imp,dumy,local)
                    if (a==1 .and. method == 1) then     ! calculate the infation factor   
                        alpha(1) = alpha_i
                        call find_alpha_geometric(Na,alpha)
                        !command = trim(dir_model)//'\outputs_'//trim(modelname)//'\alpha.dat'
                        !open(UNIT_ALP,FILE=trim(command), status = 'unknown')
                        write(UNIT_ALP,'(E20.8)') (alpha(k),k=1,Na)
                        !close(UNIT_ALP);
                    end if
        
                    ! check the limits of the updated parameters
                    call check_limit(Np,N_e,m_uc(1:Np,1:N_e))
        
                    ! write the updated parameters
                    call write_param(Np,N_e,m_uc(1:Np,1:N_e),a)
                            
                    ! run the ensemble
                    call run_ens(Np,N_e,Nd,Nt,Nta,m_uc(1:Np,1:N_e),df,a)
                    
                    if (cond_perm) then
                        df_new(1:Ndata,:)=df
                        do i=1,N_e
                            do j=1,N_perm
                                df_perm(j,i)=m_uc(perm_index(j),i)
                            end do
                        end do
                        df_new(Ndata+1:Ndata_new,:)=df_perm  
                    else
                        df_new=df
                    end if                                   
        
                    alpha_sum = alpha_sum + 1.0d0 / alpha_i
                    call obj_calc(Ndata_new,N_e,d_obs_new,df_new,s_d_new,O_ND,E_OND)
                    write(unit_msg,'(I3,10E20.8)') a,o_nd,alpha_i,alpha_sum
                end do
                a = a-1
    end if
    !!!!!!!!command = trim(dir_model)//'\outputs_'//trim(modelname)//'\param_1_250.dat'
    !!!!!!!!open(2,file=trim(command),status='unknown')
    !!!!!!!!do i=1,Np
    !!!!!!!!    read(2,*) (m_all(i,j),j=1,N_e)
    !!!!!!!!end do
    !!!!!!!!close(2)
    close(unit_msg)
    close(UNIT_ALP)
    call write_objectives(N_e,E_OND,a)
    casename = trim(casename)//'_post'
    fehmNname= trim(fehmNname)//'_post'
    if (prediction_post) then
        ! creating FEHM bat file
        call FEHM_BAT_GEN()
        call run_prediction(Np,N_e,Nd,Nt_pred,m_uc(1:Np,1:N_e),a)
    end if
    command = trim(dir_model)//'\seed.dat'
    open(2,file=trim(command),status='unknown')
    write(2,*) dumy
    close(2)
end program fehm_esmda
    