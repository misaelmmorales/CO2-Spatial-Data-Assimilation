subroutine ES_UPDATE4(N_e,N_d,Np,d_obs,df,m_uc,s_d,alpha_i,svd_imp,dumy,local)
    USE global_variables,ONLY: Nd,Loc_indx,Nact,Np_g,rho_md,alpha_sum
    implicit none
    real,external                                       :: gasdev
    integer                                             :: N_e ! number of ensemble members
    integer                                             :: N_d  ! number of data
    integer                                             :: Np  ! number of parameters
    double precision, dimension(N_d,1)                  :: d_obs
    double precision, dimension(N_d,N_e)                :: df
    double precision, dimension(Np,N_e)                 :: m_uc
    double precision, dimension(N_d)                    :: s_d
    double precision, intent(in)                        :: alpha_i,svd_imp
    integer                                             :: dumy  ! number of parameters
    logical                                             :: local
    double precision, dimension(:,:), allocatable       :: df_b, mf_b,CDD, tmp_d,tmp_m, d_uc,DD,DD2,DM,CMD
    double precision                                    :: beta,tmp
    integer                                             :: i,j,k,e,p_max
    double precision, dimension(1,1)                    :: tmp_a
    character*1                                         :: transa, transb
    double precision                                    :: const_a, const_b
    transa = 'N'
    transb = 'T'
    allocate(df_b(N_d,1),mf_b(Np,1),CDD(N_d,N_d),tmp_d(N_d,1),tmp_m(Np,1),d_uc(N_d,N_e),DD(N_d,N_e),DM(Np,N_e),DD2(N_d,N_e),CMD(Np,N_d))
    
    !Calculation of MEAN of the MF and DF

    do i=1,N_d
        df_b(i,1) = sum(df(i,1:N_e)) / N_e
    end do
    do i=1,Np
        mf_b(i,1) = sum(m_uc(i,1:N_e)) / N_e
    end do
    
    !    - Calculate Cf_MD  using matrix multiplication
    
    DD = 0.0d0
    DM = 0.0d0
    DD2 = 0.0d0
    
        
    do e=1,N_e
        DM(1:,e) = m_uc(1:,e)-mf_b(1:,1)
        DD(1:,e) = df(1:,e)-df_b(1:,1)
        do k=1,N_d
            DD2(k,e) = (df(k,e) - df_b(k,1))/s_d(k)
        end do        
    end do

    DD =DD /sqrt(N_e-1.0d0)
    DD2=DD2/sqrt(N_e-1.0d0)
    DM =DM /sqrt(N_e-1.0d0)
    p_max = 0
    call subspace_adap(DD2,N_d,N_e,s_d,CDD,alpha_i,svd_imp,p_max)
    const_a = 1.0d0
    const_b = 0.0d0
    call dgemm('T', 'N', N_d, N_e, N_d, const_a, CDD, N_d, DD, N_d, const_b, DD2, N_d)
    print *, 'CDD*DD'
    call dgemm(transa, transb, Np, N_d, N_e, const_a, DM, Np, DD2, N_d, const_b, CMD, Np)
    print *, 'Pseudo Inverse done.'
    deallocate(DM,DD,DD2,CDD)   
    !open(1096,file='.\CMD.txt', status='unknown')
    !do e=1,np
    !    write(1096,'(792E20.12)')(cmd(e,k),k=1,N_d)
    !end do
    !close(1096)
    do e=1,N_e
        do k=1,N_d
            tmp = gasdev(dumy) ! tmp ~ N(0,1)
            d_uc(k,e) = d_obs(k,1) + sqrt(alpha_i)*s_d(k)*tmp
        end do
    end do
    
    if (local) then
        do k=1,Nd
            do i=1,Np_g
                do j = Loc_indx(k,1),Loc_indx(k+1,1)-1
                    CMD((i-1)*nact+1:i*nact,j) = rho_md(1:Nact,k)*CMD((i-1)*nact+1:i*nact,j)
                end do
            end do
        end do
    end if

    
    
    do e=1,N_e
        tmp_d(1:,1) = d_uc(1:,e) - df(1:,e)
        k=1
        const_b = 0.0d0
        call dgemv(transa, Np, N_d, const_a, CMD, Np, tmp_d, k, const_b, tmp_m, k)
        m_uc(1:,e) = tmp_m(1:,1) + m_uc(1:,e)
        print *, 'Ensemble #', e,'Updated.'
    end do
    
    deallocate(df_b,mf_b,tmp_d,tmp_m,d_uc,CMD)
end subroutine ES_UPDATE4    
subroutine ES_ADAP_MINE(N_e,N_d,Np,d_obs,df,m_uc,s_d,alpha_i,svd_imp,dumy,local)
    USE global_variables,ONLY: Nd,Loc_indx,Nact,Np_g,rho_md,alpha_sum
    implicit none
    real,external                                       :: gasdev
    integer                                             :: N_e ! number of ensemble members
    integer                                             :: N_d  ! number of data
    integer                                             :: Np  ! number of parameters
    double precision, dimension(N_d,1)                  :: d_obs
    double precision, dimension(N_d,N_e)                :: df
    double precision, dimension(Np,N_e)                 :: m_uc
    double precision, dimension(N_d)                    :: s_d
    double precision, intent(in)                        :: alpha_i,svd_imp
    integer                                             :: dumy  ! number of parameters
    logical                                             :: local
    double precision, dimension(:,:), allocatable       :: df_b, mf_b,CDD, tmp_d,tmp_m, d_uc,DD,DD2,DM,CMD
    double precision                                    :: beta,tmp
    integer                                             :: i,j,k,e,p_max
    double precision, dimension(1,1)                    :: tmp_a
    character*1                                         :: transa, transb
    double precision                                    :: const_a, const_b
    transa = 'N'
    transb = 'T'
    allocate(df_b(N_d,1),mf_b(Np,1),CDD(N_d,N_d),tmp_d(N_d,1),tmp_m(Np,1),d_uc(N_d,N_e),DD(N_d,N_e),DM(Np,N_e),DD2(N_d,N_e),CMD(Np,N_d))
    
    !Calculation of MEAN of the MF and DF
    
    do i=1,N_d
        df_b(i,1) = sum(df(i,1:N_e)) / N_e
    end do
    do i=1,Np
        mf_b(i,1) = sum(m_uc(i,1:N_e)) / N_e
    end do
    
    !    - Calculate Cf_MD  using matrix multiplication
    
    DD = 0.0d0
    DM = 0.0d0
    DD2 = 0.0d0
    
        
    do e=1,N_e
        DM(1:,e) = m_uc(1:,e)-mf_b(1:,1)
        DD(1:,e) = df(1:,e)-df_b(1:,1)
        do k=1,N_d
            DD2(k,e) = (df(k,e) - df_b(k,1))/s_d(k)
        end do        
    end do
    
    DD =DD /sqrt(N_e-1.0d0)
    DD2=DD2/sqrt(N_e-1.0d0)
    DM =DM /sqrt(N_e-1.0d0)
    p_max = 0
    call subspace_adap(DD2,N_d,N_e,s_d,CDD,alpha_i,svd_imp,p_max)
    const_a = 1.0d0
    const_b = 0.0d0
    call dgemm('T', 'N', N_d, N_e, N_d, const_a, CDD, N_d, DD, N_d, const_b, DD2, N_d)
    print *, 'CDD*DD'
    call dgemm(transa, transb, Np, N_d, N_e, const_a, DM, Np, DD2, N_d, const_b, CMD, Np)
    print *, 'Pseudo Inverse done.'
    deallocate(DM,DD,DD2,CDD)   
    !open(1096,file='.\CMD.txt', status='unknown')
    !do e=1,np
    !    write(1096,'(792E20.12)')(cmd(e,k),k=1,N_d)
    !end do
    !close(1096)
    do e=1,N_e
        do k=1,N_d
            tmp = gasdev(dumy) ! tmp ~ N(0,1)
            !d_uc(k,e) = d_obs(k,1) + sqrt(alpha_i)*s_d(k)*tmp
            d_uc(k,e) = d_obs(k,1) + s_d(k)*tmp
        end do
    end do
    
    if (local) then
        do k=1,Nd
            do i=1,Np_g
                do j = Loc_indx(k,1),Loc_indx(k+1,1)-1
                    CMD((i-1)*nact+1:i*nact,j) = rho_md(1:Nact,k)*CMD((i-1)*nact+1:i*nact,j)
                end do
            end do
        end do
    end if

    
    
    do e=1,N_e
        tmp_d(1:,1) = d_uc(1:,e) - df(1:,e)
        k=1
        const_b = 0.0d0
        call dgemv(transa, Np, N_d, const_a, CMD, Np, tmp_d, k, const_b, tmp_m, k)
        m_uc(1:,e) = tmp_m(1:,1) + m_uc(1:,e)
        print *, 'Ensemble #', e,'Updated.'
    end do
    
    deallocate(df_b,mf_b,tmp_d,tmp_m,d_uc,CMD)
end subroutine ES_ADAP_MINE
subroutine ES_UPDATE_ADAPT_DUC(N_e,N_d,Np,d_obs,df,m_uc,s_d,alpha_i,svd_imp,dumy,local)
    ! DOES CALCULATE THE C_MD MATRIX BUT IT IN A FEASIBLE WAY!
    USE global_variables,ONLY: Nd,Loc_indx,Nact,Np_g,rho_md,alpha_sum,nolast
    implicit none
    real,external                                       :: gasdev
    integer                                             :: N_e ! number of ensemble members
    integer                                             :: N_d  ! number of data
    integer                                             :: Np  ! number of parameters
    double precision, dimension(N_d,1)                  :: d_obs
    double precision, dimension(N_d,N_e)                :: df
    double precision, dimension(Np,N_e)                 :: m_uc
    double precision, dimension(N_d)                    :: s_d
    double precision                                    :: alpha_i,svd_imp
    integer                                             :: dumy  ! number of parameters
    logical                                             :: local,cond
    double precision, dimension(:,:), allocatable       :: df_b, mf_b,CDD, tmp_d,tmp_m, d_uc,DD,DD2,DM,CMD
    double precision                                    :: beta,tmp
    integer                                             :: i,j,k,e, p_max,p_all
    double precision, dimension(1,1)                    :: tmp_a
    character*1                                         :: transa, transb
    double precision                                    :: const_a, const_b
    transa = 'N'
    transb = 'T'
    allocate(df_b(N_d,1),mf_b(Np,1),CDD(N_d,N_d),tmp_d(N_d,1),tmp_m(Np,1),d_uc(N_d,N_e),DD(N_d,N_e),DM(Np,N_e),DD2(N_d,N_e),CMD(Np,N_d))
    p_all = N_e
    if (N_d<N_e) p_all = N_d
    !Calculation of MEAN of the MF and DF
    do i=1,N_d
        df_b(i,1) = sum(df(i,1:N_e)) / N_e
    end do

    do i=1,Np
        mf_b(i,1) = sum(m_uc(i,1:N_e)) / N_e
    end do

    !    - Calculate Cf_MD  using matrix multiplication
    DD = 0.0d0
    DM = 0.0d0
    DD2 = 0.0d0
    
    do e=1,N_e
        DM(1:,e) = m_uc(1:,e)-mf_b(1:,1)
        DD(1:,e) = df(1:,e)-df_b(1:,1)
        do k=1,N_d
            DD2(k,e) = (df(k,e) - df_b(k,1))/s_d(k)
        end do        
    end do
    DD =DD /sqrt(N_e-1.0d0)
    DD2=DD2/sqrt(N_e-1.0d0)
    DM =DM /sqrt(N_e-1.0d0)
    p_max = 0
    call subspace_inv_duc(DD2,N_d,N_e,s_d,CDD,alpha_i,svd_imp,p_max)
    do
        do e=1,N_e
            do k=1,N_d
                tmp = gasdev(dumy) ! tmp ~ N(0,1)
                d_uc(k,e) = d_obs(k,1) + sqrt(alpha_i)*s_d(k)*tmp
            end do
        end do
        call invert_svd_duc(N_d,N_e,p_all,alpha_i,CDD,s_d)
        call adaptive_check(N_d,N_e,CDD,s_d,df,d_uc,alpha_i,cond)
        if (cond) then
            exit
        else
            alpha_i = alpha_i*2.0d0
        end if
        !end if
    end do
    call invert_svd_duc(N_d,N_e,p_max,alpha_i,CDD,s_d)
    const_a = 1.0d0
    const_b = 0.0d0
    call dgemm('T', 'N', N_d, N_e, N_d, const_a, CDD, N_d, DD, N_d, const_b, DD2, N_d)
    print *, 'CDD*DD'
    call dgemm(transa, transb, Np, N_d, N_e, const_a, DM, Np, DD2, N_d, const_b, CMD, Np)
    print *, 'Pseudo Inverse done.'
    deallocate(DM,DD,DD2,CDD)
    
    if (local) then
        do k=1,Nd
            do i=1,Np_g
                do j = Loc_indx(k,1),Loc_indx(k+1,1)-1
                    CMD((i-1)*nact+1:i*nact,j) = rho_md(1:Nact,k)*CMD((i-1)*nact+1:i*nact,j)
                end do
            end do
        end do
    end if
    
    
    do e=1,N_e
        tmp_d(1:,1) = d_uc(1:,e) - df(1:,e)
        k=1
        const_b = 0.0d0
        call dgemv(transa, Np, N_d, const_a, CMD, Np, tmp_d, k, const_b, tmp_m, k)
        m_uc(1:,e) = tmp_m(1:,1) + m_uc(1:,e)
        print *, 'Ensemble #', e,'Updated.'
    end do
    deallocate(df_b,mf_b,tmp_d,tmp_m,d_uc,CMD)
    
    
end subroutine ES_UPDATE_ADAPT_DUC
subroutine ES_UPDATE_ADAPT_IGL(N_e,N_d,Np,d_obs,df,m_uc,s_d,alpha_i,svd_imp,dumy,local)
    ! DOES CALCULATE THE C_MD MATRIX BUT IT IN A FEASIBLE WAY!
    USE global_variables,ONLY: Nd,Loc_indx,Nact,Np_g,rho_md,alpha_sum,nolast
    implicit none
    real,external                                       :: gasdev
    integer                                             :: N_e ! number of ensemble members
    integer                                             :: N_d  ! number of data
    integer                                             :: Np  ! number of parameters
    double precision, dimension(N_d,1)                  :: d_obs
    double precision, dimension(N_d,N_e)                :: df
    double precision, dimension(Np,N_e)                 :: m_uc
    double precision, dimension(N_d)                    :: s_d
    double precision                                    :: alpha_i,svd_imp
    integer                                             :: dumy  ! number of parameters
    logical                                             :: local,cond
    double precision, dimension(:,:), allocatable       :: df_b, mf_b,CDD, tmp_d,tmp_m, d_uc,DD,DD2,DM,CMD
    double precision                                    :: beta,tmp
    integer                                             :: i,j,k,e, p_max, p_all
    double precision, dimension(1,1)                    :: tmp_a
    character*1                                         :: transa, transb
    double precision                                    :: const_a, const_b
    transa = 'N'
    transb = 'T'
    allocate(df_b(N_d,1),mf_b(Np,1),CDD(N_d,N_d),tmp_d(N_d,1),tmp_m(Np,1),d_uc(N_d,N_e),DD(N_d,N_e),DM(Np,N_e),DD2(N_d,N_e),CMD(Np,N_d))
    p_all = N_e
    if (N_d<N_e) p_all = N_d
    !Calculation of MEAN of the MF and DF
    do i=1,N_d
        df_b(i,1) = sum(df(i,1:N_e)) / N_e
    end do

    do i=1,Np
        mf_b(i,1) = sum(m_uc(i,1:N_e)) / N_e
    end do

    !    - Calculate Cf_MD  using matrix multiplication
    DD = 0.0d0
    DM = 0.0d0
    DD2 = 0.0d0
    
    do e=1,N_e
        DM(1:,e) = m_uc(1:,e)-mf_b(1:,1)
        DD(1:,e) = df(1:,e)-df_b(1:,1)
        do k=1,N_d
            DD2(k,e) = (df(k,e) - df_b(k,1))/s_d(k)
        end do        
    end do
    DD =DD /sqrt(N_e-1.0d0)
    DD2=DD2/sqrt(N_e-1.0d0)
    DM =DM /sqrt(N_e-1.0d0)
    p_max = 0
    call subspace_inv_duc(DD2,N_d,N_e,s_d,CDD,alpha_i,svd_imp,p_max)
    
    e = 1
    do
        call invert_svd_duc(N_d,N_e,p_all,alpha_i,CDD,s_d)
        call adaptive_check(N_d,e,CDD,s_d,df_b,d_obs,alpha_i,cond)
        if (cond) then
            exit
        else
            alpha_i = alpha_i*2.0d0
        end if
    end do
    call invert_svd_duc(N_d,N_e,p_all,alpha_i,CDD,s_d)
    const_a = 1.0d0
    const_b = 0.0d0
    call dgemm('T', 'N', N_d, N_e, N_d, const_a, CDD, N_d, DD, N_d, const_b, DD2, N_d)
    print *, 'CDD*DD'
    call dgemm(transa, transb, Np, N_d, N_e, const_a, DM, Np, DD2, N_d, const_b, CMD, Np)
    print *, 'Pseudo Inverse done.'
    deallocate(DM,DD,DD2,CDD)
    dumy  = 1240
    do e=1,N_e
        do k=1,N_d
            tmp = gasdev(dumy) ! tmp ~ N(0,1)
            d_uc(k,e) = d_obs(k,1) + s_d(k)*tmp
        end do
    end do
    
    
    
    if (local) then
        do k=1,Nd
            do i=1,Np_g
                do j = Loc_indx(k,1),Loc_indx(k+1,1)-1
                    CMD((i-1)*nact+1:i*nact,j) = rho_md(1:Nact,k)*CMD((i-1)*nact+1:i*nact,j)
                end do
            end do
        end do
    end if 
    
    
    do e=1,N_e
        tmp_d(1:,1) = d_uc(1:,e) - df(1:,e)
        k=1
        const_b = 0.0d0
        call dgemv(transa, Np, N_d, const_a, CMD, Np, tmp_d, k, const_b, tmp_m, k)
        m_uc(1:,e) = tmp_m(1:,1) + m_uc(1:,e)
        print *, 'Ensemble #', e,'Updated.'
    end do
    deallocate(df_b,mf_b,tmp_d,tmp_m,d_uc,CMD)

end subroutine ES_UPDATE_ADAPT_IGL    

   
    
subroutine adaptive_check(N_d,N_e,CDD,s_d,df,d_uc,alpha_i,cond)
    USE global_variables, ONLY: rho
    implicit none
    integer                                             :: N_e ! number of ensemble members
    integer                                             :: N_d  ! number of data
    double precision, dimension(N_d,N_e)                :: df,d_uc
    double precision, dimension(N_d,N_d)                :: CDD
    double precision, dimension(N_d)                    :: s_d
    double precision                                    :: alpha_i, const_a,const_b,lhs,rhs
    logical                                             :: cond
    integer                                             :: i,k,e
    double precision,dimension(N_d,1)                   :: tmp_d, tmp_d2
    cond = .true.
    if (alpha_i < 1.0d10) then
        do e = 1, N_e
            tmp_d(1:,1) = d_uc(1:,e) - df(1:,e)
        
            k=1
            const_b = 0.0d0
            const_a = 1.0d0
            call dgemv('N', N_d, N_d, const_a, CDD, N_d, tmp_d, k, const_b, tmp_d2, k)

            lhs = 0.0d0
            rhs = 0.0d0
            do i=1,N_d
                lhs = lhs + tmp_d(i,1)**2 / s_d(i)**2
                rhs = rhs + tmp_d2(i,1)**2 * s_d(i)**2
            end do
            lhs = lhs * rho**2
            rhs = rhs * alpha_i**2
            if (lhs>rhs) then
                cond = .false.
                exit
            end if
        end do
    else
        print *, 'Maximum Inflation factor'
    end if
    
end subroutine adaptive_check
    
subroutine check_conv(N_d,N_e,s_d,d_obs,df,tau,etta,nolast)
    USE global_variables, ONLY: rho
    implicit none
    integer                                             :: N_e ! number of ensemble members
    integer                                             :: N_d  ! number of data
    double precision, dimension(N_d,1)                  :: d_obs
    double precision, dimension(N_d,N_e)                :: df
    double precision, dimension(N_d)                    :: s_d
    double precision                                    :: etta,tau,tmp_dbl,lhs,rhs
    logical                                             :: nolast
    integer                                             :: i,k,e
    double precision,dimension(N_d,1)                   :: df_b,tmp_d
    nolast = .true.
    
    do i=1,N_d
        df_b(i,1) = sum(df(i,1:N_e)) / N_e
    end do

    tmp_d(1:,1) = d_obs(1:,1) - df_b(1:,1)
    lhs = 0.0d0
    do i=1,N_d
        lhs = lhs + tmp_d(i,1)**2 / s_d(i)**2
    end do
    
    lhs = sqrt(lhs)
    rhs = tau*etta
    write(1003,*) lhs,rhs
    if (lhs .LE. rhs) then
        nolast = .false.
    end if

    
end subroutine check_conv  
  
    
    