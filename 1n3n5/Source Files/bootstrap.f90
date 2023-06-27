subroutine bootstrap(Ne,Nm,Nd,Nb,Ke,mf,df,s_d, alpha_i)
    implicit none
    integer, intent(in)                   :: Ne, Nm, Nd, Nb
    double precision, dimension(Nm,Nd)    :: Ke
    double precision, dimension(Nm,Ne)    :: mf
    double precision, dimension(Nd,Ne)    :: df
    double precision, dimension(Nd)       :: s_d
    double precision                      :: alpha_i
    
    
    double precision, dimension(Nm,Nd,Nb) :: Ks
    double precision, dimension(Nm,Nd)    :: c_md
    double precision, dimension(Nd,Nd)    :: c_dd
    double precision, dimension(Nd,1)     :: df_b,tmp_d
    double precision, dimension(Nm,1)     :: mf_b,tmp_m
    integer, dimension(Ne)                :: list
    integer                               :: i,j,k,e
    double precision                      :: s_alpha, Ri
    
    s_alpha = 0.6d0
    do j=1,Nb
        call rand_list(Ne,list)
        !Calculation of MEAN of the MF and DF
        df_b = 0.0d0
        do i=1,Nd
            do k=1,Ne
                df_b(i,1) = df_b(i,1) + df(i,list(k))
            end do
            df_b(i,1) = df_b(i,1) / Ne
        end do
        
        mf_b = 0.0d0
        do i=1,Nm
            do k=1,Ne
                mf_b(i,1) = mf_b(i,1) + mf(i,list(k))
            end do
            mf_b(i,1) = mf_b(i,1) / Ne
        end do
        !    - Calculate C_MD , C_DD using matrix multiplication
        C_DD = 0.0d0
        C_MD = 0.0d0

        do k=1,Ne
            e = list(k)
            tmp_d(1:,1) = df(1:,e)  -df_b(1:,1)
            tmp_m(1:,1) = mf(1:,e)  -mf_b(1:,1)
            C_DD = C_DD + MATMUL(tmp_d,transpose(tmp_d))
            C_MD = C_MD + MATMUL(tmp_m,transpose(tmp_d))
        end do

        C_DD = C_DD / (Ne-1.0d0)
        C_MD = C_MD / (Ne-1.0d0)
        
        DO k=1,Nd
            C_DD (k,k) = C_DD(k,k) + alpha_i*s_d(k)**2
        END DO
        
        call inv_svdt(C_DD,Nd,1.0d0)
        Ks(1:Nm,1:Nd,j) = MATMUL(C_MD,C_DD)
    end do
    ! bootstrap ============================================================================
    c_md = 0.0d0
    do i=1,Nm
        do j=1,Nd
            do k=1,Nb
                c_md(i,j) = c_md(i,j) + (Ks(i,j,k) - Ke(i,j))**2
            end do
            c_md(i,j) = c_md(i,j) / Nb
            Ri = c_md(i,j) / Ke(i,j)**2
            c_md(i,j) = (1.0d0  - Ri / (Nb -1.0d0) ) / ( Ri + 1.0d0)
            if (c_md(i,j) < 0.0d0)  then
                c_md(i,j) = 0.0d0
            end if
            Ke(i,j) = Ke(i,j) * c_md(i,j)
        end do
    end do
    
    ! modified bootstrap=======================================================================
    !c_md = 0.0d0
    !do i=1,Nm
    !    do j=1,Nd
    !        do k=1,Nb
    !            c_md(i,j) = c_md(i,j) + (Ks(i,j,k) - Ke(i,j))**2
    !        end do
    !        c_md(i,j) = c_md(i,j) / Nb
    !        Ri        = c_md(i,j) / Ke(i,j)**2
    !        c_md(i,j) = 1.0d0 / (1.0d0 + Ri * (1.0d0 + 1.0d0 / s_alpha**2))
    !        if (c_md(i,j) < 0.0d0)  then
    !            c_md(i,j) = 0.0d0
    !        end if
    !        Ke(i,j) = Ke(i,j) * c_md(i,j)
    !    end do
    !end do  
    
    
end subroutine bootstrap
    
subroutine rand_list(Ne,list)
    use IFPORT
    implicit none
    integer, intent(in)    :: Ne
    integer, dimension(Ne) :: list
    integer                :: i
    
    do i=1,Ne
        list(i) = floor(RAND(0)*Ne) + 1
    end do
    
end subroutine