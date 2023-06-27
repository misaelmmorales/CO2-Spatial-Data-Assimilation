!subroutine  obj_calc(N_d,N_e,d_obs,df,s_d,NOFunc,EnNOFunc)
!    implicit none
!    integer                                     :: N_d,N_e
!    double precision, dimension(N_d,1)          :: d_obs
!    double precision, dimension(N_d)            :: s_d
!    double precision, dimension(N_d,N_e)        :: df
!    double precision, dimension(N_e,1)          :: EnNOFunc
!    double precision                            :: NOFunc
!    integer                                     :: e,i
!    double precision, dimension(N_d,1)          :: tmp_d, df_b
!    EnNOFunc = 0.0d0
!    
!    do e = 1, N_e !  ========================================================================================================  
!        tmp_d(1:,1) = df(1:,e) - d_obs(1:,1)
!        do i=1,N_d
!            EnNOFunc(e,1) = EnNOFunc(e,1) + tmp_d(i,1)**2 / s_d(i)**2 
!        end do
!    end do !=================================================================================================================
!    EnNOFunc(1:,1) =EnNOFunc(1:,1) / N_d
!    close (4)
!    
!    df_b = 0.0d0
!    do i=1,N_d
!        df_b(i,1) = sum(df(i,1:N_e)) / N_e
!    end do
!
!    tmp_d(1:,1) = d_obs(1:,1) - df_b(1:,1)
!    NOFunc   = 0.0d0
!    do i=1,N_d
!        NOFUNC = NOFUNC + tmp_d(i,1)**2 / s_d(i)**2
!    end do
!    NOFUNC = NOFUNC / N_d
!    
!end subroutine

subroutine  obj_calc(N_d,N_e,d_obs,df,s_d,NOFunc,EnNOFunc)
    implicit none
    integer                                     :: N_d,N_e
    double precision, dimension(N_d,1)          :: d_obs
    double precision, dimension(N_d)            :: s_d
    double precision, dimension(N_d,N_e)        :: df
    double precision, dimension(N_e,1)          :: EnNOFunc
    double precision                            :: NOFunc
    integer                                     :: e,i
    double precision, dimension(N_d,1)          :: tmp_d
    EnNOFunc = 0.0d0
    NOFunc   = 0.0d0
    do e = 1, N_e !  ========================================================================================================  
        tmp_d(1:,1) = df(1:,e) - d_obs(1:,1)
        do i=1,N_d
            EnNOFunc(e,1) = EnNOFunc(e,1) + tmp_d(i,1)**2 / s_d(i)**2 
        end do
        NOFunc = NOFunc + EnNOFunc(e,1)
    end do !=================================================================================================================
    close (4)
    EnNOFunc(1:,1) = EnNOFunc(1:,1) / (N_d)
    NOFunc = NOFunc / (N_e * N_d)
end subroutine