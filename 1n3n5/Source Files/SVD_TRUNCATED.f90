subroutine svd( m, n, a, u, sm, vm )
      implicit none

      integer ( kind = 4 )      :: m,n

      double precision          :: a(m,n)
      integer ( kind = 4 )      :: info
      character                 :: jobu, jobv
      integer ( kind = 4 )      :: lda, ldu,ldv,lwork
  
      double precision          :: sm(m), u(m,m), vm(m,n), work(5*m+n)

      if ( n < m ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SVD_TRUNCATED_V - Fatal error!'
        write ( *, '(a)' ) '  Only call this function when M <= N.'
        write ( *, '(a,i8)' ) '  But we have M = ', m
        write ( *, '(a,i8)' ) '  and N = ', n
        stop 1
      end if

      jobu  = 'a'
      jobv  = 'a'
      lda   = m
      ldu   = m
      ldv   = n
      lwork = 5 * m + n

      call dgesvd ( jobu, jobv, m, n, a, lda, sm, u, ldu, vm, ldv, work, lwork, info )

      if ( info .NE. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SVD_TRUNCATED - Warning!'
        write ( *, '(a,i8)' ) '  DGESVD returned INFO = ', info
      end if

      return
end subroutine svd
    
subroutine inv_svdt(A,n,t,p_max)
    implicit none
    integer, intent(in)              :: n      ! dimension of the matrix
    double precision, dimension(n,n) :: A      ! matrix
    double precision                 :: t      ! treshold for singular value, if set to less than 1, truncated svd is used
    double precision, dimension(:,:),allocatable :: um,vn,ui,vi
    !double precision, dimension(:,:), a :: ui,vi
    double precision, dimension(:),allocatable   :: sn
    integer                          :: i , m, p_max
    double precision                 :: sum_p, sum_n
    
    allocate(um(n,n),vn(n,n),ui(n,1),vi(n,1),sn(n))
    if (t>1.0d0) t = 1.0d0
    
    um = 0.0d0
    vn = 0.0d0
    ui = 0.0d0
    vi = 0.0d0
    sn = 0.0d0
    call svd ( n, n, A, um, sn, vn )
    
    !vn = transpose(vn)
    sum_n = sum(sn)
    sum_p = 0.0d0
    
    do i=1,n
        if ((sum_p + sn(i)> t*sum_n) .and. (sum_p > t*sum_n) ) exit
        sum_p = sum_p + sn(i)
        m     = i
    end do
    if (p_max>0 .and. m>p_max) m = p_max
    write(10002,*) n,m
    A = 0.0d0
    do i=1,m
      ui(1:n,1) = um(1:n,i)
      vi(1:n,1) = vn(i,1:n)
      a(1:n,1:n) = a(1:n,1:n) + MATMUL(vi,transpose(ui)) / sn(i)
    end do
    
    deallocate(um,vn,ui,vi,sn)
    
end subroutine inv_svdt

subroutine subspace_inv(DDf,m,n,s_d,CDD,alpha_i,t,p_max)
    implicit none
    integer                                      :: m    ! number of data
    integer                                      :: n    ! number of ensemble members
    double precision, dimension(m,n)             :: DDf  ! 
    double precision, dimension(m)               :: s_d  ! diagonals of C_D
    double precision, dimension(m,m)             :: CDD  ! pseudo invese as output
    double precision                             :: alpha_i
    double precision                             :: t      ! treshold for singular value, if set to less than 1, truncated svd is used
    integer                                      :: i,p,l,p_max
    double precision,allocatable,dimension(:,:)  :: u,v,ui!s(m), u(m,m), v(m,n),ui(m,1)
    double precision,allocatable,dimension(:)    :: s
    double precision                             :: sum_p, sum_n,m_s,n_s
    l = n
    if (m<n) l = m
    allocate(s(l),u(m,l),v(l,n),ui(m,1))
    s = 0.0d0
    u = 0.0d0
    v = 0.0d0
    ui= 0.0d0
    if (m<n) then
        call svd_truncated_v( m, n, DDf, u, s, v )
    else
        call svd_truncated_u( m, n, DDf, u, s, v )
    end if
    !call svd( m, n, DDf, u, s, v )
    sum_n = sum(s)
    sum_p = 0.0d0
    if (t>1.0d0) t = 1.0d0
    do i=1,l
        if ((sum_p + s(i)> t*sum_n) .and. (sum_p > t*sum_n) ) exit
        sum_p = sum_p + s(i)
        p     = i
    end do
    m_s =0.0d0
    n_s =0.0d0
    do i=1,l
        m_s = m_s + s(i)
        n_s = n_s + s(i)**2
    end do
    n_s = sqrt(n_s)/l
    m_s = m_s / l
    if (p_max>0 .and. p>p_max) p = p_max
    write(10002,'(2I8, 5E20.12)') l,p,s(1),s(l),s(p),m_s,n_s
    CDD = 0.0d0
    do i=1,p
        ui(1:m,1) = u(1:m,i)
        CDD(1:m,1:m) = CDD(1:m,1:m) + MATMUL(ui,transpose(ui)) / (alpha_i+s(i)**2)
    end do
    
    DO i=1,m
        CDD (i,1:) = Cdd(i,1:)/s_d(i)
        CDD (1:,i) = Cdd(1:,i)/s_d(i)
    END DO
    deallocate(s,u,v,ui)
    
end subroutine subspace_inv

subroutine subspace_inv_duc(DDf,m,n,s_d,CDD,alpha_i,t,p_max)
    USE global_variables,ONLY: adap_s, adap_u, adap_v, adap_ui
    implicit none
    integer                                      :: m    ! number of data
    integer                                      :: n    ! number of ensemble members
    double precision, dimension(m,n)             :: DDf  ! 
    double precision, dimension(m)               :: s_d  ! diagonals of C_D
    double precision, dimension(m,m)             :: CDD  ! pseudo invese as output
    double precision                             :: alpha_i
    double precision                             :: t      ! treshold for singular value, if set to less than 1, truncated svd is used
    integer                                      :: i,p,l,p_max
    double precision                             :: sum_p, sum_n,m_s,n_s
    
    l = n
    if (m<n) l = m
    adap_s = 0.0d0
    adap_u = 0.0d0
    adap_v = 0.0d0
    adap_ui= 0.0d0
    if (m<n) then
        call svd_truncated_v( m, n, DDf, adap_u, adap_s, adap_v )
    else
        call svd_truncated_u( m, n, DDf, adap_u, adap_s, adap_v )
    end if
    !call svd( m, n, DDf, u, s, v )
    sum_n = sum(adap_s)
    sum_p = 0.0d0
    if (t>1.0d0) t = 1.0d0
    do i=1,l
        if ((sum_p + adap_s(i)> t*sum_n) .and. (sum_p > t*sum_n) ) exit
        sum_p = sum_p + adap_s(i)
        p     = i
    end do
    
    
    m_s =0.0d0
    n_s =0.0d0
    do i=1,l
        m_s = m_s + adap_s(i)
        n_s = n_s + adap_s(i)**2
    end do
    n_s = sqrt(n_s/l)
    m_s = m_s / l
    if (p_max>0 .and. p>p_max) p = p_max
    p_max = p
    write(10002,'(2I8, 5E20.12)') l,p,adap_s(1),adap_s(l),adap_s(p),m_s,n_s
    
    
end subroutine subspace_inv_duc

subroutine svd_GDGDT(Gd,m,n,s_d,CDD,alpha_i,t,p_max)
    USE global_variables,ONLY: adap_s, adap_u, adap_v, adap_ui
    implicit none
    integer                                      :: m    ! number of data
    integer                                      :: n    ! number of ensemble members
    double precision, dimension(m,n)             :: Gd   ! 
    double precision, dimension(m)               :: s_d  ! diagonals of C_D
    double precision, dimension(m,m)             :: CDD  ! pseudo invese as output
    double precision                             :: alpha_i
    double precision                             :: t      ! treshold for singular value, if set to less than 1, truncated svd is used
    integer                                      :: i,p,l,p_max
    double precision                             :: sum_p, sum_n,m_s,n_s,const_a,const_b
    
    
    const_a = 1.0d0
    const_b = 0.0d0
    call dgemm('N', 'T', m, m , n, const_a, Gd, m, Gd, m, const_b, CDD, m)
    adap_s = 0.0d0
    adap_u = 0.0d0
    adap_v = 0.0d0
    adap_ui= 0.0d0
    call svd_truncated_u( m, m, CDD, adap_u, adap_s, adap_v )
    adap_s = sqrt(adap_s)
    sum_n = sum(adap_s)
    sum_p = 0.0d0
    if (t>1.0d0) t = 1.0d0
    do i=1,m
        if ((sum_p + adap_s(i)> t*sum_n) .and. (sum_p > t*sum_n) ) exit
        sum_p = sum_p + adap_s(i)
        p     = i
    end do
    l = n
    if (m<n) l = m
    
    m_s =0.0d0
    n_s =0.0d0
    do i=1,l
        m_s = m_s + adap_s(i)
        n_s = n_s + adap_s(i)**2
    end do
    n_s = sqrt(n_s/l)
    m_s = m_s / l
    if (p_max>0 .and. p>p_max) p = p_max
    p_max = p
    
    write(10002,'(2I8, 5E20.12)') l,p,adap_s(1),adap_s(l),adap_s(p),m_s,n_s
    
    
end subroutine svd_GDGDT      
    
subroutine invert_svd_duc(m,n,p_max,alpha_i,CDD,s_d)
    USE global_variables,ONLY: adap_s, adap_u, adap_v, adap_ui
    implicit none
    integer                                      :: m    ! number of data
    integer                                      :: n    ! number of ensemble members
    double precision, dimension(m)               :: s_d  ! diagonals of C_D
    double precision, dimension(m,m)             :: CDD  ! pseudo invese as output
    integer                                      :: i,l,p_max
    double precision                             :: alpha_i, const_a, const_b
    CDD = 0.0d0
    const_b = 1.0d0
    do i=1,p_max
        adap_ui(1:m,1) = adap_u(1:m,i)
        const_a = 1.0d0 / (alpha_i+adap_s(i)**2)
        call dgemm('N', 'T', m, m, 1, const_a, adap_ui, m, adap_ui, m, const_b, CDD, m)
        !CDD(1:m,1:m) = CDD(1:m,1:m) + MATMUL(adap_ui,transpose(adap_ui)) / (alpha_i+adap_s(i)**2)
    end do
    DO i=1,m
        CDD (i,1:) = Cdd(i,1:)/s_d(i)
        CDD (1:,i) = Cdd(1:,i)/s_d(i)
    END DO
end subroutine invert_svd_duc
subroutine subspace_adap(Gd,m,n,s_d,CDD,alpha_i,t,p_max)
    USE global_variables,ONLY: method,max_alpha,rho,alpha_sum
    implicit none
    integer                                      :: m    ! number of data
    integer                                      :: n    ! number of ensemble members
    double precision, dimension(m,n),intent(in)  :: Gd  ! 
    double precision, dimension(m)               :: s_d  ! diagonals of C_D
    double precision, dimension(m,m)             :: CDD  ! pseudo invese as output
    double precision                             :: alpha_i
    double precision                             :: t      ! treshold for singular value, if set to less than 1, truncated svd is used
    integer                                      :: i,p,l,p_max
    double precision,allocatable,dimension(:,:)  :: u,v,ui!s(m), u(m,m), v(m,n),ui(m,1)
    double precision,allocatable,dimension(:)    :: s
    double precision                             :: sum_p, sum_n,m_s,n_s, const_a, const_b
    character*1                                  :: transa, transb
    integer                                      :: lda,ldb,ldc

    l = n
    if (m<n) l = m
    allocate(s(l),u(m,l),v(l,n),ui(m,1))
    s = 0.0d0
    u = 0.0d0
    v = 0.0d0
    ui= 0.0d0
    if (m<n) then
        call svd_truncated_v( m, n, Gd, u, s, v )
    else
        call svd_truncated_u( m, n, Gd, u, s, v )
    end if
    !call svd( m, n, DDf, u, s, v )
    sum_n = sum(s)
    sum_p = 0.0d0
    if (t>1.0d0) t = 1.0d0
    do i=1,l
        if ((sum_p + s(i)> t*sum_n) .and. (sum_p > t*sum_n) ) exit
        !if (s(i)<1.0d-10) exit
        sum_p = sum_p + s(i)
        p     = i
    end do
    m_s =0.0d0
    n_s =0.0d0
    do i=1,l
        m_s = m_s + s(i)
        n_s = n_s + s(i)**2
    end do
    n_s = sqrt(n_s/ l) 
    m_s = m_s / l
    if (p_max>0 .and. p>p_max) p = p_max
    
    !allocate(s(m),u(m,m),v(m,m),ui(m,1))
    !
    !const_a = 1.0d0
    !const_b = 0.0d0
    !call dgemm('N', 'T', m, m , n, const_a, Gd, m, Gd, m, const_b, CDD, m)
    !s = 0.0d0
    !u = 0.0d0
    !v = 0.0d0
    !ui= 0.0d0
    !call svd_truncated_u( m, m, CDD, u, s, v )
    !s = sqrt(s)
    !sum_n = sum(s)
    !sum_p = 0.0d0
    !if (t>1.0d0) t = 1.0d0
    !do i=1,m
    !    if ((sum_p + s(i)> t*sum_n) .and. (sum_p > t*sum_n) ) exit
    !    sum_p = sum_p + s(i)
    !    p     = i
    !end do
    !l = n
    !if (m<n) l = m
    !
    !m_s =0.0d0
    !n_s =0.0d0
    !do i=1,l
    !    m_s = m_s + s(i)
    !    n_s = n_s + s(i)**2
    !end do
    !n_s = sqrt(n_s/l)
    !m_s = m_s / l
    !if (p_max>0 .and. p>p_max) p = p_max
    !p_max = p
    
    
    write(10002,'(2I8, 5E20.12)') l,p,s(1),s(l),s(p),m_s,n_s
    if (method ==4) then
        alpha_i = rho*m_s**2/(1.0d0-rho)
        if (alpha_i>max_alpha) alpha_i = max_alpha
        if (alpha_i<1.0d0)     alpha_i = 1.0d0
        if (alpha_sum + 1/alpha_i > 1.0d0) alpha_i = 1.0d0/ (1.0d0-alpha_sum)
        !call find_alpha_trace(m,s(1:m),alpha_i)
    elseif (method==1 .and. alpha_i == 0.0d0 ) then
        alpha_i = m_s**2
        if (alpha_i>max_alpha) alpha_i = max_alpha
    end if
    CDD = 0.0d0
    const_b = 1.0d0
    const_a = 1.0d0
    transa = 'N'
    transb = 'T'
    do i=1,p
        ui(1:m,1) = u(1:m,i)
        const_a = 1.0d0 / (alpha_i+s(i)**2)
        call dgemm(transa, transb, m, m, 1, const_a, ui, m, ui, m, const_b, CDD, m)
        !CDD(1:m,1:m) = CDD(1:m,1:m) + MATMUL(ui,transpose(ui)) / (alpha_i+s(i)**2)
    end do
    
    DO i=1,m
        CDD (i,1:) = Cdd(i,1:)/s_d(i)
        CDD (1:,i) = Cdd(1:,i)/s_d(i)
    END DO
    deallocate(s,u,v,ui)
    
end subroutine subspace_adap
  
subroutine subspace_adaptive(DDf,m,n,s_d,alpha_i,t,p_max)
    USE global_variables,ONLY: max_alpha
    implicit none
    integer                                      :: m    ! number of data
    integer                                      :: n    ! number of ensemble members
    double precision, dimension(m,n)             :: DDf  ! 
    double precision, dimension(m)               :: s_d  ! diagonals of C_D
    double precision                             :: alpha_i
    double precision                             :: t      ! treshold for singular value, if set to less than 1, truncated svd is used
    integer                                      :: i,p,l,p_max
    double precision,allocatable,dimension(:,:)  :: u,vt,ui,vi!s(m), u(m,m), v(m,n),ui(m,1)
    double precision,allocatable,dimension(:)    :: s
    double precision                             :: sum_p, sum_n,m_s,n_s
    l = n
    if (m<n) l = m
    allocate(s(l),u(m,l),vt(l,n),ui(m,1),vi(1,n))
    s = 0.0d0
    u = 0.0d0
    vt = 0.0d0
    ui= 0.0d0
    if (m<n) then
        call svd_truncated_v( m, n, DDf, u, s, vt )
    else
        call svd_truncated_u( m, n, DDf, u, s, vt )
    end if
    !call svd( m, n, DDf, u, s, v )
    sum_n = sum(s)
    sum_p = 0.0d0
    if (t>1.0d0) t = 1.0d0
    do i=1,l
        if ((sum_p + s(i)> t*sum_n) .and. (sum_p > t*sum_n) ) exit
        !if (s(i)<1.0d-10) exit
        sum_p = sum_p + s(i)
        p     = i
    end do
    m_s =0.0d0
    n_s =0.0d0
    do i=1,l
        m_s = m_s + s(i)
        n_s = n_s + s(i)**2
    end do
    n_s = sqrt(n_s/ l) 
    m_s = m_s / l
    if (p_max>0 .and. p>p_max) p = p_max
    
    write(10002,'(2I8, 5E20.12)') l,p,s(1),s(l),s(p),m_s,n_s
    if (alpha_i ==0.0d0) then
        alpha_i = n_s
        if (alpha_i>max_alpha) alpha_i = max_alpha
    end if
    DDf = 0.0d0
    do i=1,p
        ui(1:m,1) = u(1:m,i)
        vi(1,1:n) = vt(i,1:n)
        DDf(1:m,1:n) = DDf(1:m,1:n) + MATMUL(ui,vi) * s(i)/ (alpha_i+s(i)**2)
    end do
    deallocate(s,u,vt,ui)
    
end subroutine subspace_adaptive    
    
subroutine svd_truncated_u ( m, n, a, un, sn, v )

!*****************************************************************************80
!
!! SVD_TRUNCATED_U computes the SVD when N <= M.
!
!  Discussion:
!
!    A(mxn) = U(mxm)  * S(mxn)  * V(nxn)'
!           = Un(mxn) * Sn(nxn) * V(nxn)'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of A.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix to be decomposed.
!
!    Output, real ( kind = 8 ) UN(M,N), the first N left singular vectors.
!
!    Output, real ( kind = 8 ) SN(N), the first N singular values.
!
!    Output, real ( kind = 8 ) V(N,N), the right singular vectors.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) info
  character jobu
  character jobv
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldu
  integer ( kind = 4 ) ldv
  integer ( kind = 4 ) lwork
  real ( kind = 8 ) sn(n)
  real ( kind = 8 ) un(m,n)
  real ( kind = 8 ) v(n,n)
  real ( kind = 8 ) work(5*n+m)

  if ( m < n ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SVD_TRUNCATED_U - Fatal error!'
    write ( *, '(a)' ) '  Only call this function when N <= M.'
    write ( *, '(a,i8)' ) '  But we have M = ', m
    write ( *, '(a,i8)' ) '  and N = ', n
    stop 1
  end if

  jobu = 's'  ! a: all m columns of U are returned in the array u
              ! 
  jobv = 'a'
  lda = m
  ldu = m
  ldv = n
  lwork = 5 * n + m

  call dgesvd ( jobu, jobv, m, n, a, lda, sn, un, ldu, v, ldv, work, lwork, &
    info )

  if ( info == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SVD_TRUNCATED_U:'
    write ( *, '(a)' ) '  DGESVD computation was successful.'
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SVD_TRUNCATED_U - Warning!'
    write ( *, '(a,i8)' ) '  DGESVD returned INFO = ', info
  end if

  return
end subroutine svd_truncated_u
subroutine svd_truncated_v ( m, n, a, u, sm, vm )

!*****************************************************************************80
!
!! SVD_TRUNCATED_V computes the SVD when M <= N.
!
!  Discussion:
!
!    A(mxn) = U(mxm) * S(mxn)  * V(nxn)'
!           = U(mxm) * Sm(mxm) * Vm(mxn)'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of A.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix to be decomposed.
!
!    Output, real ( kind = 8 ) U(M,M), the left singular vectors.
!
!    Output, real ( kind = 8 ) SM(M), the first M singular values.
!
!    Output, real ( kind = 8 ) VM(M,N), the first M right singular vectors.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) info
  character jobu
  character jobv
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldu
  integer ( kind = 4 ) ldv
  integer ( kind = 4 ) lwork
  real ( kind = 8 ) sm(m)
  real ( kind = 8 ) u(m,m)
  real ( kind = 8 ) vm(m,n)
  real ( kind = 8 ) work(5*m+n)

  if ( n < m ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SVD_TRUNCATED_V - Fatal error!'
    write ( *, '(a)' ) '  Only call this function when M <= N.'
    write ( *, '(a,i8)' ) '  But we have M = ', m
    write ( *, '(a,i8)' ) '  and N = ', n
    stop 1
  end if

  jobu = 'a'
  jobv = 's'
  lda = m
  ldu = m
  ldv = m
  lwork = 5 * m + n

  call dgesvd ( jobu, jobv, m, n, a, lda, sm, u, ldu, vm, ldv, work, lwork, &
    info )

  if ( info == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SVD_TRUNCATED_V:'
    write ( *, '(a)' ) '  DGESVD computation was successful.'
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SVD_TRUNCATED_V - Warning!'
    write ( *, '(a,i8)' ) '  DGESVD returned INFO = ', info
  end if

  return
end subroutine svd_truncated_v

subroutine svd_truncated_all ( m, n, a, u, sm, vm )

!*****************************************************************************80
!
!! SVD_TRUNCATED_V computes the SVD .
!
!  Discussion:
!
!    A(mxn) = U(mxm) * S(mxn)  * V(nxn)'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of A.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix to be decomposed.
!
!    Output, real ( kind = 8 ) U(M,M), the left singular vectors.
!
!    Output, real ( kind = 8 ) SM(M), the first M singular values.
!
!    Output, real ( kind = 8 ) VM(M,N), the first M right singular vectors.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) info
  character jobu
  character jobv
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldu
  integer ( kind = 4 ) ldv
  integer ( kind = 4 ) lwork
  real ( kind = 8 ) sm(m)
  real ( kind = 8 ) u(m,m)
  real ( kind = 8 ) vm(n,n)
  real ( kind = 8 ) work(5*m+n)


  jobu = 'a'
  jobv = 'a'
  lda = m
  ldu = m
  ldv = n
  lwork = max(3*min(m, n)+max(m, n), 5*min(m,n)) !5 * m + n

  call dgesvd ( jobu, jobv, m, n, a, lda, sm, u, ldu, vm, ldv, work, lwork, &
    info )
  if ( info .NE. 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SVD_TRUNCATED_ALL - Warning!'
    write ( *, '(a,i8)' ) '  DGESVD returned INFO = ', info
  end if

  return
end subroutine svd_truncated_all
subroutine invert_sqrt(IMAT,TR,m,n,p,alpha_i)
    USE global_variables,ONLY: adap_s, adap_u, adap_v
    implicit none
    integer                                         :: m,n,p
    double precision, dimension(n,m)                :: IMAT
    double precision, dimension(n,n)                :: TR
    double precision                                :: alpha_i
    integer                                         :: i,j,k
    CHARACTER*1                                     :: transa, transb
    double precision                                :: const_a, const_b
    double precision,dimension(n,1)                 :: vi
    double precision,dimension(m,1)                 :: ui
    
    TR = 0.0d0
    IMAT = 0.0d0
    const_b = 1.0d0
    do i=1,p
        const_a = 1.0d0
        transa = 'N'
        transb = 'T'
        vi(1:n,1) = adap_v(1:n,i)
        if (i .LE. m) then
            const_a = sqrt(alpha_i / (alpha_i+adap_s(i)**2))
        end if
        call dgemm(transa, transb, n, n, 1, const_a, vi, n, vi, n, const_b, TR, n)
        
        const_a = 1.0d0
        transa = 'N'
        transb = 'T'
        ui(1:m,1) = adap_u(1:m,i)
        if (i .LE. m) then
            const_a = adap_s(i) / (alpha_i+adap_s(i)**2)
        end if
        call dgemm(transa, transb, n, m, 1, const_a, vi, n, ui, m, const_b, IMAT, n)
        
        
        
    end do
    
    
    
end subroutine invert_sqrt
    
    
subroutine invert_sqrt_2(CDD,TR,m,n,alpha_i,s_d,p_max)
    USE global_variables,ONLY: adap_s, adap_u, adap_v
    implicit none
    integer                                         :: m,n
    double precision, dimension(m,m)                :: CDD
    double precision, dimension(n,n)                :: TR
    double precision,dimension(m)                   :: s_d
    double precision                                :: alpha_i
    integer                                         :: i,j,k,p_max,l
    CHARACTER*1                                     :: transa, transb
    double precision                                :: const_a, const_b
    double precision,dimension(n,1)                 :: vi
    double precision,dimension(m,1)                 :: ui
    
    
    CDD = 0.0d0
    const_b = 1.0d0
    const_a = 1.0d0
    transa = 'N'
    transb = 'T'
    do i=1,m
        ui(1:m,1) = adap_u(1:m,i)
        const_a = 1.0d0 / (alpha_i+adap_s(i)**2)
        call dgemm(transa, transb, m, m, 1, const_a, ui, m, ui, m, const_b, CDD, m)
    end do
    
    DO i=1,m
        CDD (i,1:) = Cdd(i,1:)/s_d(i)
        CDD (1:,i) = Cdd(1:,i)/s_d(i)
    END DO
    
    TR = 0.0d0

    do i=1,n
        const_a = 1.0d0
        transa = 'N'
        transb = 'T'
        vi(1:n,1) = adap_v(1:n,i)
        if (i .LE. m) then
            const_a = sqrt(alpha_i / (alpha_i+adap_s(i)**2))
        end if
        call dgemm(transa, transb, n, n, 1, const_a, vi, n, vi, n, const_b, TR, n)
        
    end do
    
    
    
end subroutine invert_sqrt_2    
    
    
    
    
    