    !SUBROUTINE choldc(a,n,np,p)
    !INTEGER n,np
    !REAL*8 a(np,np),p(n,n)
    !INTEGER i,j,k
    !REAL*8 sum
    !do i=1,n
    !    do j=i,n
    !        sum=a(i,j)
    !        do k=i-1,1,-1
    !            sum=sum-a(i,k)*a(j,k)
    !        end do
    !        if(i.eq.j)then
    !            if(sum <= 0.0d0) pause 'choldc failed'
    !            p(i,i)=dsqrt(sum)
    !        else
    !            a(j,i)=sum/p(i,i)
    !            p(j,i)=a(j,i)
    !            p(i,j)=0
    !        endif
    !    end do
    !end do
    !END
    !!  (C) Copr. 1986-92 Numerical Recipes Software Z3,)%'v1-031..

    ! input/output  a  matrix
! output        p  vector of resulting diag of a
! author:       <Vadum Kutsyy, kutsyy@hotmail.com>
! -------------------------------------------------
	Subroutine choldc1(n,a,p)
	integer n
	real*8 a(n,n), p(n)
	integer i,j,k
	real*8 sum
	do i = 1, n
	do j = i, n
      sum = a(i,j)
      do k = i - 1, 1, -1
        sum = sum - a(i,k) * a(j,k)
      end do
      if (i.eq.j) then
        if (sum <= 0.d0) then
			pause ' the matrix is not positive definite!'
		endif
        p(i) = dsqrt(sum)
      else
        a(j,i) = sum / p(i)
      end if
	end do
	end do
	return
	End

! ------------------------------------------------
!     Cholesky decomposition.
!     input    n  size of matrix
!     input    A  Symmetric positive def. matrix
!     output  aa  lower deomposed matrix
!     uses        choldc1(int,MAT,VEC)
! ------------------------------------------------

	Subroutine choldc(n,A,aa)
	integer :: n
	real*8 :: A(n,n), aa(n,n)
	integer :: i,j, ialloc 
	real*8, allocatable :: p(:)
	allocate(p(1:n),stat=ialloc)

	aa(1:n,1:n) = A(1:n,1:n)
 
	call choldc1(n, aa, p)

	do i = 1, n
	aa(i,i) = p(i)
	do j = i + 1, n
	  aa(i,j) = 0.d0
	end do
	end do
	deallocate(p)
	return
	End