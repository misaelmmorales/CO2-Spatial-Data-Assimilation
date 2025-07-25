!******************************************************************
!*  Inversion of a symmetric matrix by Cholesky decomposition.    *
!*  The matrix must be positive definite.                         * 
!* -------------------------------------------------------------- *
!* REFERENCE:                                                     *
!*             From a Java Library Created by Vadim Kutsyy,       *
!*             "http://www.kutsyy.com".                           *
!* -------------------------------------------------------------- * 
! -------------------------------------------------
! main method for Cholesky decomposition.
!
! input         n  size of matrix
! input/output  a  matrix
! output        p  vector of resulting diag of a
! author:       <Vadum Kutsyy, kutsyy@hotmail.com>
! -------------------------------------------------
	Subroutine choldc1(n,a,p)
	integer n
	real*8 a(0:n-1,0:n-1), p(0:n-1)
	integer i,j,k
	real*8 sum
	do i = 0, n-1
	do j = i, n-1
      sum = a(i,j)
      do k = i - 1, 0, -1
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
	integer n
	real*8 A(0:n-1,0:n-1), aa(0:n-1,0:n-1)
	integer i,j, ialloc 
	real*8, pointer :: p(:)
	allocate(p(0:n-1),stat=ialloc)

	aa = A
 
	call choldc1(n, aa, p)

	do i = 0, n-1
	aa(i,i) = p(i)
	do j = i + 1, n-1
	  aa(i,j) = 0.d0
	end do
	end do
	deallocate(p)
	return
	End



