subroutine check_limit(Np,Ne,m_uc)
    use global_variables, only: par_limit, nact, np_g,np_gl
    implicit none
    integer                             :: Np,Ne
    double precision, dimension(Np,Ne)  :: m_uc
    integer                             :: i,j,k,l
    
    
    do i=1,Np_g
        do j = 1,Nact
            k = (i-1)*nact + j
            do l = 1, Ne
                if (m_uc(k,l) < par_limit(1,i)) m_uc(k,l) = par_limit(1,i)
                if (m_uc(k,l) > par_limit(2,i)) m_uc(k,l) = par_limit(2,i)
            end do
        end do
    end do
    do i=1,np_gl
        do j =1,Ne
            if (m_uc(Np_g*nact+i,j) < par_limit(1,Np_g+i)) m_uc(Np_g*nact+i,j) = par_limit(1,Np_g+i)
            if (m_uc(Np_g*nact+i,j) > par_limit(2,Np_g+i)) m_uc(Np_g*nact+i,j) = par_limit(2,Np_g+i)
        end do
    end do
end subroutine
    