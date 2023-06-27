subroutine write_outputs(Nd,N_e,df,a)
    use global_variables, only: cwd,modelname
    implicit none
    integer                             :: Nd
    integer                             :: N_e
    integer                             :: a
    double precision, dimension(Nd,N_e) :: df
    character*20                        :: L1,L2,FMT
    character*400                       :: filename
    integer                             :: i,j
    if (a<10) then
        write(L1,'(I1)') a
    elseif (a<100) then
        write(L1,'(I2)') a
    else
        write(L1,'(I3)') a
    end if
    
    if (N_e<10) then
        write(L2,'(I1)') N_e
    elseif (N_e<100) then
        write(L2,'(I2)') N_e
    else
        write(L2,'(I3)') N_e
    end if
    filename = trim(cwd)//'\outputs_'//trim(modelname)//'\outputs_'//trim(L1)//'.txt'
    FMT = '('//trim(L2)//'f16.4)'
    open(1,file=trim(filename),status = 'unknown')
    do i=1,Nd
        write(1,FMT) (df(i,j),j=1,N_e)
    end do
    close(1)
    
end subroutine write_outputs
subroutine write_param(N,N_e,M,a)
    use global_variables, ONLY: cwd,NG,M_all,ACTLIST,nact, np_g, par_name,modelname,np_gl
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: N,N_e
    REAL*8, DIMENSION(N,N_e) :: M
    integer                             :: a
    character*20                        :: L1,L2,FMT,L3
    character*400                       :: filename
    integer                             :: i,j,k
    
    do i=1,Nact
        do k = 1,Np_g
            if (par_name(2,k)(1:4) == 'perm') then
                m_all(ACTLIST(i) + (k-1)*Ng ,1:N_e)  = EXP(M((k-1)*Nact + i,1:N_e))
            else
                m_all(ACTLIST(i) + (k-1)*Ng ,1:N_e)  = M((k-1)*Nact + i,1:N_e)
            end if
        end do
    end do
    do j=1,np_gl
        do i=1,N_e
            m_all(np_g*Ng + j,i) = M(np_g*Nact+j,i)
        end do
    end do
    
    if (a<10) then
        write(L1,'(I1)') a
    elseif (a<100) then
        write(L1,'(I2)') a
    else
        write(L1,'(I3)') a
    end if
    
    if (N_e<10) then
        write(L2,'(I1)') N_e
    elseif (N_e<100) then
        write(L2,'(I2)') N_e
    else
        write(L2,'(I3)') N_e
    end if
    FMT = '('//trim(L2)//'f16.4)'
    do k=1,Np_g
        if (k<10) then
            write(L3,'(I1)') k
        elseif (k<100) then
            write(L3,'(I2)') k
        else
            write(L3,'(I3)') k
        end if
        filename = trim(cwd)//'\outputs_'//trim(modelname)//'\param_'//trim(L3)//'_'//trim(L1)//'.txt'
        
        open(1,file=trim(filename),status = 'unknown')
        do i=(k-1)*Ng+1,k*Ng
            write(1,FMT) (m_all(i,j),j=1,N_e)
        end do
        close(1)
    end do
    if (np_gl>0) then
        filename = trim(cwd)//'\outputs_'//trim(modelname)//'\global_'//trim(L1)//'.txt'
        open(1,file=trim(filename),status = 'unknown')
        do i=1,N_e
            write(1,'(4f16.8)') (m_all(Np_g*Ng+j,i),j=1,np_gl)
        end do
        close(1)
    end if
    
    
end subroutine write_param

subroutine write_fhffile(Nd,Nt,d,e,fhf_id)
    implicit none
    integer                             :: Nd
    integer                             :: Nt
    integer                             :: e
    integer                             :: fhf_id
    CHARACTER*400                       :: Model_name
    double precision, dimension(Nt,Nd+1) :: d
    CHARACTER*20                        :: L1,L2,FMT
    integer                             :: i,j
    
    if (e<10) then
        write(L1,'(I1)') e
    elseif (e<100) then
        write(L1,'(I2)') e
    elseif (e<1000) then
        write(L1,'(I3)') e
    else
        write(L1,'(I4)') e
    end if
    
    if (Nd<10) then
        write(L2,'(I1)') Nd
    elseif (Nd<100) then
        write(L2,'(I2)') Nd
    else
        write(L2,'(I3)') Nd
    end if
    FMT ='(f14.2,'//trim(L2)//'E20.10)'
    model_name = "'Model_"//trim(L1)//"'"
    write(fhf_id,'(A)') model_name
    do i=1,Nt
        write(fhf_id,FMT) d(i,1), (d(i,j),j=2,Nd+1)
    end do
    write(fhf_id,'(A)') ' '
    
    
    
end subroutine write_fhffile
    
subroutine write_objectives(N_e,M,a)
    use global_variables, ONLY: cwd,modelname
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: N_e
    REAL*8, DIMENSION(N_e,1) :: M
    integer                             :: a
    character*20                        :: L1,L2,FMT,L3
    character*400                       :: filename
    integer                             :: i,j,k
    
    
    if (a<10) then
        write(L1,'(I1)') a
    elseif (a<100) then
        write(L1,'(I2)') a
    else
        write(L1,'(I3)') a
    end if
    filename = trim(cwd)//'\outputs_'//trim(modelname)//'\objc_'//trim(L1)//'.txt'
    open(1,file=trim(filename),status = 'unknown')
    do k=1,N_e
        write(1,'(E24.14)') M(k,1)
    end do
    close(1)
end subroutine write_objectives
