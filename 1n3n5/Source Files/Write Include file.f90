!SUBROUTINE W_INC_FILE (ind) ! ORIGINAL BRUGGE MODEL
!    use global_variables, ONLY: generic_dir_name,NG,M_all,np_g, par_name,actnum,cwd,modelname,N_e,np_gl,SAT_VALS, PC_VALS,NSAT_TAB,NTABLES
!    IMPLICIT NONE
!    INTEGER :: i,j,ind,k
!    double precision   :: swl,phi,sw,swc,sor,krwro,krocw,n_w,n_o,kro,krw
!    CHARACTER*20 :: FILENAME,LINE1
!    CHARACTER*300            :: FILE,command
!    logical :: file_e
!    !FILENAME = 'grid_data.inc'
!    do k = 1, Np_g
!        FILE = trim(generic_dir_name) // '\'//trim(par_name(1,k))//'.dat'
!        OPEN (1, FILE=trim(FILE), STATUS='UNKNOWN')
!        WRITE(1,'(A)') trim(par_name(2,k))
!        do i=1,NG
!            write(1,'(f16.8)') m_all(i+(k-1)*Ng,ind)
!        end do
!        WRITE(1,'(A1)') '/'
!        close(unit=1)
!    end do
!    !!! ONLY FOR BRUGGE CASE
!    !if (ind == 1) then
!        FILE = trim(generic_dir_name) // '\region.dat'
!        OPEN (1, FILE=trim(FILE), STATUS='UNKNOWN')
!        WRITE(1,'(A)') 'SATNUM' 
!        do i=1,NG
!            if (ACTNUM(i)==1) then
!                phi = m_all(3*Ng+i,ind)            ! one by one
!                !phi = sum(m_all(2*Ng+i,1:N_e))/N_e  ! based on average
!                if (phi>0.225d0) then
!                    k=1
!                elseif(phi>0.2d0) then
!                    k=2
!                elseif(phi>0.175d0) then
!                    k=3
!                elseif(phi>0.15d0) then
!                    k=4
!                elseif(phi>0.125d0) then
!                    k=5
!                elseif(phi>0.075d0) then
!                    k=6
!                else
!                    k=7
!                end if            
!            else
!                k=0
!            end if
!            write(1,'(I1)') k
!        end do
!        WRITE(1,'(A1)') '/'
!        close(unit=1)
!    if (np_gl>0) then
!       FILE = trim(generic_dir_name) // '\'//trim(par_name(1,np_g+1))//'.dat'
!       OPEN (1, FILE=trim(FILE), STATUS='UNKNOWN')
!       WRITE(1,'(A)') 'SWOF'
!       sor  = 0.15d0
!       krwro= m_all(ng*np_g+1,ind)
!       krocw= 0.4d0!m_all(ng*np_g+2,ind)
!       n_w  = m_all(ng*np_g+2,ind)
!       n_o  = m_all(ng*np_g+3,ind)
!       do i=1,NTABLES
!           swc  = SAT_VALS(i,1)
!           do j=1,NSAT_TAB(i)-1
!              sw  = SAT_VALS(i,j) 
!              if (sw>(1.0d0-sor)) then
!                  kro =0.0d0
!                  krw = krwro + (1.0d0-krwro)/sor*(sw-1.0d0+sor)
!              elseif (sw == (1.0d0-sor)) then
!                  kro = 0.0d0
!                  krw = krwro
!              else
!                    kro = krocw*((1.0d0-sw-sor)/(1.0d0-swc-sor))**n_o
!                    krw = krwro*((sw-swc)/(1.0d0-swc-sor))**n_w
!              end if
!              write(1,'(f6.4,2E12.4,f13.5)')sw,krw,kro,PC_VALS(i,j) 
!           end do
!           sw = 1.0d0
!           krw = 1.0d0
!           kro = 0.0d0
!           write(1,'(f6.4,2E12.4,f13.5,A)')sw,krw,kro,PC_VALS(i,NSAT_TAB(i)),'  /'
!       end do
!       close(1)
!    end if
!    
! 5  format (5E20.8)
! 6  FORMAT(A5)
! 
! END SUBROUTINE W_INC_FILE
    
    
SUBROUTINE W_INC_FILE ()  ! 2D MODEL
    use global_variables, ONLY: cwd, generic_dir_name,NG,M_all,N_e,ACTLIST,nact, np_g, par_name, N_node, modelname,casename
    IMPLICIT NONE
    INTEGER :: i,j,ind,k,nn,e
    CHARACTER*20 :: FILENAME,LINE1
    CHARACTER*300            :: FILE
    double precision         :: m_all_unit_convert
    INTEGER                   :: unt_includefile
    CHARACTER*20                                :: L1,L2
    CHARACTER*256                           :: command
    
    Do e=1,N_e
        nn=0
        !N_node=10
        !FILENAME = 'grid_data.inc'
        do k = 1, Np_g
          
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
            
            unt_includefile = e
            FILE = trim(generic_dir_name) //'\SimFold'//'\Ensemble_'//trim(L2)//'\'//trim(par_name(1,k))//'.dat'
            OPEN (unit=unt_includefile, FILE=trim(FILE), STATUS='UNKNOWN')
            WRITE(unt_includefile,'(A)') trim(par_name(2,k))
            do i=1,NG*N_node
                !m_all_unit_convert=m_all(i+(k-1)*Ng,ind)*9.869233e-16 ! change unit from mD to m2
                if ((mod(i-1,Ng)==0) .and. (i.NE.1)) then
                    nn=nn+1
                endif
                m_all_unit_convert=m_all(i-nn*Ng+(k-1)*Ng,e)*9.869233e-16 ! change unit from mD to m2
                write(unt_includefile,11) i, i, '1', m_all_unit_convert, m_all_unit_convert, m_all_unit_convert
            end do
            WRITE(unt_includefile,'(A1)') ''
            close(unit=unt_includefile)
        end do

    end do
 5  format (5E20.8)
 6  FORMAT(A5)
 11 FORMAT(I8,I8,A8,e16.6,e16.6,e16.6)

 
 END SUBROUTINE W_INC_FILE