    !SUBROUTINE CML(L,Nx,Ny,Nz,Dx,Dy,Dz,ax,ay,az,angle)
    !IMPLICIT NONE
    !INTEGER,INTENT(IN)                        :: Nx,Ny,Nz
    !double precision, INTENT(IN)              :: Dx,Dy,Dz(Nz),ax,ay,az,angle
    !double precision, INTENT(OUT)             :: L(2*Nx*NY*NZ,2*Nx*NY*NZ)!MUC(3*N,NE)
    !double precision                          :: CM(2*Nx*NY*NZ,2*Nx*NY*NZ)!MUC(3*N,NE)
    !double precision                          :: CK(Nx*NY*NZ,Nx*NY*NZ)
    !double precision                          :: SK,SKZ,SPHI,RKKZ,RKPHI,RKZPHI
    !INTEGER                                   :: i,j,N, info
    !
    !info  = 0
    !N = Nx*NY*NZ
    !SK=1.0d0
    !SPHI=0.03d0
    !RKPHI=0.8d0
    !
    !CALL CLNK(CK,Nx,Ny,Nz,Dx,Dy,Dz,ax,ay,az,angle)
    !CM(1:N,1:N)=SK**2 * CK(1:N,1:N)
    !CM(1:N,N+1:2*N)= RKPHI * SPHI * SK * CK(1:N,1:N)
    !CM(N+1:2*N,1:N)=RKPHI * SPHI * SK * CK(1:N,1:N)
    !CM(N+1:2*N,N+1:2*N)=SPHI**2 * CK(1:N,1:N)
    !
    !CALL choldc(2*N,CM,L)
    !
    !
    !
    !!call dpotrf( 'L', 2*N, L, 2*N, info )
    !if (info .NE. 0) then
    !    write (*,*) 'potrf routine failed to calculate Cholesky factorization and returned error', info
    !    stop       
    !end if
    !
    !END SUBROUTINE CML
    !
    !
    !SUBROUTINE CLNK(C,Nx,Ny,Nz,Dx,Dy,Dz,ax,ay,az,angle)
    !IMPLICIT NONE
    !INTEGER,INTENT(IN) :: Nx,Ny,Nz
    !double precision, INTENT(IN) :: Dx,Dy,Dz(Nz),ax,ay,az,angle
    !double precision, INTENT(OUT):: C(Nx*Ny*Nz,Nx*Ny*Nz)
    !double precision :: h,hi,hj,lx,ly,lz
    !double precision, parameter :: PI = 3.1415d0
    !INTEGER :: i,j,k,N,xi,yi,yt,zi,xj,yj,zj
    !
    !N = Nx*Ny*Nz
    !DO i=1,N
    !    yi=INT(i/Nx)           ! number of blocks in Y direction before the i th grid block
    !    IF((i-yi*Nx)==0) THEN  ! if the i index of the block is Nx
    !        xi=Nx
    !        yt=yi
    !    ELSE 
    !        xi=i-yi*Nx
    !        yt=yi+1
    !    END IF
    !    yi=yt
    !
    !    zi = INT(i/(Nx*Ny))
    !    IF((i-zi*Nx*Ny) .NE. 0) THEN  ! if the i index of the block is Nx
    !        zi = zi+1
    !    END IF
    !    hi = 0.0d0
    !    do k=2,zi
    !        hi = (Dz(k) + Dz(k-1)) / 2.0d0
    !    end do
    !
    !
    !    DO j=1,N
    !        yj=INT(j/Nx)
    !        IF((j-yj*Nx)==0) THEN
    !            xj=Nx
    !            yt=yj
    !        ELSE 
    !            xj=j-yj*Nx
    !            yt=yj+1
    !        END IF
    !        yj=yt
    !
    !        zj = INT(j/(Nx*Ny))
    !        IF((j-zi*Nx*Ny) .NE. 0) THEN  ! if the i index of the block is Nx
    !            zj = zj+1
    !        END IF
    !        hj = 0.0d0
    !        do k=2,zj
    !            hj = (Dz(k) + Dz(k-1)) / 2.0d0
    !        end do
    !        lx =  Dx*(xi-xj)*cos(angle*PI/180.0) + Dy*(yi-yj)*sin(angle*PI/180.0)
    !        ly = -Dx*(xi-xj)*sin(angle*PI/180.0) + Dy*(yi-yj)*cos(angle*PI/180.0)
    !        lz =  (hi-hj)
    !        h=((lx/ax)**2+(ly/ay)**2+(lz/az)**2)**0.5
    !        IF(h<=1.0d0) THEN
    !            C(i,j)=(1.0d0 - 1.5 * h + 0.5 * h**3)
    !        ELSE
    !            C(i,j)=0.0
    !        END IF
    !        !C(j,i)=C(i,j)      
    !    END DO
    !END DO
    !END SUBROUTINE CLNK