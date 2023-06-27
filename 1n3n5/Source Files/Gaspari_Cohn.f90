SUBROUTINE LOCALIZATION_MATRIX(rho) !,Nx,Ny,Nz,Dx,Dy,Dz,ax,ay,az,angle)
USE global_variables, ONLY: Nact,Nd,NG,local_var,d_loc,Nx,Ny,NZ,Dx,Dy,ACTINDX,N_e
IMPLICIT NONE
double precision, parameter             :: PI = 3.1415d0
double precision, INTENT(OUT)           :: RHO(Nact,Nd) ! GASPARI-COHEN COEFFICIENTS
double precision                        :: d_x,d_y,rho_i,angle,ax,ay,lx,ly,h
INTEGER                                 :: i,j,i_d,j_d,ii,jj,kk
DOUBLE PRECISION                        :: GASPARI_COHN, FURRER_BENGTSSON
DO i=1,Nd
    i_d = d_loc(i,1)
    j_d = d_loc(i,2)
    do j=1,Nact
        ii = ACTINDX(j,1)
        jj = ACTINDX(j,2)
        kk = ACTINDX(j,3)
        angle  = local_var(1,kk)*PI/180.0d0
        ax     = local_var(2,kk)
        ay     = local_var(3,kk)
        d_x  = (i_d-ii)*dx
        d_y  = (j_d-jj)*dy
        lx =  d_x*cos(angle) + d_y*sin(angle)
        ly = -d_x*sin(angle) + d_y*cos(angle)
        h=((lx/ax)**2+(ly/ay)**2)**0.5

        rho_i = GASPARI_COHN(h)
        !rho_i = FURRER_BENGTSSON(h,N_e)
        RHO(j,i) = rho_i
    end do
END DO
END SUBROUTINE LOCALIZATION_MATRIX   
FUNCTION GASPARI_COHN(h)
    IMPLICIT NONE
    DOUBLE PRECISION :: GASPARI_COHN,h
    IF(h<=1.0d0) THEN
        GASPARI_COHN = - 0.25d0 * (h)**5 + 0.5d0 * (h)**4 + 5.0d0 / 8.0d0 * (h)**3 - 5.0d0 / 3.0d0 * (h)**2 + 1.0d0
    ELSEIF (h<=2.0d0) THEN
        GASPARI_COHN=1.0d0 / 12.0d0 * (h)**5 - 1.0d0 / 2.0d0 * (h)**4 + 5.0d0 / 8.0d0 * (h)**3 + 5.0d0 / 3.0d0 * (h)**2 - 5.0d0 * (h) + 4.0d0 - 2.0d0 / 3.0d0 /h
    ELSE
        GASPARI_COHN=0.0d0
    END IF
END FUNCTION
FUNCTION FURRER_BENGTSSON(h,N_e)
    IMPLICIT NONE
    DOUBLE PRECISION :: FURRER_BENGTSSON,h,h0
    INTEGER          :: N_e
    double precision :: SPH_CORR
    h0 = 0.0d0
    FURRER_BENGTSSON = 1.0d0 / (1.0d0+(1.0d0+SPH_CORR(h0)**2/SPH_CORR(h)**2)/N_e)
END FUNCTION
FUNCTION SPH_CORR(h)
    IMPLICIT NONE
    DOUBLE PRECISION :: sph_corr,h
    IF(h<=1.0d0) THEN
        SPH_CORR = (1.0d0 - 1.5d0 * h + 0.5d0 * h**3)
    ELSE
        SPH_CORR=0.0d0
    END IF
END FUNCTION    
SUBROUTINE GASCOHN_CMD(Np,Nd,Nt,rho,d_loc) !,Nx,Ny,Nz,Dx,Dy,Dz,ax,ay,az,angle)
USE global_variables, ONLY: NG,ACTLIST,nact,ACTNUM,local_var,Nx,Ny,NZ,XCOORD,YCOORD,ZCOORD
IMPLICIT NONE
double precision, parameter             :: PI = 3.1415d0
double precision, INTENT(OUT)           :: RHO(Np,Nd*Nt) ! GASPARI-COHEN COEFFICIENTS
INTEGER, INTENT(IN)                     :: Nd,Nt,Np
double precision, Intent(in), dimension(Nd,3)    :: d_loc

double precision                        :: xi,yi,zi,xj,yj,zj,angle,ax,ay,lx,ly,lz,rho_i,hi,h
INTEGER                                 :: i,j,k,N,ii,jj,kk


!angle = 53.92d0-90.d0!55.36d0
!ax = 2.0*1714.0d0!1371.d0
!ay = 2.0*151.0d0!138.d0
!az = 5.0d0*sum(dz)/Nz

N=Nx*Ny*Nz

DO i=1,Nd
        xi = d_loc(i,1)
        yi = d_loc(i,2)
        zi = d_loc(i,3)
        hi = 0.0d0
        k = 0 !!!!! parameter counter
        do ii=1,nx
            do jj=1,ny
                do kk = 1,nz
                    j = (kk-1)*nx*ny + (jj-1)*nx + ii !!! gridnumber
                    if (ACTNUM(j) == 1) then
                        k = k+1
                        xj = XCOORD(j)
                        yj = YCOORD(j)
                        angle  = local_var(1,kk)*PI/180.0d0
                        ax  = local_var(2,kk)
                        ay  = local_var(3,kk)
                        
                        lx =  (xi-xj)*cos(angle) + (yi-yj)*sin(angle)
                        ly = -(xi-xj)*sin(angle) + (yi-yj)*cos(angle)
                        h=((lx/(2.0d0*ax))**2+(ly/(2.0d0*ay))**2)**0.5

                        IF(h<=1.0d0) THEN
                            rho_i = - 0.25d0 * (h)**5 + 0.5d0 * (h)**4 + 5.0d0 / 8.0d0 * (h)**3 - 5.0d0 / 3.0d0 * (h)**2 + 1.0d0
                        ELSEIF (h<=2.0d0) THEN
                            rho_i=1.0d0 / 12.0d0 * (h)**5 - 1.0d0 / 2.0d0 * (h)**4 + 5.0d0 / 8.0d0 * (h)**3 + 5.0d0 / 3.0d0 * (h)**2 - 5.0d0 * (h) + 4.0d0 - 2.0d0 / 3.0d0 /h
                        ELSE
                            rho_i=0.0d0
                        END IF
                        
                        RHO(k,(i-1)*Nt+1:i*Nt) = rho_i
                        RHO(k+nact,(i-1)*Nt+1:i*Nt) = rho_i
                        RHO(k+2*nact,(i-1)*Nt+1:i*Nt) = rho_i
                        
                    end if
                end do
            end do
        end do
    END DO
END SUBROUTINE GASCOHN_CMD
SUBROUTINE GASCOHN_CDD(Nd,Nt,rho,d_loc,ax,ay,az,angle)
IMPLICIT NONE
double precision, parameter             :: PI = 3.1415d0
double precision, INTENT(OUT)           :: RHO(Nd*Nt,Nd*Nt) ! GASPARI-COHEN COEFFICIENTS
INTEGER, INTENT(IN)                     :: Nd,Nt
Double precision, intent(in)            :: ax,ay,az,angle
INTEGER                                 :: NX,NY,NZ
double precision                        :: DX,DY,DZ(3)!,ax,ay,az,angle
double precision, Intent(in), dimension(Nd,3)    :: d_loc

double precision                        :: a,h,hi,hj,lx,ly,lz
INTEGER                                 :: i,j,k,N,xi,yi,yt,zi,xj,yj,zj

NX = 64
NY = 64
Nz = 3
Dx = 100.0d0
Dy = 100.0d0
Dz = 20.0d0
!angle = 53.92d0-90.d0!55.36d0
!ax = 2.0*1714.0d0!1371.d0
!ay = 2.0*151.0d0!138.d0
!az = 5.0d0*sum(dz)/Nz

N=Nx*Ny*Nz

DO i=1,Nd
        xi = d_loc(i,1)
        yi = d_loc(i,2)
        zi = d_loc(i,3)
        hi = 0.0d0
        do k=2,zi
            hi = (Dz(k) + Dz(k-1)) / 2.0d0
        end do
        

        DO j=i,Nd
            xj = d_loc(j,1)
            yj = d_loc(j,2)
            zj = d_loc(j,3)
            hj = 0.0d0
            do k=2,zj
                hj = (Dz(k) + Dz(k-1)) / 2.0d0
            end do
            
            
            lx =  Dx*(xi-xj)*cos(angle*PI/180.0) + Dy*(yi-yj)*sin(angle*PI/180.0)
            ly = -Dx*(xi-xj)*sin(angle*PI/180.0) + Dy*(yi-yj)*cos(angle*PI/180.0)
            lz =  (hi-hj)
            h=((lx/ax)**2+(ly/ay)**2+(lz/az)**2)**0.5
            
            IF(h<=1.0d0) THEN
                RHO((j-1)*Nt+1:j*Nt,(i-1)*Nt+1:i*Nt) = - 0.25d0 * (h)**5 + 0.5d0 * (h)**4 + 5.0d0 / 8.0d0 * (h)**3 - 5.0d0 / 3.0d0 * (h)**2 + 1.0d0
            ELSEIF (h<=2.0d0*a) THEN
                RHO((j-1)*Nt+1:j*Nt,(i-1)*Nt+1:i*Nt)=1.0d0 / 12.0d0 * (h)**5 - 1.0d0 / 2.0d0 * (h)**4 + 5.0d0 / 8.0d0 * (h)**3 + 5.0d0 / 3.0d0 * (h)**2 - 5.0d0 * (h) + 4.0d0 - 2.0d0 / 3.0d0 /h
            ELSE
                RHO((j-1)*Nt+1:j*Nt,(i-1)*Nt+1:i*Nt)=0.0d0
            END IF
            RHO((i-1)*Nt+1:i*Nt,(j-1)*Nt+1:j*Nt) = RHO((j-1)*Nt+1:j*Nt,(i-1)*Nt+1:i*Nt)
        END DO
    END DO
END SUBROUTINE GASCOHN_CDD    