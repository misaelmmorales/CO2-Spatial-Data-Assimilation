subroutine find_alpha_geometric(Na,a)
    implicit none
    integer :: Na,i
    double precision, dimension(Na), intent(inout) :: a
    double precision :: r,rp,rm,f,fp,fm
    rp = 0.98d0
    rm = 0.01d0
    r = rm;
    
    if (a(1)<10) then
        a(1)=10
    end if
    
    fm= 1.0d0/(r**Na)-a(1)/r+a(1)-1.0d0
    r = rp;
    fp= 1.0d0/(r**Na)-a(1)/r+a(1)-1.0d0
    f = 1.0d0
    do
        r = (rm+rp)/2.0d0
        f = 1.0d0/(r**Na)-a(1)/r+a(1)-1.0d0
        if (f*fp > 0.0d0) then
            fp = f
            rp = r
        elseif (f*fm > 0.0d0) then
            fm = f
            rm = r
        else
            exit
        end if
        
        if (abs(f)<0.000001d0) exit
    end do
    
    do i = 2,Na
        a(i) = r*a(i-1)
    end do
end subroutine find_alpha_geometric
subroutine find_alpha_log10(Na,a)
    implicit none
    integer :: Na,i
    double precision, dimension(Na), intent(inout) :: a
    double precision :: a_sum,stp,l_sum,tmp
    a_sum = 1/a(1)
    stp   = -log10(a_sum)/(Na-1.0d0)
    l_sum = Log10(a_sum)
                    
    do i=2,Na
        tmp = 10.0d0**(l_sum+stp)-a_sum
        a(i) = 1.0d0/tmp
        a_sum = a_sum + tmp
        l_sum = log10(a_sum)
    end do
end subroutine find_alpha_log10    
subroutine find_alpha_ln(Na,a)
    implicit none
    integer :: Na,i
    double precision, dimension(Na), intent(inout) :: a
    double precision :: a_sum,stp,l_sum,tmp
    a_sum = 1/a(1)
    stp   = -log(a_sum)/(Na-1.0d0)
    l_sum = Log(a_sum)
                    
    do i=2,Na
        tmp = EXP(l_sum+stp)-a_sum
        a(i) = 1.0d0/tmp
        a_sum = a_sum + tmp
        l_sum = log(a_sum)
    end do
end subroutine find_alpha_ln     

subroutine find_alpha_geo2(Na,a)
    implicit none
    integer :: Na,i
    double precision, dimension(Na), intent(inout) :: a
    double precision :: r,rp,rm,f,fp,fm
    a (Na) =2.0d0
    rp = 0.999d0
    rm = 0.0001d0
    r = rm;
    fm= 1.0d0/(r**(Na-1.0d0))+(1.0d0-1.0d0/r)*a(1)*(1.0d0-1.0d0/a(Na))
    r = rp;
    fp= 1.0d0/(r**(Na-1.0d0))+(1.0d0-1.0d0/r)*a(1)*(1.0d0-1.0d0/a(Na))
    f = 1.0d0
    do
        r = (rm+rp)/2.0d0
        f = 1.0d0/(r**(Na-1.0d0))+(1.0d0-1.0d0/r)*a(1)*(1.0d0-1.0d0/a(Na))
        if (f*fp > 0.0d0) then
            fp = f
            rp = r
        elseif (f*fm > 0.0d0) then
            fm = f
            rm = r
        else
            exit
        end if
        
        if (abs(f)<0.000001d0) exit
    end do
    
    do i = 2,Na-1
        a(i) = r*a(i-1)
    end do
end subroutine find_alpha_geo2
subroutine find_alpha_trace(N,S,alpha)
    implicit none
    integer                        :: N
    double precision, dimension(N) :: S
    double precision               :: alpha
    double precision               :: f, fp
    integer                        :: i
    
    alpha = 1.0d0
    do
        f  = 1.0d0
        fp = 0.0d0
        do i=1,N
            f = f - 1/(s(i)**2+alpha)
            fp= fp+ 1/(s(i)**2+alpha)**2
        end do
        alpha = alpha - f/fp
        if (abs(f) <1.0d-6) exit
    end do
    
    
end subroutine find_alpha_trace    