SUBROUTINE fehm_exe_gen ()  ! 2D MODEL
    use global_variables, ONLY: fehm_ver,cwd, generic_dir_name,NG,M_all,N_e,ACTLIST,nact, np_g, par_name, N_node, modelname,casename
    IMPLICIT NONE
    INTEGER :: i,j,ind,k,nn,e
    CHARACTER*20 :: FILENAME,LINE1
    CHARACTER*300            :: FILE
    double precision         :: m_all_unit_convert
    INTEGER                   :: unt_includefile
    CHARACTER*20                                :: L1,L2
    CHARACTER*256                           :: command
    
    Do e=1,N_e
        if (e<10) then
            write(L2,'(I1)') e
        elseif (e<100) then
            write(L2,'(I2)') e
        elseif (e<1000) then
            write(L2,'(I3)') e
        else
            write(L2,'(I4)') e
        end if
        
        command = 'COPY /Y "'//trim(cwd)//'\'//trim(fehm_ver)//'.exe" "'//trim(cwd)//'\fehm_exe'//'" >nul'
        call system(trim(command))
        command = 'ren "'//trim(cwd)//'\fehm_exe\'//trim(fehm_ver)//'.exe" "'//trim(fehm_ver)//'_'//trim(L2)//'.exe"'
        call system(trim(command))

    end do

 END SUBROUTINE fehm_exe_gen