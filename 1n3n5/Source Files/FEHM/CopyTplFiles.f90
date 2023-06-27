SUBROUTINE CopyTplFIles ()  ! 2D MODEL
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
        
        !command = 'COPY /Y "'//trim(cwd)//'\'//trim(generic_dir_name)//'\tpl\'//'run_presCO2.his" "'//trim(cwd)//'\outputs_'//trim(modelname)//'\out_'//trim(L2)//'" >nul'
        command = 'COPY /Y "'//trim(cwd)//'\'//trim(generic_dir_name)//'\tpl\'//trim(casename)//'.dat" "'//trim(cwd)//'\'//trim(generic_dir_name)//'\SimFold'//'\Ensemble_'//trim(L2)//'" >nul'
        call system(trim(command))
        command = 'COPY /Y "'//trim(cwd)//'\'//trim(generic_dir_name)//'\tpl\'//trim(casename)//'_post'//'.dat" "'//trim(cwd)//'\'//trim(generic_dir_name)//'\SimFold'//'\Ensemble_'//trim(L2)//'" >nul'
        call system(trim(command))
        command = 'COPY /Y "'//trim(cwd)//'\'//trim(generic_dir_name)//'\tpl\'//trim(casename)//'_prior'//'.dat" "'//trim(cwd)//'\'//trim(generic_dir_name)//'\SimFold'//'\Ensemble_'//trim(L2)//'" >nul'
        call system(trim(command))
        
        command = 'COPY /Y "'//trim(cwd)//'\'//trim(generic_dir_name)//'\tpl\'//'fehmn.files" "'//trim(cwd)//'\'//trim(generic_dir_name)//'\SimFold'//'\Ensemble_'//trim(L2)//'" >nul'
        call system(trim(command))
        command = 'COPY /Y "'//trim(cwd)//'\'//trim(generic_dir_name)//'\tpl\'//'fehmn_post.files" "'//trim(cwd)//'\'//trim(generic_dir_name)//'\SimFold'//'\Ensemble_'//trim(L2)//'" >nul'
        call system(trim(command))
        command = 'COPY /Y "'//trim(cwd)//'\'//trim(generic_dir_name)//'\tpl\'//'fehmn_prior.files" "'//trim(cwd)//'\'//trim(generic_dir_name)//'\SimFold'//'\Ensemble_'//trim(L2)//'" >nul'
        call system(trim(command))
        
        command = 'COPY /Y "'//trim(cwd)//'\'//trim(generic_dir_name)//'\tpl\'//'co2_inj.txt" "'//trim(cwd)//'\'//trim(generic_dir_name)//'\SimFold'//'\Ensemble_'//trim(L2)//'" >nul'
        call system(trim(command))

    end do
 5  format (5E20.8)
 6  FORMAT(A5)
 11 FORMAT(I8,I8,A8,e16.6,e16.6,e16.6)

 
 END SUBROUTINE CopyTplFIles