subroutine make_dir_modelrun()
    use global_variables, ONLY: N_e, cwd,modelname
    implicit none
    integer         :: e,i
    character*20    :: L1,L2
    character*1000 :: command
    logical :: dir_e,file_del   
    !integer, intent(in) :: N_e               ! number of ensemble members
    
    command  = trim(cwd)//'\Models\'//trim(modelname)//'\'//'\SimFold'//'\'
    inquire(DIRECTORY=trim(command), exist=dir_e)

    if ( dir_e ) then
        do e = 1, N_e
            if (e<10) then
                write(L1,'(I1)') e
                L1  = '000'//trim(L1)
            elseif (e<100) then
                write(L1,'(I2)') e
                L1  = '00'//trim(L1)
            elseif (e<1000) then
                write(L1,'(I3)') e
                L1  = '0'//trim(L1)
            else
                write(L1,'(I4)') e
            end if
            
            command = 'mkdir '//trim(cwd)//'\Models\'//trim(modelname)//'\SimFold'//'\Ensemble_'//trim(L1)
            call system(trim(command))
        end do
    else
        do e = 1, N_e
            if (e<10) then
                write(L1,'(I1)') e
                L1  = '000'//trim(L1)
            elseif (e<100) then
                write(L1,'(I2)') e
                L1  = '00'//trim(L1)
            elseif (e<1000) then
                write(L1,'(I3)') e
                L1  = '0'//trim(L1)
            else
                write(L1,'(I4)') e
            end if
            
            command = 'mkdir '//trim(cwd)//'\Models\'//trim(modelname)//'\SimFold'//'\Ensemble_'//trim(L1)
            call system(trim(command))
        end do
    end if
        
end subroutine