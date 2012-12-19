module sawlibf_test
    implicit none

contains
    subroutine assert(value)
        logical, intent(in) :: value

        if (value .ne. .true.) then
            call error('Assertion Fail')
        end if
    end subroutine

    subroutine error(message)
        use ifcore
        character(*) :: message

        call logmessage('ERROR',message)
        call tracebackqq()
        error stop
    end subroutine
    subroutine info(message)
        character(*) :: message

        call logmessage('INFO',message)
    end subroutine
    

    subroutine logmessage(level,message)
        use, intrinsic :: iso_fortran_env
        character(len=*),intent(in) :: level
        character(len=*),intent(in) :: message

        write(error_unit,*) level,": ",message
    end subroutine
end module
