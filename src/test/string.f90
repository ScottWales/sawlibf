module test_string
    use sawlibf_string
    use sawlibf_test
    implicit none

contains
    subroutine declare
        type(string) foo
    end subroutine
    subroutine initInt
        type(string) foo

        foo = string(9)
        
        call assert(foo%length .eq. 1)

    end subroutine

end module

program test
    use test_string

    call declare()
    call initInt()

end program
