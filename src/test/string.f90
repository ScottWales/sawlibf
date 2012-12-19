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
        call assert(foo%length .eq. 9)
        foo%value = "abcd"
        call assert(foo%value .eq. "abcd")
    end subroutine
    subroutine initChar
        type(string) foo
        foo = string("abcd")
        call assert(foo%length .eq. 4)
        call assert(foo%value .eq. "abcd")
    end subroutine
    subroutine initString
        type(string) :: foo, bar
        bar = string("abcd")
        foo = string(bar)
        call assert(foo%length .eq. 4)
        call assert(foo%value(:) .eq. "abcd")
    end subroutine

end module

program test
    use test_string

    call declare()
    call initInt()
    call initChar()
    call initString()

end program
