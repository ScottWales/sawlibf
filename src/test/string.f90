module test_string
    use sawlibf_string, only: string
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
        bar%value = "hijk"
        call assert(foo%length .eq. 4)
        call assert(foo%value(:) .eq. "abcd")
    end subroutine
    subroutine copy
        type(string) :: foo,bar
        bar = string("abcd")
        foo = string("efg")

        foo = bar
        call assert(foo%value(:) .eq. "abcd")
        bar%value = "hijk"
        call assert(foo%value(:) .eq. "abcd")
    end subroutine
    subroutine append
        type(string) :: foo,bar
        foo = string("abc")
        bar = string("def")

        call foo%append(bar)
        call assert(foo%value(:) .eq. "abcdef")
        call assert(bar%value(:) .eq. "def")

        call bar%append("ghi")
        call assert(bar%value(:) .eq. "defghi")
    end subroutine

end module

program test
    use test_string

    call declare()
    call initInt()
    call initChar()
    call initString()
    call copy()
    call append()

end program
