module test_string
    use sawlibf_string
    use sawlibf_test
    implicit none

contains
    subroutine initChar
        type(string) foo
        foo = string("abcd")
        call assert(foo .eq. "abcd")
    end subroutine
    subroutine initString
        type(string) :: foo, bar
        bar = string("abc")
        foo = string(bar)
        bar = string("def")
        call assert(foo .eq. "abc")
        call assert(bar .eq. "def")
    end subroutine
    subroutine copy
        type(string) :: foo,bar
        bar = string("abcd")
        foo = string("efg")

        foo = bar
        bar = 'def'
        call assert(foo .eq. "abcd")
        call assert(bar .eq. "def")
    end subroutine
    subroutine append
        type(string) :: foo,bar
        foo = string("abc")
        bar = string("def")

        foo =foo .append. bar
        call assert(foo .eq. "abcdef")
        call assert(bar .eq. "def")

        bar = bar .append. "ghi"
        call assert(bar .eq. "defghi")
    end subroutine
    subroutine equal
        type(string) :: foo,bar
        foo = string("abc")
        bar = string("def")

        call assert(.not. (foo .eq. bar))
        call assert(foo .eq. "abc")
        call assert("abc" .eq. foo)
        foo = bar
        call assert(foo .eq. bar)
    end subroutine
    subroutine tochar
        type(string) :: foo
        character(len=3) :: bar

        foo = "abc"
        bar = foo

        call assert(bar .eq. "abc")
    end subroutine
end module

program test
    use test_string

    call initChar()
    call initString()
    call copy()
    call append()
    call equal()
    call tochar()

end program
