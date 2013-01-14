module wrapped_int
    type wrapper
        integer :: i
    end type
end module

module modlist
    use wrapped_int, value_type => wrapper
    include 'list_impl.inc'
end module


module test_list
    use modlist
    use sawlibf_test
    implicit none

contains
    subroutine init
        type(list) foo
        type(value_type) bar

        bar%i = 1
        foo = list(bar)
        call assert(.not. associated(foo%next))
        call assert(foo%value%i .eq. 1)
    end subroutine
    subroutine add
        type(list) foo
        type(value_type) bar, baz

        bar%i = 1
        baz%i = 2

        foo = list(bar)
        call foo%add(baz)

        baz%i = 3
        call assert(foo%next%value%i .eq. 2)
    end subroutine
end module

program test
    use test_list

    call init

end program
