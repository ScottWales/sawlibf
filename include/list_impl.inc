! vim: set filetype=fortran:
! A generic singly-linked list
!
! Usage:
!    module modvalue
!        type value
!            ...
!        end type
!    end module
!    module modlist
!        use modvalue, value_type => value
!        include 'list_impl.inc'
!    end module

    type list
        type(list), pointer :: next
        type(value_type) :: value
    contains
        final :: delete_list
        procedure :: addList
        procedure :: addValue
        generic :: add => addList, addValue
    end type

    interface list
        procedure :: construct
    end interface
contains
    subroutine delete_list(this)
        type(list), intent(inout) :: this
    end subroutine

    function construct(value) result(this)
        type(list) :: this
        type(value_type), intent(in) :: value

        this%value = value
        nullify(this%next)
    end function

    subroutine addValue(this, new)
        class(list), intent(inout) :: this
        class(value_type), intent(in) :: new
        type(list) :: newlink
        
        newlink = list(new)
        call addList(this,newlink)
    end subroutine

    subroutine addList(this, new)
        class(list), intent(inout) :: this
        class(list), intent(in), target :: new

        this%next => new
    end subroutine
