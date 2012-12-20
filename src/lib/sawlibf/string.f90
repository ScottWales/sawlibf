! A simple string type
! Provides basic operations for dynamic strings
!
! Interface:
! ----------
! string(integer n) -> string
!   Create a string with space to hold n characters
! string(character(len=*) c) -> string
!   Create a new string and initialise with value of c
! string(string s) -> string
!   Copy constructor
! string%append(string s) -> string
!   Append s to the end of this

module sawlibf_string
implicit none
    type string
        character(len=:),allocatable :: value !< Character data
        integer :: length !< The allocated length
    contains
        procedure :: appendString
        procedure :: appendCharacterString
        generic :: append => appendString, &
                             appendCharacterString
        final :: delete_string 
    end type

    ! Constructors
    interface string
        procedure :: constructFromSize
        procedure :: constructFromString
        procedure :: constructFromCharacterString
    end interface
    interface assignment(=)
        procedure :: copy
    end interface
contains
    ! Construct a string given an initial size
    function constructFromSize(value) result(this)
        integer,intent(in) :: value
        type(string) this

        this%length = value
        allocate(character(this%length)::this%value)
        this%value = char(0)
    end function

    ! Copy constructor
    function constructFromString(value) result(this)
        type(string),intent(in) :: value
        type(string) this

        this = constructFromSize(value%length)
        this%value = value%value
    end function

    ! Construct from a character(len=*) 
    function constructFromCharacterString(value) result(this)
        character(len=*),intent(in) :: value
        type(string) this

        this = constructFromSize(len(value))
        this%value = value
    end function

    subroutine delete_string(this)
        implicit none
        type(string),intent(inout) :: this

        this%length = 0
        if (allocated(this%value)) then 
            deallocate(this%value)
        end if
    end subroutine

    subroutine appendString(this,other)
        implicit none
        class(string),intent(inout) :: this
        class(string),intent(in) :: other
        character(len=this%length) :: tmp

        tmp = this%value(:)

        this%length = this%length + other%length
        if (allocated(this%value)) then 
            deallocate(this%value)
        end if
        allocate(character(this%length)::this%value)
        this%value(1:len(tmp)) = tmp
        this%value(len(tmp)+1:this%length) = other%value
    end subroutine
    subroutine appendCharacterString(this,other)
        implicit none
        class(string),intent(inout) :: this
        character(len=*),intent(in) :: other

        call this%appendString(string(other))
    end subroutine
    subroutine copy(this,other)
        implicit none
        class(string),intent(inout) :: this
        class(string),intent(in) :: other

        if (allocated(this%value)) then
            deallocate(this%value)
        end if
        this%length=other%length
        allocate(character(this%length)::this%value)
        this%value = other%value
    end subroutine
end module
