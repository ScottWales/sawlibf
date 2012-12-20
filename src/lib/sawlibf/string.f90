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
        final :: delete_string 
    end type

    ! Constructors
    interface string
        procedure :: constructFromSize
        procedure :: constructFromString
        procedure :: constructFromCharacterString
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

        if (allocated(self%value)) then 
            deallocate(self%value)
        end if
    end subroutine

    subroutine copy(self,other)
        implicit none
        class(string),intent(inout) :: self
        class(string),intent(in) :: other

        if (allocated(self%value)) then 
            deallocate(self%value)
        end if
        self%length = other%length
        allocate(character(self%length)::self%value)
        self%value = other%value
    end subroutine
end module
