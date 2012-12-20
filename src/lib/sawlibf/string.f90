! A simple string type
! Provides basic operations for dynamic strings

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
    function constructFromSize(value) result(self)
        integer,intent(in) :: value
        type(string) self

        self%length = value
        allocate(character(self%length)::self%value)
        self%value = char(0)
    end function

    ! Copy constructor
    function constructFromString(value) result(self)
        type(string),intent(in) :: value
        type(string) self

        self = constructFromSize(value%length)
        self%value = value%value
    end function

    ! Construct from a character(len=*) 
    function constructFromCharacterString(value) result(self)
        character(len=*),intent(in) :: value
        type(string) self

        self = constructFromSize(len(value))
        self%value = value
    end function

    subroutine delete_string(self)
        implicit none
        type(string),intent(inout) :: self

        self%length = 0

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
