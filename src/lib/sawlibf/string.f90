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
!
! a = b
!   Assigns the value of one string to another, or assigns between strings and
!   character(len=*)
! a .eq. b -> logical
!   Returns true if string/character(len=*) a and b are equal
! a .append. b -> string
!   Returns a string with b appended to a
!
! string%length -> integer
!   Returns the length of the string
! 
! Converting to character(len=*):
!     type(string) :: foo
!     character(len=:),allocatable ::bar
!     integer :: length
!     foo = "abc"
!     length = foo%length
!     allocate(character(length)::bar)
!     bar = foo
!     ! ...
!     deallocate(bar)


module sawlibf_string
implicit none
    private
    type string
        character(len=:),allocatable,private :: value
        integer,private :: valuelength
    contains
        procedure :: length

        final :: delete_string 
    end type

    ! Constructors
    interface string
        procedure :: constructFromString
        procedure :: constructFromCharacterString
    end interface

    ! Operators
    interface assignment(=)
        procedure :: copyString
        procedure :: copyCharacterString
        procedure :: toCharacterString
    end interface
    interface operator(.eq.)
        procedure :: equalString
        procedure :: equalCharacterString
        procedure :: characterStringEqual
    end interface
    interface operator(.append.)
        procedure :: appendString
        procedure :: appendCharacterString
        procedure :: characterStringAppend
    end interface

    public :: string
    public :: assignment(=)
    public :: operator(.eq.)
    public :: operator(.append.)
contains
    ! Construct a string given an initial size
    function constructFromSize(value) result(this)
        integer,intent(in) :: value
        type(string) this

        this%valuelength = value
        allocate(character(this%valuelength)::this%value)
        this%value = char(0)
    end function

    ! Copy constructor
    function constructFromString(value) result(this)
        type(string),intent(in) :: value
        type(string) this

        this = constructFromSize(value%valuelength)
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

        this%valuelength = 0
        if (allocated(this%value)) then 
            deallocate(this%value)
        end if
    end subroutine

    function appendString(lhs,rhs) result(this)
        implicit none
        class(string),intent(in) :: lhs, rhs
        character(len=lhs%valuelength) :: tmp
        type(string) :: this

        tmp = lhs%value(:)
        this = lhs
        this%valuelength = this%valuelength + rhs%valuelength
        if (allocated(this%value)) then 
            deallocate(this%value)
        end if
        allocate(character(this%valuelength)::this%value)
        this%value(1:len(tmp)) = tmp
        this%value(len(tmp)+1:this%valuelength) = rhs%value
    end function
    function appendCharacterString(lhs,rhs) result(this)
        implicit none
        class(string),intent(in) :: lhs
        character(len=*),intent(in) :: rhs
        type(string) :: this

        this = lhs .append. string(rhs)
    end function
    function characterStringAppend(lhs,rhs) result(this)
        implicit none
        character(len=*),intent(in) :: lhs
        class(string),intent(in) :: rhs
        type(string) :: this

        this = string(lhs) .append. rhs
    end function

    subroutine copyString(this,other)
        implicit none
        class(string),intent(inout) :: this
        class(string),intent(in) :: other

        if (allocated(this%value)) then
            deallocate(this%value)
        end if
        this%valuelength=other%valuelength
        allocate(character(this%valuelength)::this%value)
        this%value = other%value
    end subroutine
    subroutine copyCharacterString(this,other)
        implicit none
        type(string),intent(inout) :: this
        character(*),intent(in) :: other

        call copyString(this,string(other))
    end subroutine
    subroutine toCharacterString(this,other)
        implicit none
        character(len=*),intent(out) :: this
        type(string),intent(in) :: other

        this = other%value
    end subroutine

    elemental function equalString(lhs,rhs) result(equal)
        implicit none
        class(string),intent(in) :: lhs
        class(string),intent(in) :: rhs
        logical :: equal

        equal = lhs%value .eq. rhs%value
    end function
    elemental function equalCharacterString(lhs,rhs) result(equal)
        implicit none
        class(string),intent(in) :: lhs
        character(len=*),intent(in) :: rhs
        logical :: equal

        equal = lhs%value .eq. rhs
    end function
    elemental function characterStringEqual(lhs,rhs) result(equal)
        implicit none
        character(len=*),intent(in) :: lhs
        class(string),intent(in) :: rhs
        logical :: equal

        equal = lhs .eq. rhs%value
    end function

    elemental function length(this) 
        implicit none
        class(string),intent(in) :: this
        integer :: length
        length = this%valuelength
    end function
end module
