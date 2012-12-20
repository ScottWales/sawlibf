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
! a .eq. b
!   Returns true if string a is equal to string or character(len=*) b
!
! call string%append(string s)
!   Append s to the end of the string
! string%length -> integer
!   Returns the length of the string
! 
! Converting to character(len=*):
!     type(string) :: foo
!     character(len=:),allocatable ::bar
!     integer :: length
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
        procedure :: appendString
        procedure :: appendCharacterString
        generic :: append => appendString, &
                             appendCharacterString

        procedure :: equalString
        procedure :: equalCharacterString
        generic :: operator(.eq.) => equalString, &
                                     equalCharacterString

        procedure :: length

        final :: delete_string 
    end type

    ! Constructors
    interface string
        procedure :: constructFromString
        procedure :: constructFromCharacterString
    end interface
    interface assignment(=)
        procedure :: copyString
        procedure :: copyCharacterString
        procedure :: toCharacterString
    end interface

    public :: string
    public :: assignment(=)
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

    subroutine appendString(this,other)
        implicit none
        class(string),intent(inout) :: this
        class(string),intent(in) :: other
        character(len=this%valuelength) :: tmp

        tmp = this%value(:)

        this%valuelength = this%valuelength + other%valuelength
        if (allocated(this%value)) then 
            deallocate(this%value)
        end if
        allocate(character(this%valuelength)::this%value)
        this%value(1:len(tmp)) = tmp
        this%value(len(tmp)+1:this%valuelength) = other%value
    end subroutine

    subroutine appendCharacterString(this,other)
        implicit none
        class(string),intent(inout) :: this
        character(len=*),intent(in) :: other

        call this%appendString(string(other))
    end subroutine

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

    elemental function equalString(this,other) result(equal)
        implicit none
        class(string),intent(in) :: this
        class(string),intent(in) :: other
        logical :: equal

        equal = this%value .eq. other%value
    end function
    elemental function equalCharacterString(this,other) result(equal)
        implicit none
        class(string),intent(in) :: this
        character(len=*),intent(in) :: other
        logical :: equal

        equal = this%value .eq. other
    end function

    elemental function length(this) 
        implicit none
        class(string),intent(in) :: this
        integer :: length
        length = this%valuelength
    end function
end module
