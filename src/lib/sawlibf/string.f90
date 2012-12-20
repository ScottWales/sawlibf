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
!   Sets the string a equal to string b
! a .eq. b
!   Returns true if string a is equal to string b
!
! call string%append(string s)
!   Append s to the end of this

module sawlibf_string
implicit none
    type string
        character(len=:),allocatable,private :: value
        integer,private :: length
    contains
        procedure :: appendString
        procedure :: appendCharacterString
        generic :: append => appendString, &
                             appendCharacterString
        procedure :: equalString
        procedure :: equalCharacterString
        generic :: operator(.eq.) => equalString, &
                                     equalCharacterString
        final :: delete_string 
    end type

    ! Constructors
    interface string
        procedure :: constructFromSize
        procedure :: constructFromString
        procedure :: constructFromCharacterString
    end interface
    interface assignment(=)
        procedure :: copyString
        procedure :: copyCharacterString
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

    subroutine copyString(this,other)
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
    subroutine copyCharacterString(this,other)
        implicit none
        type(string),intent(inout) :: this
        character(*),intent(in) :: other

        call copyString(this,string(other))
    end subroutine

    function equalString(this,other) result(equal)
        implicit none
        class(string),intent(in) :: this
        class(string),intent(in) :: other
        logical :: equal

        equal = this%value .eq. other%value
    end function
    function equalCharacterString(this,other) result(equal)
        implicit none
        class(string),intent(in) :: this
        character(len=*),intent(in) :: other
        logical :: equal

        equal = this%value .eq. other
    end function
end module
