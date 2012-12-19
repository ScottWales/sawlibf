module sawlibf_string
implicit none
    type string
        character,dimension(:),allocatable :: value
        integer :: length
        integer :: availableSize
    contains
        final :: delete_string 
    end type
    interface string
        procedure :: constructFromSize
        procedure :: constructFromString
        procedure :: constructFromCharacterString
    end interface
contains
    function constructFromSize(value) result(self)
        integer,intent(in) :: value
        type(string) self

        self%length = 0
        self%availableSize = value
        allocate(self%value(self%availableSize))
        self%value = '\0'
    end function

    function constructFromString(value) result(self)
        type(string),intent(in) :: value
        type(string) self

        self = constructFromSize(value%length)
        self%length = value%length
        self%value = value%value
    end function

    function constructFromCharacterString(value) result(self)
        character(len=*),intent(in) :: value
        type(string) self

        self = constructFromSize(len(value))
        self%value = value
    end function

    subroutine delete_string(self)
        implicit none
        type(string),intent(inout) :: self

        self%availableSize = 0
        self%length = 0

        if (allocated(self%value)) then 
            deallocate(self%value)
        end if
    end subroutine
end module
