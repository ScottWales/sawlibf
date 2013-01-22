! The Quad-Edge data structure of Guibas and Stolfi (1983)
!
! A single edge is represented by an index into the adjacency table `onext`. New
! edges are created with the `makeEdge` class function.
!
! Each edge has an origin point, a destination point, a left face and a right
! face. Each edge also has a corresponding edge in the dual graph, whose left
! face corresponds to the origin point, right face to the destination point and
! origin and destination points to the original left and right faces
! respectively. The dual of the dual edge is the original edge pointing in the
! opposite direction.
!
! Edge adjacency information is stored in the array `onext`. Convert G&S's
! indexing to an index into onext with
!   index = e*4 + r
! (Edge indices start at 0)
!
! A large amount of the operators defined by G&S are made available. Sym, Rot
! and Rot^{-1} are defined as free functions operating on an index, next and
! prev functions are class functions so that they can access the onext array.
!
! Other operators include makeEdge, splice, connect and deleteEdge.
!
! The structure allows for storing data through face/vertex and edge index
! arrays `facedata` and `edgedata`. Original and dual spaces are stored in the
! same array: vertices and Voronoi edges are stored in columns r=0,2 while faces
! and Delaunay edges are stored in columns r=1,3. 
!

module sawlibf_quadedge
    implicit none
    private 
    public :: quadedge
    public :: sym, rot, irot
    public :: leftSide, rightSide

    type quadedge
        ! Adjacency information
        integer, dimension(:), allocatable :: onext
        integer :: onext_size      ! Space used in onext
        integer :: onext_available ! Space available in onext

        ! Indices for data access
        integer, dimension(:), allocatable :: dataindex
    contains
        procedure :: makeEdge
        procedure :: splice
        procedure :: connect
        procedure :: deleteEdge

        ! Edge transformations
        ! onext is accessed with the same interface
        procedure :: lnext
        procedure :: rnext
        procedure :: dnext
        procedure :: lprev
        procedure :: rprev
        procedure :: dprev
        procedure :: oprev

        ! dataindex accessors
        procedure :: org
        procedure :: dest
        procedure :: left
        procedure :: right
    end type

    interface quadedge
        procedure :: init
    end interface

    type side
        logical :: isLeft
    end type
    type(side), parameter :: leftSide = side(.true.)
    type(side), parameter :: rightSide = side(.false.)
contains
    function init result(this)
        type(quadedge) :: this

        this%onext_size = 0
        this%onext_available = 8
        allocate(this%onext(0:this%onext_available-1))
        allocate(this%facedata(0:this%onext_available-1))
        allocate(this%edgedata(0:this%onext_available-1))
    end function
    subroutine reallocData(this)
        type(quadedge), intent(inout) :: this
        integer, dimension(:), allocatable :: temp
        
        if (this%onext_size .ge. this%onext_available) then
            this%onext_available = this%onext_available * 2
        end if
        allocate(temp(0:this%onext_available-1))
        temp(0:size(this%onext)-1) = this%onext
        call move_alloc(temp,this%onext)
    end subroutine

    ! Create a new edge E with no connections
    function makeEdge(this) result(e)
        class(quadedge), intent(inout) :: this
        integer :: e

        e = this%onext_size
        this%onext_size = this%onext_size + 4
        call reallocData(this)
        
        this%onext(e)       =  e
        this%onext(sym(e))  =  sym(e)
        this%onext(irot(e)) = irot(sym(e))
        this%onext(rot(e))  =  rot(sym(e))
    end function

    ! Create or destroy a link between the origin of A and the origin of B
    subroutine splice(this, a, b)
        class(quadedge), intent(inout) :: this
        integer, intent(in) :: a, b

        integer :: alpha, beta
        integer :: temp

        alpha = rot(this%onext(a))
        beta  = rot(this%onext(b))

        temp              = this%onext(a)
        this%onext(a)     = this%onext(b)
        this%onext(b)     = temp
        temp              = this%onext(alpha)
        this%onext(alpha) = this%onext(beta)
        this%onext(beta)  = temp
    end subroutine

    ! Create a new edge E connecting the origin of A with the destination of B
    ! on side S (either leftSide or rightSide)
    function connect(this,a,b,s) result(e)
        class(quadedge), intent(inout) :: this
        integer, intent(in) :: a, b
        type(side), intent(in) :: s
        integer :: e

        e = this%makeEdge()

        ! Set Org(e) to Dest(a)
        this%facedata(e) = this%facedata(sym(a))
        ! Set Dest(e) to Org(b)
        this%facedata(sym(e)) = this%facedata(b)

        if (s%isLeft .eq. .true.) then
            call this%splice(e,this%lnext(a))
            call this%splice(sym(e),b)
        else
            call this%splice(e,sym(a))
            call this%splice(sym(e),this%oprev(b))
        end if
    end function

    ! Disconnect edge E from the graph
    subroutine deleteEdge(this,e)
        class(quadedge), intent(inout) :: this
        integer, intent(in) :: e

        call this%splice(e,this%oprev(e))
        call this%splice(sym(e),this%oprev(sym(e)))
    end subroutine

    ! The following functions manipulate a single edge
    !  rot(e)  -> Convert to dual edge
    !  sym(e)  -> Swap edge direction ( = rot^2(e) )
    !  irot(e) -> Convert to dual and swap direction  ( = rot^3(e) )
    !
    ! These do not need adjacency information so are implemented as free
    ! functions
    elemental function sym(e)
        integer, intent(in) :: e
        integer :: sym
        integer :: ie, ir
        ie = e/4
        ir = mod(e,4)
        sym = ie + mod(ir + 2,4)
    end function
    elemental function rot(e)
        integer, intent(in) :: e
        integer :: rot
        integer :: ie, ir
        ie = e/4
        ir = mod(e,4)
        rot = ie + mod(ir + 1,4)
    end function
    elemental function irot(e)
        integer, intent(in) :: e
        integer :: irot
        integer :: ie, ir
        ie = e/4
        ir = mod(e,4)
        irot = ie + mod(ir + 3,4)
    end function

    ! The following functions are used to access adjacent edges. 
    !  this%onext(e) -> Next counterclockwise with same origin
    !  this%lnext(e) -> Next counterclockwise with same left face
    !  this%rnext(e) -> Next counterclockwise with same right face
    !  this%dnext(e) -> Next counterclockwise with same destination
    ! Prev functions work the same only moving clockwise
    ! 
    ! While onext is implemented as an array it is accessed the same as the rest
    ! of the query functions, i.e. this%onext(e)
    elemental function lnext(this,e)
        class(quadedge), intent(in) :: this
        integer, intent(in) :: e
        integer :: lnext
        lnext = rot(this%onext(irot(e)))
    end function
    elemental function rnext(this,e)
        class(quadedge), intent(in) :: this
        integer, intent(in) :: e
        integer :: rnext
        rnext = irot(this%onext(rot(e)))
    end function
    elemental function dnext(this,e)
        class(quadedge), intent(in) :: this
        integer, intent(in) :: e
        integer :: dnext
        dnext = sym(this%onext(sym(e)))
    end function
    elemental function oprev(this,e)
        class(quadedge), intent(in) :: this
        integer, intent(in) :: e
        integer :: oprev
        oprev = rot(this%onext(rot(e)))
    end function
    elemental function lprev(this,e)
        class(quadedge), intent(in) :: this
        integer, intent(in) :: e
        integer :: lprev
        lprev = sym(this%onext(e))
    end function
    elemental function rprev(this,e)
        class(quadedge), intent(in) :: this
        integer, intent(in) :: e
        integer :: rprev
        rprev = this%onext(sym(e))
    end function
    elemental function dprev(this,e)
        class(quadedge), intent(in) :: this
        integer, intent(in) :: e
        integer :: dprev
        dprev = irot(this%onext(irot(e)))
    end function

    ! The following functions should be used to access data
    ! They return unique indices for each vertex, face & edge that can be used
    ! as an index into a data storage array.
    !  this%Org(e)   -> Origin point
    !  this%Dest(e)  -> Destination point
    !  this%Left(e)  -> Left face
    !  this%Right(e) -> Right face
    !  Edgeindex(e)  -> Edge index without direction/dual information (1-N if there
    !                   are N edges)
    elemental function org(this,e)
        class(quadedge), intent(in) :: this
        integer, intent(in) :: e
        integer :: org
        org = this%dataindex(e)
    end function
    elemental function dest(this,e)
        class(quadedge), intent(in) :: this
        integer, intent(in) :: e
        integer :: dest
        dest = this%dataindex(sym(e))
    end function
    elemental function left(this,e)
        class(quadedge), intent(in) :: this
        integer, intent(in) :: e
        integer :: left
        left = this%dataindex(irot(e))
    end function
    elemental function right(this,e)
        class(quadedge), intent(in) :: this
        integer, intent(in) :: e
        integer :: right
        right = this%dataindex(rot(e))
    end function
    elemental function edgeindex(e)
        integer, intent(in) :: e
        integer :: edgeindex
        return e/4
    end function
end module
