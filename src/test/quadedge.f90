module test_quadedge
    use sawlibf_quadedge
    use sawlibf_test
    implicit none

contains
    subroutine init
        type(quadedge) :: q
        q = quadedge()
    end subroutine
    subroutine accessors
        integer :: e

        e = 1*4
        call assert(sym(e)  .eq. e + 2)
        call assert(rot(e)  .eq. e + 1)
        call assert(irot(e) .eq. e + 3)
        call assert(rot(irot(e)) .eq. e)
    end subroutine
    subroutine makeedge
        type(quadedge) :: q
        integer :: e

        q = quadedge()
        e = q%makeEdge()

        call assert(q%lnext(e) .eq. sym(e))
        call assert(q%rnext(e) .eq. sym(e))
        call assert(q%onext(e) .eq. e)
        call assert(q%oprev(e) .eq. e)

        call assert(q%org(e) .ne. q%dest(e))
        call assert(q%left(e) .eq. q%right(e))
    end subroutine
    subroutine makeedge2
        type(quadedge) :: q
        integer :: a, b

        q = quadedge()
        a = q%makeEdge()
        b = q%makeEdge()

        call assert(q%onext(a) .eq. a)
        call assert(q%oprev(a) .eq. a)
        call assert(q%lnext(a) .eq. sym(a))
        call assert(q%rnext(a) .eq. sym(a))

        call assert(q%org(a) .ne. q%dest(a))
        call assert(q%left(a) .eq. q%right(a))

        call assert(q%lnext(b) .eq. sym(b))
        call assert(q%rnext(b) .eq. sym(b))
        call assert(q%onext(b) .eq. b)
        call assert(q%oprev(b) .eq. b)

        call assert(q%org(b) .ne. q%dest(b))
        call assert(q%left(b) .eq. q%right(b))
    end subroutine
    subroutine makeedgerot
        type(quadedge) :: q
        integer :: e
        q = quadedge()
        e = rot(q%makeEdge())

        call assert(q%lnext(e) .eq. e)
        call assert(q%rnext(e) .eq. e)
        call assert(q%onext(e) .eq. sym(e))
        call assert(q%oprev(e) .eq. sym(e))

        call assert(q%org(e) .eq. q%dest(e))
        call assert(q%left(e) .ne. q%right(e))
    end subroutine
    subroutine makeface
        type(quadedge) :: q
        integer :: a,b
        q = quadedge()
        a = q%makeEdge()
        b = q%makeEdge()

        call q%splice(a,b)
        call q%splice(sym(b),sym(a))
        write(*,*) q%onext

        call assert(q%onext(a) .eq. b)
        call assert(q%dnext(a) .eq. b)
        call assert(q%lnext(a) .eq. sym(b))
        call assert(q%rnext(a) .eq. sym(b))
    end subroutine

end module

program test
    use test_quadedge

    call init
    call accessors
    call makeedge
    call makeedge2
    call makeedgerot
    call makeface
end program
