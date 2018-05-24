MODULE pakket_package

    IMPLICIT NONE

    ! A pair of package version bounds limits which package versions can be
    ! installed. The lower bound is inclusive whilst the upper bound is
    ! exclusive.
    TYPE bounds_t
        INTEGER :: lower
        INTEGER :: upper
    END TYPE

    ! A package atom pairs a package with a pair of package version bounds.
    TYPE atom_t
        INTEGER :: package
        TYPE(bounds_t) :: bounds
    END TYPE

CONTAINS

    ! A pair of package version bounds is invalid if its upper bound is lower
    ! than its lower bound.
    FUNCTION bounds_valid(a)
        TYPE(bounds_t), INTENT(IN) :: a
        LOGICAL :: bounds_valid
        bounds_valid = a%lower .LT. a%upper
    END FUNCTION

    ! What is the intersection of these two pairs of package version bounds? In
    ! case there is no intersection, this function returns an invalid pair of
    ! package version bounds.
    FUNCTION bounds_intersect(a, b)
        TYPE(bounds_t), INTENT(IN) :: a, b
        TYPE(bounds_t) :: bounds_intersect
        bounds_intersect%lower = MAX(a%lower, b%lower)
        bounds_intersect%upper = MIN(a%upper, b%upper)
    END FUNCTION

END MODULE
