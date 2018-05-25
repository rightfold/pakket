MODULE pakket_package

    ! Integers that denote packages refer to the array index of the package in
    ! the package array. The package array is an array of all known packages.
    ! It is not globally defined, but rather passed to the various procedures
    ! that deal with packages.

    IMPLICIT NONE

    ! A pair of package version bounds limits which package versions can be
    ! installed. The lower bound is inclusive whilst the upper bound is
    ! exclusive.
    TYPE bounds_t
        INTEGER :: lower
        INTEGER :: upper
    END TYPE

    ! A bounded package pairs a package with a pair of package version bounds.
    TYPE bounded_package_t
        INTEGER :: package
        TYPE(bounds_t) :: bounds
    END TYPE

    ! A particular version of a package, along with its dependencies.
    TYPE package_version_t
        INTEGER :: version
        TYPE(bounded_package_t), DIMENSION(:), POINTER :: dependencies
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

    ! Return whether the given package version is in bounds.
    ELEMENTAL FUNCTION in_bounds(version, bounds)
        INTEGER, INTENT(IN) :: version
        TYPE(bounds_t), INTENT(IN) :: bounds
        LOGICAL :: in_bounds
        in_bounds = version .GE. bounds%lower .AND.                         &
                    version .LT. bounds%upper
    END FUNCTION

END MODULE
