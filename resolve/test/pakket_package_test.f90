PROGRAM pakket_package_test

    USE pakket_package, ONLY: bounds_t, bounds_valid

    IMPLICIT NONE

    TYPE(bounds_t) :: bounds

    CALL tap_plan(2)

    bounds%lower = 1
    bounds%upper = 2
    CALL tap_true(bounds_valid(bounds))

    bounds%lower = 2
    bounds%upper = 1
    CALL tap_false(bounds_valid(bounds))

    CALL tap_true(.FALSE.)

CONTAINS

    SUBROUTINE tap_plan(n)
        INTEGER, INTENT(IN) :: n
        WRITE (*, '(a, I6.6)') '1..', n
    END SUBROUTINE

    SUBROUTINE tap_true(a)
        LOGICAL, INTENT(IN) :: a
        INTEGER, SAVE :: n = 0
        CHARACTER(6) :: directive
        n = n + 1
        directive = MERGE('ok    ', 'not ok', a)
        WRITE (*, '(a, 1x, I6.6)') directive, n
    END SUBROUTINE

    SUBROUTINE tap_false(a)
        LOGICAL, INTENT(IN) :: a
        CALL tap_true(.NOT. a)
    END SUBROUTINE

END PROGRAM
