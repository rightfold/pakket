MODULE pakket_resolve

    USE pakket_package, ONLY:                                               &
        bounded_package_t,                                                  &
        bounds_t,                                                           &
        package_version_t,                                                  &
        in_bounds

    IMPLICIT NONE

    ! Pairs an array of package versions that are available with a resolved
    ! version. The resolved version is an index into the array, and is assigned
    ! to by resolve.
    TYPE resolvable_package_t
        TYPE(package_version_t), DIMENSION(:), POINTER :: versions
        INTEGER :: version_loc
    END TYPE

    PRIVATE
    PUBLIC :: resolve
    PUBLIC :: highest_version_loc

CONTAINS

    ! This procedure resolves for the given target package and each
    ! (transitive) dependency of the package the highest possible version that
    ! does not cause a conflict and does not violate any version bounds in the
    ! dependency graph.
    !
    ! There must not be any cyclic dependencies.
    RECURSIVE SUBROUTINE resolve(packages, target)
        TYPE(resolvable_package_t), DIMENSION(:), INTENT(INOUT) :: packages
        TYPE(bounded_package_t), INTENT(IN) :: target
        CALL resolve_package(packages(target%package), target%bounds)
        CALL resolve_dependencies(packages, target%package)
    END SUBROUTINE

    ! Find the highest version of a package and store it in the package.
    SUBROUTINE resolve_package(package, bounds)
        TYPE(resolvable_package_t), INTENT(INOUT) :: package
        TYPE(bounds_t), INTENT(IN) :: bounds
        package%version_loc = highest_version_loc(                          &
            package%versions%version,                                       &
            bounds                                                          &
        )
    END SUBROUTINE

    ! Resolve the dependencies of a package by recursing into resolve for each
    ! one of them.
    RECURSIVE SUBROUTINE resolve_dependencies(packages, target)
        TYPE(resolvable_package_t), INTENT(INOUT), TARGET :: packages(:)
        INTEGER, INTENT(IN) :: target

        TYPE(resolvable_package_t), POINTER :: package
        TYPE(bounded_package_t), DIMENSION(:), POINTER :: dependencies
        INTEGER :: i

        package => packages(target)
        dependencies => package%versions(package%version_loc)%dependencies

        DO i = 1, SIZE(dependencies)
            CALL resolve(packages, dependencies(i))
        END DO
    END SUBROUTINE

    ! Given an array of available package versions and a pair of package
    ! version bounds, return the index of the highest available package version
    ! that satisfies the package bounds.
    FUNCTION highest_version_loc(versions, bounds)
        INTEGER, DIMENSION(:), INTENT(IN) :: versions
        TYPE(bounds_t), INTENT(IN) :: bounds
        INTEGER :: highest_version_loc
        highest_version_loc = MAXLOC(versions, 1, in_bounds(versions, bounds))
    END FUNCTION

END MODULE
