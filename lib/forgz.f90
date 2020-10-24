MODULE forgz
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: gzopen, gzclose, gzgets, gzread
  PUBLIC :: scanfs

  INTERFACE
     TYPE(c_ptr) FUNCTION gzopen(filename, mode) BIND(C, name="gzopen")
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_ptr, c_char
       IMPLICIT NONE
       CHARACTER(kind=c_char), DIMENSION(*) :: filename
       CHARACTER(kind=c_char), DIMENSION(*) :: mode
     END FUNCTION gzopen
  END INTERFACE

  INTERFACE
     INTEGER(c_int) FUNCTION gzread(filename, buffer, length) BIND(c, name="gzread")
       USE, INTRINSIC :: iso_c_binding, ONLY: c_ptr, c_char, c_int
       IMPLICIT NONE
       TYPE(c_ptr), VALUE :: filename
       CHARACTER(kind=c_char), DIMENSION(*) :: buffer
       INTEGER(c_int), VALUE :: length
     END FUNCTION gzread
  END INTERFACE

    INTERFACE
     INTEGER(c_int) FUNCTION gzgets(filename, buffer, length) BIND(c)
       USE, INTRINSIC :: iso_c_binding, ONLY: c_ptr, c_char, c_int
       IMPLICIT NONE
       TYPE(c_ptr), VALUE:: filename
       CHARACTER(kind=c_char), DIMENSION(*) :: buffer  ! char *buffer
       INTEGER(c_int) :: length
     END FUNCTION gzgets
  END INTERFACE

  INTERFACE
     INTEGER(c_int) FUNCTION gzclose(gzFile) BIND(c)
       USE, INTRINSIC :: iso_c_binding, ONLY: c_ptr, c_int
       IMPLICIT NONE
       TYPE(c_ptr), VALUE :: gzFile
     END FUNCTION gzclose
  END INTERFACE


CONTAINS

  SUBROUTINE scanfs(filename, buffer, status)
    USE, INTRINSIC :: iso_c_binding, ONLY: c_char, c_int, c_ptr, &
         c_null_char, c_associated
    IMPLICIT NONE

    ! -- dummy arguments
    CHARACTER(len=*, kind=c_char), INTENT(in) :: filename
    CHARACTER(len=1, kind=c_char), DIMENSION(:), INTENT(inout) :: buffer
    INTEGER, INTENT(out) :: status

    ! -- Local Variables
    TYPE(c_ptr) :: inp
    INTEGER(c_int) :: ierr


    ! ---

    status = 1

	! - open file
    INP = gzopen(TRIM(filename)//c_null_char, "rb")
    IF (.not. c_ASSOCIATED(inp)) RETURN ! BTA: sanity check??

	! read data
     ierr = gzread(INP, buffer, SIZE(buffer))
    ! IF (ierr /= 0) RETURN ! TODO: differenciate and handle EOF

	! close
    ierr = gzclose(INP)
    IF (ierr /= 0) RETURN

    ! No errors
    status = 0
  END SUBROUTINE scanfs


END MODULE forgz
