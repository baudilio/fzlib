MODULE forgz
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: gzopen, gzclose, gzgets, gzread

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

END MODULE forgz
