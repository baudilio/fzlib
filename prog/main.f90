PROGRAM main
  USE, INTRINSIC :: iso_c_binding, ONLY: c_char, c_int, c_ptr, c_null_char, c_ASSOCIATED
  USE forgz
  IMPLICIT NONE

  CHARACTER(Len=*, Kind=c_char), PARAMETER :: filename = "data/scanfs.gz"
  LOGICAL :: exists

  !CHARACTER(Len=1, kind=c_char), DIMENSION(256) :: record
  CHARACTER(Len=256, kind=c_char) :: record = CHAR(0)

  ! to test scanfs()
  ! CHARACTER(Len=256, kind=c_char), ALLOCATABLE, dimension(:) :: buff ! nope!
  CHARACTER(Len=1, kind=c_char), ALLOCATABLE, DIMENSION(:) :: buff ! OK

  INTEGER :: i
  INTEGER :: ierr

  TYPE(c_ptr) :: inp

  !
  INTEGER, DIMENSION(13) :: attr
  INTEGER :: status


  ! ----

  ! -- make sure that the gz file is accessible
  INQUIRE(file=filename, exist=exists)
  IF (.NOT. exists) STOP 'File not found'

  ! -- get some info about the gz file
  CALL STAT(NAME=filename, VALUES=attr, STATUS=status)
  WRITE (*, FMT="('File size:',               T30, I19)") attr(8)
  WRITE (*, FMT="('Last access time:',        T30, A19)") CTIME(attr(9))
  WRITE (*, FMT="('Preferred block size:',    T30, I19)") attr(12)
  WRITE (*, FMT="('No. of blocks allocated:', T30, I19)") attr(13)


  ! -- open gz file
  inp = gzopen(filename, "rb")
  IF (.NOT. c_ASSOCIATED(inp)) THEN
     PRINT '("Cannot open file ",A, " for reading")', TRIM(filename)
     STOP 1
  ENDIF

  ! -- Read a record
  ierr = gzgets(inp, record, len(record))
  PRINT *, "Header: ",  Trim(record)


  ! Read the next three records
  DO i =1, 10
     record = c_null_char ! or "", or CHAR(0)
     ierr = gzgets(inp, record, LEN(record))
     PRINT '("Record:", I2, " - has: ", A)', i, Trim(record)
  END DO


  ! - Done processing the file.
  ierr = gzclose(inp)
  IF (ierr /= 0) THEN
     PRINT *, "something went wrong when closing file"
     STOP 1;
  END IF


  ! Test the subroutine scanfs()
  ALLOCATE(buff(258*11))
  buff = char(0)
  CALL scanfs(FILENAME=filename, BUFFER=buff, STATUS=status)
  IF (status == 0) THEN
     PRINT *, buff
  ELSE
     PRINT *, "kk"
  END IF




  PRINT *, 'Main has finished gracefully.'
  STOP 0
END PROGRAM main
