MODULE file_module
  IMPLICIT NONE

  INTEGER            :: lun_input
  CHARACTER(LEN=5)   :: input_fname  = 'input'

  CHARACTER(LEN=130) :: raw_data_dir = './raw_data'
  CHARACTER(LEN=130) :: new_data_dir = './new_data'

  CONTAINS

  SUBROUTINE safe_open_old( lun, dir, fname, ierr )
    IMPLICIT NONE

    ! Input variables
    CHARACTER(LEN=*), INTENT(IN) :: dir, fname

    ! Output variables
    INTEGER, INTENT(OUT) :: lun, ierr

    ! Local variables
    CHARACTER(LEN=130) :: tmp_fname

    tmp_fname = TRIM(ADJUSTL(dir))//'/'//TRIM(ADJUSTL(fname))
    OPEN( NEWUNIT=lun, FILE=tmp_fname, STATUS='OLD', IOSTAT=ierr )
    IF ( ierr /= 0 ) WRITE(*,'(a,i2,2a)') 'ERROR(',ierr,'): Cannot open ',tmp_fname

    RETURN
  END SUBROUTINE safe_open_old

  SUBROUTINE safe_open_new( lun, dir, fname, ierr )
    IMPLICIT NONE

    ! Input variables
    CHARACTER(LEN=*), INTENT(IN) :: dir, fname

    ! Output variables
    INTEGER, INTENT(OUT) :: lun, ierr

    ! Local variables
    CHARACTER(LEN=130) :: tmp_fname

    tmp_fname = TRIM(ADJUSTL(dir))//'/'//TRIM(ADJUSTL(fname))
    OPEN( NEWUNIT=lun, FILE=tmp_fname, STATUS='NEW', IOSTAT=ierr )
    IF ( ierr /= 0 ) THEN
      OPEN( NEWUNIT=lun, FILE=tmp_fname, STATUS='REPLACE', IOSTAT=ierr )
      IF ( ierr /= 0 ) WRITE(*,'(a,i2,2a)') 'ERROR(',ierr,'): Cannot open ',tmp_fname
    END IF

    RETURN
  END SUBROUTINE safe_open_new

  SUBROUTINE file_init
    USE net_module, ONLY: lun_sunet, lun_netsu_in, lun_netsu_out, &
    & sunet_fname, netsu_in_fname, netsu_out_fname
    USE partf_module, ONLY: lun_netwinv_in, lun_netwinv_out, &
    & netwinv_in_fname, netwinv_out_fname, lun_ame11, lun_reac1, &
    & lun_ame11extrap, lun_frdm, lun_ame03, lun_ame03extrap, &
    & ame11_fname, reac1_fname, ame11extrap_fname, frdm_fname, &
    & ame03_fname, ame03extrap_fname
    USE ffn_module, ONLY: lun_netweak_in, lun_netweak_out, &
    & netweak_in_fname, netweak_out_fname, ffn_flag
    USE nnu_module, ONLY: lun_netneutr_in, lun_netneutr_out, &
    & netneutr_in_fname, netneutr_out_fname, nnu_flag
    IMPLICIT NONE

    ! Local variables
    INTEGER :: ierr, idefaults, iffn_flag, innu_flag

    CALL safe_open_old( lun_input, '.', input_fname, ierr )
    IF ( ierr /= 0 ) THEN
      WRITE(*,'(a)') 'Use default inputs? (no=0,yes=1)'
      READ(*,*) idefaults

      IF ( idefaults <= 0 ) THEN
        WRITE(*,'(a)') 'Replace REACLIB rates with tabulated EC/PC rates? (no=0,yes=1)'
        READ(*,*) iffn_flag
        IF ( iffn_flag <= 0 ) ffn_flag = .false.

        WRITE(*,'(a)') 'Include neutrino capture rates? (no=0,yes=1)'
        READ(*,*) innu_flag
        IF ( innu_flag <= 0 ) nnu_flag = .false.

        WRITE(*,'(a)') 'Enter path for new network files directory.'
        READ(*,*) new_data_dir
      ELSE
        ffn_flag = .true.
        nnu_flag = .true.
        new_data_dir = './new_data'
      END IF
    ELSE
      READ(lun_input,'(i1)') iffn_flag
      IF ( iffn_flag <= 0 ) ffn_flag = .false.

      READ(lun_input,'(i1)') innu_flag
      IF ( innu_flag <= 0 ) nnu_flag = .false.

      READ(lun_input,*)
      READ(lun_input,*)
      READ(lun_input,'(a130)') new_data_dir
      CLOSE(lun_input)
    END IF

    ! Open raw data files
    CALL safe_open_old( lun_sunet, '.', sunet_fname, ierr )
    IF ( ierr /= 0 ) STOP
    
    CALL safe_open_old( lun_netsu_in, raw_data_dir, netsu_in_fname, ierr )
    IF ( ierr /= 0 ) STOP

    CALL safe_open_old( lun_netwinv_in, raw_data_dir, netwinv_in_fname, ierr )
    IF ( ierr /= 0 ) STOP

    CALL safe_open_old( lun_ame03, raw_data_dir, ame03_fname, ierr )
    IF ( ierr /= 0 ) STOP

    CALL safe_open_old( lun_ame03extrap, raw_data_dir, ame03extrap_fname, ierr )
    IF ( ierr /= 0 ) STOP

    CALL safe_open_old( lun_ame11, raw_data_dir, ame11_fname, ierr )
    IF ( ierr /= 0 ) STOP

    CALL safe_open_old( lun_reac1, raw_data_dir, reac1_fname, ierr )
    IF ( ierr /= 0 ) STOP

    CALL safe_open_old( lun_ame11extrap, raw_data_dir, ame11extrap_fname, ierr )
    IF ( ierr /= 0 ) STOP

    CALL safe_open_old( lun_frdm, raw_data_dir, frdm_fname, ierr )
    IF ( ierr /= 0 ) STOP

    IF ( ffn_flag ) THEN
      CALL safe_open_old( lun_netweak_in, raw_data_dir, netweak_in_fname, ierr )
      IF ( ierr /= 0 ) ffn_flag = .false.
    END IF

    IF ( nnu_flag ) THEN
      CALL safe_open_old( lun_netneutr_in, raw_data_dir, netneutr_in_fname, ierr )
      IF ( ierr /= 0 ) nnu_flag = .false.
    END IF

    ! Open output files for new network
    CALL safe_open_new( lun_netsu_out, new_data_dir, netsu_out_fname, ierr )
    IF ( ierr /= 0 ) STOP

    CALL safe_open_new( lun_netwinv_out, new_data_dir, netwinv_out_fname, ierr )
    IF ( ierr /= 0 ) STOP

    CALL safe_open_new( lun_netweak_out, new_data_dir, netweak_out_fname, ierr )
    IF ( ierr /= 0 ) STOP

    IF ( nnu_flag ) THEN
      CALL safe_open_new( lun_netneutr_out, new_data_dir, netneutr_out_fname, ierr )
      IF ( ierr /= 0 ) STOP
    END IF

    RETURN
  END SUBROUTINE file_init

  SUBROUTINE file_finalize
    USE net_module, ONLY: lun_sunet, lun_netsu_in, lun_netsu_out
    USE partf_module, ONLY: lun_netwinv_in, lun_netwinv_out, lun_ame11, &
    & lun_reac1, lun_ame11extrap, lun_frdm, lun_ame03, lun_ame03extrap
    USE ffn_module, ONLY: lun_netweak_in, lun_netweak_out, ffn_flag
    USE nnu_module, ONLY: lun_netneutr_in, lun_netneutr_out, nnu_flag
    IMPLICIT NONE

    CLOSE( lun_sunet )
    CLOSE( lun_netsu_in )
    CLOSE( lun_netwinv_in )
    CLOSE( lun_ame03 )
    CLOSE( lun_ame03extrap )
    CLOSE( lun_ame11 )
    CLOSE( lun_reac1 )
    CLOSE( lun_ame11extrap )
    CLOSE( lun_frdm )
    CLOSE( lun_netsu_out )
    CLOSE( lun_netwinv_out )
    CLOSE( lun_netweak_out )
    IF ( ffn_flag ) CLOSE( lun_netweak_in )
    IF ( nnu_flag ) CLOSE( lun_netneutr_in )
    IF ( nnu_flag ) CLOSE( lun_netneutr_out )

    RETURN
  END SUBROUTINE file_finalize

END MODULE file_module
