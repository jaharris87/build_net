PROGRAM reaclib_reader
  USE file_module, ONLY: file_init, file_finalize
  USE net_module, ONLY: read_sunet
  USE partf_module, ONLY: build_netwinv
  USE ffn_module, ONLY: build_netweak
  USE nnu_module, ONLY: build_netneutr
  IMPLICIT NONE

  ! Initialize I/O
  CALL file_init

  ! Read list of nuclei for new network from sunet file
  CALL read_sunet

  ! Build netwinv file containing partition function data
  CALL build_netwinv

  ! Build netweak file containing tabulated EC/PC rates in FFN format
  CALL build_netweak

  ! Build netneutr file containing neutrino-capture rates
  CALL build_netneutr

  ! Build netsu file containg REACLIB formatted rates
  CALL build_netsu

  ! Finalize I/O
  CALL file_finalize

  STOP

  CONTAINS

  SUBROUTINE build_netsu
    USE net_module, ONLY: nnet, net_index_from_name, write_net_rate, nname_net, lun_netsu_in, lun_netsu_out
    USE ffn_module, ONLY: nffn, ffn_index_from_name, write_ffn_rate
    USE nnu_module, ONLY: nnnu, nnu_index_from_name, write_nnu_rate
    IMPLICIT NONE

    ! Local variables
    INTEGER          :: k_read
    CHARACTER(LEN=5) :: nname_read(6)
    CHARACTER(LEN=4) :: desc_read
    CHARACTER(LEN=1) :: rflag_read, wflag_read
    REAL(8)          :: q_read,rc_read(7)

    INTEGER :: krate, k1(8)
    INTEGER :: inucw, iffn(2), innu(2)
    INTEGER :: i, j, inuc, jnuc, ierr, nrcn

    k1 = (/2,3,4,3,4,5,6,5/)
    inucw = 0
    nrcn = 0

    WRITE(lun_netsu_out,'(2i5)') nffn,nnnu
    
    READ(lun_netsu_in,'(i1,4x,6a5,8x,a4,a1,a1,3x,1pe12.5)',IOSTAT=ierr) &
    &   k_read,(nname_read(j),j=1,6),desc_read,rflag_read,wflag_read,q_read
    LOOP1: DO
      krate = k_read
      READ(lun_netsu_in,'(4e13.6)') (rc_read(j),j=1,7)

      CALL write_net_rate( krate, nname_read, desc_read, rflag_read, wflag_read, q_read, rc_read )

      LOOP2: DO
        READ(lun_netsu_in,'(i1,4x,6a5,8x,a4,a1,a1,3x,1pe12.5)',IOSTAT=ierr) &
        &   k_read,(nname_read(j),j=1,6),desc_read,rflag_read,wflag_read,q_read
        IF ( ierr /= 0 ) THEN
          EXIT LOOP1
        ELSE IF ( k_read /= 0 ) THEN
          IF ( krate == 1 .and. inucw < nnet ) THEN
            inucw = inucw + 1
            DO inuc = inucw, nnet
              CALL ffn_index_from_name( nname_net(inuc), iffn )
              CALL nnu_index_from_name( nname_net(inuc), innu )
              DO i = 1, 2
                CALL write_nnu_rate( innu(i) )
                CALL write_ffn_rate( iffn(i) )
              END DO
            END DO
          END IF
          EXIT LOOP2
        END IF
        READ(lun_netsu_in,'(4e13.6)') (rc_read(j),j=1,7)

        DO i = 1, k1(krate)
          IF ( LEN_TRIM(nname_read(i)) > 0 ) THEN
            CALL net_index_from_name( nname_read(i), inuc )
            IF ( inuc == 0 ) CYCLE LOOP2
          END IF
        END DO

        IF ( krate == 1 ) THEN
          inucw = inucw + 1
          CALL net_index_from_name( nname_read(1), inuc )
          IF ( inucw < inuc ) THEN
            DO jnuc = inucw, inuc-1
              CALL ffn_index_from_name( nname_net(jnuc), iffn )
              CALL nnu_index_from_name( nname_net(jnuc), innu )
              DO i = 1, 2
                CALL write_nnu_rate( innu(i) )
                CALL write_ffn_rate( iffn(i) )
              END DO
            END DO
          END IF

          CALL ffn_index_from_name( nname_net(inuc), iffn )
          CALL nnu_index_from_name( nname_net(inuc), innu )
          IF ( iffn(1) == 0 .and. innu(1) == 0 ) THEN
            CALL write_net_rate( k_read, nname_read, desc_read, rflag_read, wflag_read, q_read, rc_read )
            nrcn = nrcn + 1
          ELSE IF ( inucw <= inuc ) THEN
            IF ( iffn(1) == 0 .and. innu(1) /= 0 ) THEN
              CALL write_nnu_rate( innu(1) )
              CALL write_net_rate( k_read, nname_read, desc_read, rflag_read, wflag_read, q_read, rc_read )
              CALL write_nnu_rate( innu(2) )
              nrcn = nrcn + 1
            ELSE IF ( iffn(1) /= 0 .and. innu(1) == 0 ) THEN
              CALL write_ffn_rate( iffn(1) )
              CALL write_ffn_rate( iffn(2) )
            ELSE IF ( iffn(1) /= 0 .and. innu(1) /= 0 ) THEN
              DO i = 1, 2
                CALL write_nnu_rate( innu(i) )
                CALL write_ffn_rate( iffn(i) )
              END DO
            END IF
          ELSE IF ( iffn(1) == 0 ) THEN
            CALL write_net_rate( k_read, nname_read, desc_read, rflag_read, wflag_read, q_read, rc_read )
            nrcn = nrcn + 1
          END IF
          inucw = inuc
        ELSE
          CALL write_net_rate( k_read, nname_read, desc_read, rflag_read, wflag_read, q_read, rc_read )
          nrcn = nrcn + 1
        END IF

      END DO LOOP2
    END DO LOOP1

    WRITE(*,'(a,i5)') '# reactions (total): ',nrcn+nffn+nnnu
    WRITE(*,'(a,i5)') '          (REACLIB): ',nrcn
    WRITE(*,'(a,i5)') '              (FFN): ',nffn
    WRITE(*,'(a,i5)') '              (NNU): ',nnnu

    RETURN
  END SUBROUTINE build_netsu

END PROGRAM
