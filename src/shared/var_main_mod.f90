module var_main

  character(len=300) :: obsd_file, synt_file, synt_phydisp_file

  double precision :: MIN_PERIOD, MAX_PERIOD

  character(len=300) :: flexwin_outdir
  logical :: WRITE_SINGLE_WIN_FILE, REMOVE_SW

  character(len=300) :: MEASURE_ADJ_OUTDIR
  integer :: WEIGHTING_OPTION
  logical :: USE_PHYDISP, WRITE_ADJ_ASDF, ROTATE_COMP, WRITE_ADJ_ASCII

end module var_main
