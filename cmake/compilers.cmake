add_compile_options(
$<$<AND:$<CONFIG:Release>,$<COMPILE_LANG_AND_ID:Fortran,GNU>>:-fno-backtrace>
)
