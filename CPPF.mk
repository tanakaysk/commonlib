# Make file for common developing platforms
#
# "vpath" directive must include 
#    directory path of common development platforms
#    (for example $(CDPD))
#
# macros defiend prior to the inclusion of this file
#   - Compile (necessary)
#       $(FC)          : compiler
#       $(FFLAGS)      : compile options
#       $(FFLAGS_NODBG): compile options without debug options

#===== netCDF supplemental module =====
nclib.o : module__CPPFlib__netcdf.f90 cnst.o
	$(FC) $(FFLAGS) $(NC_INC) -o $@ $<

#===== Constants definition modules =====
#   kind constants
cnst.o : module__CPPFdef__cnst.f90
	$(FC) $(FFLAGS) -o $@ $<
