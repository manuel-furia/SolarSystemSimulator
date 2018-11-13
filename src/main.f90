!CelestialSimulator, an n-body simulator for celestial bodies
!Main program, it call the modules and manage the simulation

program CelestialSimulator
  use LoadSave
  use Celestial
  use Orbit
  use Phys
  implicit none 

  character(len=1000) :: infile, outfile
  integer :: argc 
  integer :: steps, savedsteps, lastdebug, debugevery
  real(kind=8) :: curtime, timestep, duration, lastsave, savetime

  !Get the number of arguments
  argc = command_argument_count()

  !If the number of arguments is not correct, print the usage
  if (argc /= 2) then
    print *, "Usage: celsim input_file output_file"
    stop
  end if

  !Get the arguments
  call get_command_argument(1, infile)
  call get_command_argument(2, outfile)

  !Call the Loadsave module:
  !load the simulation parameters and open the output file
  call loadSim(infile)
  call beginSaveSim(outfile)
  
  !Initialize some useful value for the simulation
  steps = 0
  savedsteps = 0
  lastdebug = 0
  lastsave = 0.0

  !Get some useful value from the loadsave module
  duration = getEndTime()
  savetime = getSaveTime()
  debugevery = getDebugEvery()

  !Print and save the initial step
  if (debugevery > 0) then 
    !Print the info only if is requested
    call printInfo(curtime, steps, savedsteps)
  end if
  call saveSimStep(curtime, steps, savedsteps)

  !Continue the simulation until curtime get past the duration
  do while (curtime < duration)
    !Call the Phys module, calculate the next positions and other properties
    curtime = calculateNext()
    !Call the Orbit module, try to calculate the orbital parameters
    call updateOrbit(curtime)
    !Increment the number of steps done
    steps = steps + 1
    
    !If enough time has passed before the last save...
    if (curtime - lastsave >= savetime) then
      lastsave = curtime !... set the last save to be now
      savedsteps = savedsteps + 1 !Increment the number of saves done
      call saveSimStep(curtime, steps, savedsteps) !Save
    end if

    !If enough time has passed before the last debug print...
    if (debugevery > 0 .and. savedsteps - lastdebug >= debugevery) then
      lastdebug = savedsteps !... set the last debug print to be now
      call printInfo(curtime, steps, savedsteps) !Print the info
    end if
    
  end do

  !Terminate the simulation, close the file and
  !finalize the modules
  call closeSim()

end program CelestialSimulator
