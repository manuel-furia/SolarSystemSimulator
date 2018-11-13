!This module loads the parameters from the input file, initializes the modules,
!finalizes the modules when the simulation is done and save the results

module LoadSave
  use Celestial
  use Orbit
  use Phys
  implicit none

  !Parameters that will be loaded from the input file
  !and made available to the main program
  !endtime -> duration of the simulation
  !savefactor -> the simulation time between to saves on the output file
  !debugevery -> number of steps to be saved before printing info to screen (-1 -> never print)
  real(kind=8), private :: endtime, savefactor
  integer, private :: debugevery
  integer, private, parameter :: SIMFILE = 2
  

contains

  !Load a simulation from a parameter file (path)
  subroutine loadSim(path)
    character(len=1000), intent(in) :: path
    
    !dur -> duration of the simulation
    !aprec -> angular precision of the Orbit module
    !timestep -> time increment between each step of the simulation
    real(kind=8) :: dur, aprec, timestep

    !To read the tnitial parameters for the bodies
    real(kind=8) :: mass, pos(3), vel(3)

    !To read the names of the bodies
    character(len=50) :: bodyname

    !Identifier of the body, counter, number of bodies,
    !io status of the file, number of steps to be saved
    integer :: id, i, n, ios, numsaves

    !Open the input file
    open(unit=1, file=path, iostat=ios, status="old") 
    
    !If there was an error, print it and quit the program
    if (ios/=0) then
      print '(a,a)','*** Error in opening file ',trim(path)
      stop
    end if
   
    !Read the simulation parameters line by line
    read (1, *) timestep
    read (1, *) dur
    read (1, *) numsaves
    read (1, *) debugevery
    read (1, *) aprec
    read (1, *) n
   
    !Set endtime and savefactor with the data just obtained
    endtime = dur
    savefactor = (dur / numsaves)

    !Initialize the modules
    call initCelestial(n)
    call initOrbit(aprec)
    call initPhys(timestep)

    !Fill the Celestial module with all the object, reading their
    !name on one line and their ptoperty on the next
    do i = 1, n
      read (1, *) bodyname
      read (1, *) mass, pos(1), pos(2), pos(3), vel(1), vel(2), vel(3)
      id = addBody(bodyname, mass, pos, vel)
    end do   


  end subroutine loadSim
  
  !Open the output file and prepares for writing data
  subroutine beginSaveSim(path)
    character(len=1000), intent(in) :: path

    integer :: ios

    open(unit=SIMFILE, file=path, iostat=ios, status="replace") 

    !If there was an error, print it and quit the program
    if (ios/=0) then
      print '(a,a)','*** Error in opening file ',trim(path)
      stop
    end if

  end subroutine beginSaveSim

  !Close the output file and releases the modules
  subroutine closeSim
    close(SIMFILE)
    call finalizeCelestial()
    call finalizeOrbit()
  end subroutine closeSim
 
  !Print debug info about the current status of the simulation to the screen
  subroutine printInfo(curtime, numsteps, numanim)
    real(kind=8), intent(in) :: curtime
    integer, intent(in) :: numsteps, numanim

    !Nothing interesting going on in this function,
    !it simply retrieves the data and prints them

    character(len=50) :: bodyname
    real(kind=8) :: cmdist, speed, period, maxdist, mindist
    real(kind=8) :: cm(3), cmv(3), pos(3), vel(3), acc(3)
    integer :: i, n, completedorbits

    n = getLastBodyId()
    cm = getCenterOfMassPosition()
    cmv = getCenterOfMassVelocity()

    print *, ""
    print *, ""

    print "(A18, I8)", "Number of objects:", n
    print "(A10, F20.8)", "Time: ", curtime
    print "(A10, I10)", "Steps: ", numsteps
    print "(A10, I10)", "Saved steps: ", numanim
    print *, "Center of mass (CoM): ", cm
    print *, "Velocity of the CoM: ", cmv    

    do i = 1, n
      !The commented properties can be decommented and added to
      !print if needed
      bodyname = getName(i)
      cmdist = getDistanceFromCenterOfMass(i)
      speed = GetSpeed(i)
      pos = getPosition(i)
      !vel = getVelocity(i)
      !acc = getAcceleration(i)
      period = getPeriod(i)
      !maxdist = getMaxRadius(i)
      !mindist = getMinRadius(i)
      completedorbits = getNumCompletedOrbits(i)
      
      print *, ""
      print *, ""
      print "(A10, A)", "-- Object: ", bodyname
      print *, "Position: ", pos
      print *, "CoM distance: ", cmdist
      print *, "Speed: ", speed
      print *, "Completed orbits: ", completedorbits
      print *, "Period: ", period
      
      
    end do

    print *, ""
  end subroutine printInfo

  !Append the current data from the simulation to the output file
  subroutine saveSimStep(curtime, numsteps, numanim)
    real(kind=8), intent(in) :: curtime
    integer, intent(in) :: numsteps, numanim

    character(len=50) :: bodyname
    real(kind=8) :: cmdist, speed, period, maxdist, mindist, pos(3), vel(3), acc(3)
    integer :: i, n, completedorbits

    n = getLastBodyId()

    write(SIMFILE, *) "Time: ", curtime, "Steps: ", numsteps, "Saved steps: ", numanim

    do i = 1, n
      bodyname = getName(i)
      cmdist = getDistanceFromCenterOfMass(i)
      speed = GetSpeed(i)
      pos = getPosition(i)
      vel = getVelocity(i)
      acc = getAcceleration(i)
      period = getPeriod(i)
      maxdist = getMaxRadius(i)
      mindist = getMinRadius(i)
      completedorbits = getNumCompletedOrbits(i)
      
      write(SIMFILE, "(A10)", advance="no") bodyname
      write(SIMFILE, "(A)", advance="no") " CM dist: "
      write(SIMFILE, "(G12.4)", advance="no") cmdist
      write(SIMFILE, "(A)", advance="no") " Speed: "
      write(SIMFILE, "(G12.4)", advance="no") speed
      write(SIMFILE, "(A)", advance="no") " Pos: "
      write(SIMFILE, "(3G13.4)", advance="no") pos(1), pos(2), pos(3)
      write(SIMFILE, "(A)", advance="no") " Vel: "
      write(SIMFILE, "(3G13.4)", advance="no") vel(1), vel(2), vel(3)
      write(SIMFILE, "(A)", advance="no") " Acc: "
      write(SIMFILE, "(3G13.4)", advance="no") acc(1), acc(2), acc(3)
      write(SIMFILE, "(A)", advance="no") " Period: "
      write(SIMFILE, "(G12.4)", advance="no") period
      write(SIMFILE, "(A)", advance="no") " Max dist: "
      write(SIMFILE, "(G12.4)", advance="no") maxdist
      write(SIMFILE, "(A)", advance="no") " Min dist: "
      write(SIMFILE, "(G12.4)", advance="no") mindist
      write(SIMFILE, "(A)", advance="no") " Completed orbits: "
      !In the last write, advance to a new line
      write(SIMFILE, *) completedorbits
      
    end do

  end subroutine saveSimStep

  !Return the number of steps that will be saved between every debug print
  real function getDebugEvery()
    getDebugEvery = debugevery
    return
  end function getDebugEvery

  !Return the total duration of the simulation
  real function getEndTime()
    getEndTime = endtime
    return
  end function getEndTime

  !Get the simulation time between two saves in the output file
  real function getSaveTime()
    getSaveTime = savefactor
    return
  end function getSaveTime
  
end module
