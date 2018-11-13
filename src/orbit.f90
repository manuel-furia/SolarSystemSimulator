!This module analyzes the orbits of the objects in the simulation around the center of mass of the system,
!trying to extrapolate the period of the orbit and other useful informations

module Orbit
  use Celestial
  implicit none

  !Possible status of the orbit
  !UNKNOWN -> initial status
  !STARTED -> the object is in the start region of the orbit (startangle - aprec < angle < starangle + aprec)
  !WAITING -> the object is not in the start region, and we are waiting for it come back
  integer, private, parameter :: UNKNOWN = 0  
  integer, private, parameter :: STARTED = 1   
  integer, private, parameter :: WAITING = 2  
  
  !Represents an orbit
  type Orb
    integer :: completedOrbits !Number of times the orbit has been completed
    integer :: orbitstatus !Status of the orbit (see constants in the beginning of the module)
    integer :: bestplane !Coordinate plane in which the projection of the orbit is bigger (NOT DEFINED = 0, XY = 1, XZ = 2, YZ = 3)

    !Period, maximum distance from the center of mass, minimum distance from
    !the center of mass, start angle in the orbit, start time in the orbit
    real(kind=8) :: period, maxrad, minrad, startangle, starttime
  end type

  !List of orbits, one for each body
  type(Orb), private, allocatable :: orbits(:)
  
  !Angular precision of the module in determining if the object has come back to the start
  !NOTE: A value too small with a timestep too big can cause the object to skip the starting region,
  !      making the orbit impossible to count by this method
  real(kind=8), private :: aprec

contains

  !Inizialize the variables in the module and allocate the space
  !for orbital data
  subroutine initOrbit(angprecision)
    real(kind=8), intent(in) :: angprecision
    integer :: i, n

    aprec = angprecision
    n = getMaxBodiesNumber()
    
    allocate(orbits(n))

    do i = 1, n
      orbits(i)%completedOrbits = 0
      orbits(i)%orbitStatus = UNKNOWN
      orbits(i)%bestplane = 0
      orbits(i)%period = 0.0
      orbits(i)%maxrad = 0.0
      orbits(i)%minrad = huge(orbits(i)%minrad)
      orbits(i)%startangle = 0.0
      orbits(i)%starttime = 0.0
    end do

  end subroutine initOrbit

  !Deallocate the space previously allocated
  subroutine finalizeOrbit()

    deallocate(orbits)

  end subroutine finalizeOrbit

  !Get the vector normal to the plane in which the orbit of the object
  !identified by bodyid lies
  function getOrbitPlane(bodyid)
    real(kind=8), dimension(3) :: getOrbitPlane
    integer, intent(in) :: bodyid

    !Vectors, versors and magnitudes
    real(kind=8) :: pos(3), vel(3), plane(3), magplane
 
    !Get the vectors
    pos = getPosition(bodyid)
    vel = getVelocity(bodyid)

    
    !Take the vector product of position and velocity versors
    plane(1) = pos(2) * vel(3) - pos(3) * vel(2)
    plane(2) = pos(3) * vel(1) - pos(1) * vel(3)
    plane(3) = pos(1) * vel(2) - pos(2) * vel(1)

    magplane = sqrt(plane(1)**2 + plane(2)**2 + plane(3)**2)

    !If the plane is undefined return the null vector
    if (abs(magplane) < tiny(magplane)) then
      getOrbitPlane = (/ 0.0, 0.0, 0.0 /)
    else
      !Return the versor
      getOrbitPlane = plane / magplane
    end if

    return
  end function

  !Get the coordinate plane which maximize the projection area of the orbit
  integer function getBestProjectionPlane(bodyid)
    integer, intent(in) :: bodyid

    
    real(kind=8) :: plane(3), planemag, px, py, pz 
 
    !Get the orbit plane
    plane = getOrbitPlane(bodyid)
    !and it's magnitude (should be 1, or 0 if undefined)
    planemag = sqrt(plane(1)**2 + plane(2)**2 + plane(3)**2)

    !Absolute values of the components of the vector, to check the
    !prevalent one
    px = abs(plane(1))
    py = abs(plane(2))
    pz = abs(plane(3))

    !Check which component of the orbit plane is prevalent
    if (planemag < 0.5) then
      getBestProjectionPlane = 0
    elseif (px >= py .and. px >= pz) then
      getBestProjectionPlane = 1
    elseif (py >= px .and. py >= pz) then
      getBestProjectionPlane = 2
    elseif (pz >= px .and. pz >= py) then
      getBestProjectionPlane = 3
    end if

    return
  end function

  !Get the angle of the projection of a vector on a coordinate plane
  real(kind=8) function getAngleInCoordinatePlane(plane, r)
    integer, intent(in) :: plane
    real(kind=8), intent(in) :: r(3)

    if (plane == 1) then
      getAngleInCoordinatePlane = atan2(r(2), r(3))
    elseif (plane == 2) then
      getAngleInCoordinatePlane = atan2(r(3), r(1))
    elseif (plane == 3) then
      getAngleInCoordinatePlane = atan2(r(1), r(2))
    else
      getAngleInCoordinatePlane = 0.0
    end if

  end function
  

  !Update the orbit module
  subroutine updateOrbit(curtime)
    real(kind=8), intent(in) :: curtime
    
    integer :: i, n, bestplane
    real(kind=8) :: pos(3), poscm(3), r(3), magr, angle

    !Get the number of bodies
    n = getLastBodyId()

    do i = 1, n !For every object
      
      !Find the vector r connecting the object to the
      !center of mass of the system
      pos = getPosition(i) 
      poscm = getCenterOfMassPosition()
      r = pos - poscm

      !Find the magnitude of the r-vector
      magr = sqrt(r(1)**2 + r(2)**2 + r(3)**2)     
 
      !Find the best coordinate plane to monitor this orbit
      !(the one in which the projection of the orbit is bigger in area)
      bestplane = getBestProjectionPlane(i)
      
      !If the plane is not defined
      if (bestplane == 0) then
        !Set it as undefined and reset all the orbit parameters
        orbits(i)%bestplane = 0 
        orbits(i)%startangle = 0.0
        orbits(i)%maxrad = 0.0
        orbits(i)%minrad = huge(orbits(i)%minrad)
        cycle
      end if

      !If the best plane has changed
      !(maybe the orbit has changed radically due to the interaction with some massive body)
      if (bestplane /= orbits(i)%bestplane) then
        orbits(i)%bestplane = bestplane !Set the new best plane
        orbits(i)%startangle = getAngleInCoordinatePlane(bestplane, r) !Set the starting angle
        orbits(i)%maxrad = 0.0 !Reset the orbit max radius
        orbits(i)%minrad = huge(orbits(i)%minrad) !Reset the orbit min radius
        orbits(i)%orbitstatus = STARTED !Set that the orbit has started
        orbits(i)%starttime = curtime !Set the starting time of the orbit
        cycle
      end if
      
      !The following two ifs keep track of the maximum and minimum distance
      !of the object from the center of mass of the system until the orbit has started
      
      if (magr >= orbits(i)%maxrad) then
        orbits(i)%maxrad = magr
      end if

      if (magr <= orbits(i)%minrad) then
        orbits(i)%minrad = magr
      end if

      !Get the angle of the object in the coordinate plane that best contains the orbit
      angle = getAngleInCoordinatePlane(orbits(i)%bestplane, r)

      !Check if the object transitate from inside to outside or from outside to inside the starting area
      !In the first case wait for the object to come back to the start
      !In the second case register the orbit as done and start a new one
      if (orbits(i)%orbitStatus == STARTED) then
        !|startangle - angle| > aprec
        if (abs(orbits(i)%startangle - angle) > aprec) then
          orbits(i)%orbitStatus = WAITING
        end if
      elseif (orbits(i)%orbitStatus == WAITING) then
        !|startangle - aprec| < aprec
        if (abs(orbits(i)%startangle - angle) < aprec) then
          orbits(i)%orbitStatus = STARTED
          orbits(i)%completedOrbits = orbits(i)%completedOrbits + 1
          orbits(i)%period = curtime - orbits(i)%starttime
          orbits(i)%starttime = curtime          
        end if
      end if

    end do

  end subroutine updateOrbit

  !Returns the number of completed orbits on the object
  real function getNumCompletedOrbits(bodyid)
    integer, intent(in) :: bodyid
    
    getNumCompletedOrbits = orbits(bodyid)%completedOrbits
    return
  end function getNumCompletedOrbits
  
  !Return the object's minimum distance from the center of mass during this orbit
  real function getMinRadius(bodyid)
    integer, intent(in) :: bodyid
    
    getMinRadius = orbits(bodyid)%minrad
    return
  end function getMinRadius

  !Return the object's maximum distance from the center of mass during this orbit
  real function getMaxRadius(bodyid)
    integer, intent(in) :: bodyid
    
    getMaxRadius = orbits(bodyid)%maxrad
    return
  end function getMaxRadius

  !Return the time the object has took to complete the last orbit
  real function getPeriod(bodyid)
    integer, intent(in) :: bodyid
    
    getPeriod = orbits(bodyid)%period
    return
  end function getPeriod
  
end module Orbit
