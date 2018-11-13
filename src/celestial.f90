!This module keeps track of the celestial bodies in the simulation and their properties

module Celestial
  implicit none


  !Represent a celestial body, with mass, position,
  !velocity, acceleration and a name
  type :: Body
    real(kind=8) :: mass, pos(3), vel(3), acc(3)
    character(len=50) :: bodyname
  end type Body

  !lastid -> id of the last celestial object in the module
  !NOTE: since fortran has base 1 indeces, lastid equals the number of objects
  !capacity -> the maximum amount of object the module can posses
  integer, private :: lastid, capacity
 
  !The celestial objects
  type(Body), private, allocatable :: bodies(:)

  
contains
  
  !Inizialize the variables in the module and allocate the space
  !for celestial bodies
  !num -> Number of maximum celestial bodies
  subroutine initCelestial(num)
    integer, intent(in) :: num

    capacity = num
    lastid = 0

    allocate(bodies(num))

  end subroutine initCelestial

  !Deallocate the space previously allocated for the celestial bodies
  subroutine finalizeCelestial()

    deallocate(bodies)

  end subroutine finalizeCelestial

  subroutine setAcceleration(bodyid, acc)
    integer, intent(in) :: bodyid
    real(kind=8), intent(in) :: acc(3)

    bodies(bodyid)%acc = acc

  end subroutine setAcceleration

  !Set the velocity of the body identified by bodyid to the value vel
  subroutine setVelocity(bodyid, vel)
    integer, intent(in) :: bodyid
    real(kind=8), intent(in) :: vel(3)

    bodies(bodyid)%vel = vel

  end subroutine setVelocity

  !Set the position of the body identified by bodyid to the value pos
  subroutine setPosition(bodyid, pos)
    integer, intent(in) :: bodyid
    real(kind=8), intent(in) :: pos(3)

    bodies(bodyid)%pos = pos

  end subroutine setPosition

  !Add a body to the module by specifying its parameters
  integer function addBody(bodyname, mass, pos, vel)
    character(len=50), intent(in) :: bodyname
    real(kind=8), intent(in) :: mass, pos(3), vel(3)
    
    !If we reached the maximum capacity, return -1 (invalid index)
    if (lastid >= capacity) then
      addBody = -1
      return
    end if

    lastid = lastid + 1 !We are adding an element, so we get a free id for it
    
    !Set the parameters for the new body
    bodies(lastid)%mass = mass
    bodies(lastid)%pos = pos
    bodies(lastid)%vel = vel
    bodies(lastid)%bodyname = bodyname

    !Return the id of the new element
    addBody = lastid
    return

  end function addBody

  !Get the mass of the body identified by bodyid
  real(kind=8) function getMass(bodyid)
    integer, intent(in) :: bodyid
    
    getMass = bodies(bodyid)%mass
    return
  end function getMass

  !Get the acceleration of the body identified by bodyid
  function getAcceleration(bodyid)
    real(kind=8),dimension(3) :: getAcceleration
    integer, intent(in) :: bodyid
    
    getAcceleration = bodies(bodyid)%acc
    return
  end function getAcceleration

  !Get the velocity (vector) of the body identified by bodyid
  function getVelocity(bodyid)
    real(kind=8),dimension(3) :: getVelocity
    integer, intent(in) :: bodyid
    
    getVelocity = bodies(bodyid)%vel
    return
  end function getVelocity

  !Get the mass of the position identified by bodyid
  function getPosition(bodyid)
    real(kind=8), dimension(3) :: getPosition    
    integer, intent(in) :: bodyid
    
    getPosition = bodies(bodyid)%pos
    return
  end function getPosition

  !Get the name of the body identified by bodyid
  character(len=50) function getName(bodyid)
    integer, intent(in) :: bodyid
    
    getName = bodies(bodyid)%bodyname
    return
  end function getName

  !Get the id of the last body (equivalent to the number of bodies)
  integer function getLastBodyId()

    getLastBodyId = lastid
    return
  end function getLastBodyId

  !Get the maximum quantity of bodies this module has been initialized to handle
  integer function getMaxBodiesNumber()

    getMaxBodiesNumber = capacity
    return
  end function getMaxBodiesNumber

  !Get the position of the center of mass
  function getCenterOfMassPosition()
    real(kind=8), dimension(3) :: getCenterOfMassPosition   
    real(kind=8) :: totalmass, cm(3)
    integer :: i

    cm = (/ 0.0, 0.0, 0.0 /)
    totalmass = 0.0

    !The sum: sum_1^i (r_i * m_i)
    !and also calculate the totalmass
    do i = 1, lastid
      cm = bodies(i)%pos * bodies(i)%mass
      totalmass = totalmass + bodies(i)%mass
    end do
    
    !cm = sum_1^i (r_i * m_i) / M
    cm = cm / totalmass

    getCenterOfMassPosition = cm
    return
  end function getCenterOfMassPosition

  !Get the velocity of the center of mass
  function getCenterOfMassVelocity()
    real(kind=8), dimension(3) :: getCenterOfMassVelocity   
    real(kind=8) :: totalmass, cmv(3)
    integer :: i

    cmv = (/ 0.0, 0.0, 0.0 /)
    totalmass = 0.0

    !The sum: sum_1^i (v_i * m_i)
    !and also calculate the totalmass
    do i = 1, lastid
      cmv = bodies(i)%vel * bodies(i)%mass
      totalmass = totalmass + bodies(i)%mass
    end do

    !cmv = sum_1^i (v_i * m_i) / M
    cmv = cmv / totalmass

    getCenterOfMassVelocity = cmv
    return
  end function getCenterOfMassVelocity
  
  !Get the distance of a body from the center of mass
  real function getDistanceFromCenterOfMass(bodyid)
    integer, intent(in) :: bodyid
    real(kind=8) :: pos(3), cm(3), r(3)

    cm = getCenterOfMassPosition()
    pos = getPosition(bodyid)
    r = pos - cm

    !Get the magnitude of the r-vector, that's the distance
    getDistanceFromCenterOfMass = sqrt(r(1)**2 + r(2)**2 + r(3)**2)
    return
  end function getDistanceFromCenterOfMass
 
  !Get the speed (scalar) of a body
  real function getSpeed(bodyid)
    integer, intent(in) :: bodyid
    real(kind=8) :: vel(3)

    vel = getVelocity(bodyid)
    
    !The speed is the magnitude of the velocity
    getSpeed = sqrt(vel(1)**2 + vel(2)**2 + vel(3)**2)
    return
  end function
 
end module Celestial
