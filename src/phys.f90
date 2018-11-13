!This module applies the gravity force on the object, calculate the accelerations
!and the positions and velocities at each istant of time

module Phys
  use Celestial
  implicit none

  !curtime -> current time of the simulation
  !timestep -> increment of curtime at every step
  real(kind=8), private :: curtime, timestep

contains

  !Initialize the module
  subroutine initPhys(deltat)
    real(kind=8), intent(in) :: deltat
    
    curtime = 0.0
    timestep = deltat  

  end subroutine initPhys
  
  !Force that body2 exert on body1
  function gravityForce(mass1, mass2, pos1, pos2)
    real(kind=8), dimension(3) :: gravityForce
    real(kind=8), dimension(3), intent(in) :: pos1, pos2
    real(kind=8), intent(in) :: mass1, mass2
    
    real(kind=8), parameter :: G = 6.674d-11
    real(kind=8), dimension(3) :: r
    real(kind=8), dimension(3) :: versor
    real(kind=8) :: forcemag, rmag

    r = (pos2 - pos1)
    rmag = sqrt(r(1)**2 + r(2)**2 + r(3)**2) !Get the magnitude of r
    versor = r / rmag !Get the versor of the direction of r
    forcemag = (G  * mass1 * mass2) / (rmag**2) !Universal law of gravitation
    gravityForce = forcemag * versor

    return
  end function gravityForce

  !Calculate positions, velocity and accelerations for the next step
  !and return the current time of the simulation after the increment
  real(kind=8) function calculateNext()
    
    !Properties of the object we are modifying
    real(kind=8) :: mass, pos(3), vel(3), acc(3), force(3)
    !Properties of the object that is acting on the object we are modifying
    real(kind=8) :: mass2, pos2(3)
    !Number of objects and counters
    integer :: n, i, j

    !Get the number of objects
    n = getLastBodyId()
    
    do i = 1, n !Iterate through all the objects
      !Get the properties of the current object
      mass = getMass(i)
      pos = getPosition(i)
      vel = getVelocity(i)
      acc = (/ 0, 0, 0 /)

      !Verlet algorithm(acceleration)
      do j = 1, n !Iterate though all the object except the current one
        if (i /= j) then
          !Calculate the acceleration caused by the acting object
          mass2 = getMass(j)
          pos2 = getPosition(j)
          force = gravityForce(mass, mass2, pos, pos2)
          acc = acc + (force / mass)
        end if
      end do

      !Verlet algorithm(position)
      pos = pos + vel * timestep + 0.5 * acc * (timestep**2)

      !Verlet algorithm(velocity) NOTE: getAcceleration(i) contains a_i, acc contains a_i+1
      vel = vel + 0.5 * (getAcceleration(i) + acc) * timestep      
      
      !Set the updated properties
      call setPosition(i, pos)
      call setVelocity(i, vel)
      call setAcceleration(i, acc)
      
    end do
    
    !Increment the current time by the timestep and return it
    curtime = curtime + timestep
    calculateNext = curtime
    return
  end function calculateNext
  
end module Phys
