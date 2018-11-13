#
#  SimGen
#
#  Generate a simulation input parameter file
#
#  The code is straightforward, it only takes the parameters as input
#  from the keyboard and put them in a file with the required format
#

import io

timestep = float(raw_input("Insert the time step (dt) in seconds: "))
duration = float(raw_input("Insert the duration of the simulation in seconds: "))
numsaving = int(raw_input("Insert the total number of states to save in the output file: "))
debugevery = int(raw_input("How many saved steps before printing the data on screen (-1 for never print): "))
aprec = float(raw_input("Insert the angular precision of the orbit analyzer (adviced: 0.001): "))
num = int(raw_input("Insert the number of celestial bodies in the simulation: "))

objs = list()

for i in range(num):
  name = raw_input("Insert the name of the object: ")
  mass = float(raw_input("Insert the mass of the object (kg):"))
  x = float(raw_input("Insert the X coordinate (m):"))
  y = float(raw_input("Insert the Y coordinate (m):"))
  z = float(raw_input("Insert the Z coordinate (m):"))
  vx = float(raw_input("Insert the VX velocity component (m/s):"))
  vy = float(raw_input("Insert the VY velocity component (m/s):"))
  vz = float(raw_input("Insert the VZ velocity component (m/s):"))

  objs.append((name, mass, x, y, z, vx, vy, vz))

print "\n\n"
print "Summary:"
print "Duration: ", duration
print "Time step: ", timestep
print "Number of steps: ", int(duration/timestep)
print "Number of states that will be saved: ", numsaving
print "Print on screen every ", debugevery, " states"
print "Number screen printings: ", (numsaving/debugevery)
print "Angular precision of Orbit: ", aprec
print "Number of celestial bodies: ", num

raw_input("Press enter to see the object initial parameters:")

for i in range(num):
  print "-- Name: ", objs[i][0]
  print "Mass: ", objs[i][1]
  print "Initial position: ({0}, {1}, {2})".format(objs[i][2], objs[i][3], objs[i][4])
  print "Initial velocity: ({0}, {1}, {2})".format(objs[i][5], objs[i][6], objs[i][7])

raw_input("Press enter to continue:")

filename = raw_input("Insert the name of the file to save:")

lines = list()

f = open(filename, 'w')

lines.append(str(timestep).upper())
lines.append(str(duration).upper())
lines.append(str(numsaving).upper())
lines.append(str(debugevery).upper())
lines.append(str(aprec).upper())
lines.append(str(num).upper())

for obj in objs:
  lastline = ""
  lines.append(obj[0])
  for i in range(1,8):
    lastline = lastline + str(obj[i]) + " "
  lines.append(lastline)

text = "\n".join(lines)

f.write(text)

f.close()

print "Simulation input file <" + filename + "> written."

