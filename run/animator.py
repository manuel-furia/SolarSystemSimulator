#
# animator.py
#
# Generate a series of png for a simulation
#
import sys
import os
import math
from numpy import *
from matplotlib.pyplot import *
from scipy.integrate import ode

#Check command line arguments, and if they are less than required
#display yhe usage info and quit
if len(sys.argv) < 5:
  print "Usage: python animator.py plane paramfile inputfile outputdir"
  print "plane -> The plane of the simulation to draw (xy, xz or yz)"
  print "paramfile -> file with the parameter of the simulation (the input file of the simulator)"
  print "inputfile -> file produced by the simulator"
  print "outputdir -> empty directory (or nonexistent directory) in which the images will be put"
  print "Examples:"
  print "python animator.py xy input.txt sim.txt images"
  print "python animator.py xz --gif --fps 25 input.txt sim.txt animation"
  exit()

#Command line arguments
plane = ""
paramfile = ""
inputfile = ""
outputdir = ""

try:
  #Read the command line arguments
  plane = sys.argv[-4]
  paramfile = sys.argv[-3]
  inputfile = sys.argv[-2]
  outputdir = sys.argv[-1]

  if (not plane in ("xy", "xz", "yz")):
    print "Error in parsing the command line arguments: the plane (first argument) must be xy, xz or yz."
    exit()
 
except:
  print "Error in parsing the command line arguments."
  exit()

#Read all the lines of the parameters file
with open(paramfile, mode='r') as pf:
  paramlines = pf.read().splitlines()

#Filter the result for tailing white lines
paramlines = filter(lambda a: a != "", paramlines)

bodynames = paramlines[6::2] #Names of the celestial bodies
bodyparam = paramlines[7::2] #Initial parameters of the celestial bodies
masses = {} #Temporary dictionary


objects = {}
masses = []

maxmass = 0.0

for i in range(len(bodynames)):
  #Find the max mass
  maxmass = max(maxmass, float(bodyparam[i].split()[0]))
  #Create the object dictionary:
  #each value is a tuple, the first value of the tuple is the mass
  #and the second is a list of 3D tuples representing the positions in time
  objects[bodynames[i]] = (float(bodyparam[i].split()[0]), list())


#Open the simulation file and read all the lines
with open(inputfile, mode='r') as f:
  flines = f.read().splitlines()

for line in flines:
  l = line.split() #l contains all the words in the line (text and numbers)
  nums = [] #This will contain all the float numbers in the line
  name = l[0] #The first word is the name of the object
  for elem in l: #For each word
    try:
      #try to convert the word to float, and if successful
      #add it to the list of numbers
      nums.append(float(elem)) 
    except ValueError:
      pass
  #if len(nums) > 3, we are in a line specifying the position,
  #or else we are in a line specifying the time, and we skip it 
  if (len(nums) > 3): 
    #Get the position from the line (3rd, 4th and 5th numbers in the current file format)
    objects[name][1].append((nums[2], nums[3], nums[4]))
 


fig = figure() # initialize figure
ax = fig.add_subplot(111) # name of the plot

#These will determine the area to draw
max_xcoord = 0.0
min_xcoord = 10e50
max_ycoord = 0.0
min_ycoord = 10e50

#We take the number of steps counting the different position vectors
#with time of the first object in the dictionary
steps = len(objects.values()[0][1])

#For every time step
for i in range(steps):
  #and for every object
  for name, data in objects.iteritems():
    #Get the drawing size of the object by some logarithmic proportion
    #to the mass
    size = max(int(math.log10(data[0]/maxmass) * 2 + 20), 2)

    #Draw the actual point on the graph, considering which plane
    #of the simulation we are drawing
    if plane == "xy":
      ax.plot(data[1][i][0], data[1][i][1],'b.',markersize=size)
      max_xcoord = max(max_xcoord, data[1][i][0])
      min_xcoord = min(min_xcoord, data[1][i][0])
      max_ycoord = max(max_ycoord, data[1][i][1])
      min_ycoord = min(min_ycoord, data[1][i][1])
    elif plane == "xz":
      ax.plot(data[1][i][0], data[1][i][2],'b.',markersize=size)
      max_xcoord = max(max_xcoord, data[1][i][0])
      min_xcoord = min(min_xcoord, data[1][i][0])
      max_ycoord = max(max_ycoord, data[1][i][2])
      min_ycoord = min(min_ycoord, data[1][i][2])
    elif plane == "yz":
      ax.plot(data[1][i][1], data[1][i][2],'b.',markersize=size)
      max_xcoord = max(max_xcoord, data[1][i][1])
      min_xcoord = min(min_xcoord, data[1][i][1])
      max_ycoord = max(max_ycoord, data[1][i][2])
      min_ycoord = min(min_ycoord, data[1][i][2])
    
    #Set the size of the graph such that is enough to contain all objects
    ax.set_xlim([min_xcoord, max_xcoord])
    ax.set_ylim([min_ycoord, max_ycoord])

  #If the output directory doesn't exists...
  if not os.path.exists(outputdir):
    #...create it
    os.makedirs(outputdir)

  #Save the graph on the respective [00..]i.png (i -> current step index)
  fig.savefig(outputdir + "/" + str(i).zfill(5) + ".png")
  print "Saved picture " + str(i+1) + "/" + str(steps)

print "Done!" #Done :)
