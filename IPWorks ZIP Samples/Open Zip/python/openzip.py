# 
# IPWorks ZIP 2024 Python Edition - Sample Project
# 
# This sample project demonstrates the usage of IPWorks ZIP in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
# 
# www.nsoftware.com/ipworkszip
# 
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
# 

import sys
import string
from ipworkszip import *

input = sys.hexversion < 0x03000000 and raw_input or input


def ensureArg(args, prompt, index):
    if len(args) <= index:
        while len(args) <= index:
            args.append(None)
        args[index] = input(prompt)
    elif args[index] is None:
        args[index] = input(prompt)



def fireError(e):
  print("Error %i: %s\n" %(e.code, e.message))

zip = Zip()
try:
  print("Please enter the name of the zip file to extract [samplezip.zip]: "),
  buffer = input()
  if buffer == '':
    zip.set_archive_file("samplezip.zip")
  else:
    zip.set_archive_file(buffer)

  print("Please enter the path for extraction [./extractedfiles/]: "),
  buffer = input()
  if buffer == '':
    zip.set_extract_to_path("./extractedfiles/")
  else:
    zip.set_extract_to_path(buffer)

  zip.extract_all()

  print("Archive extracted.")
  print("\r\nPress enter to continue...")
  input()
  sys.exit(1)
except IPWorksZipError as e:
  fireError(e)

