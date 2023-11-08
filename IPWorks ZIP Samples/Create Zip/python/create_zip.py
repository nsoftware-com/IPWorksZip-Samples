# 
# IPWorks ZIP 2022 Python Edition - Sample Project
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

input = sys.hexversion<0x03000000 and raw_input or input


def fireError(e):
  print("Error %i: %s\n" %(e.code, e.message))

try:
  zip = Zip()

  print("Please enter the name of the zip file to create [test.zip]: "),
  buffer = input()
  if buffer != '':
    zip.set_archive_file(buffer)
  else:
    zip.set_archive_file("test.zip")

  print("Recurse? [N]: "),
  buffer = input().lower()

  if buffer == 'n':
    zip.set_recurse_subdirectories(False)

  print("Please enter the path of the directory to compress [.]: "),
  buffer = input()
  if buffer != '':
    zip.include_files(buffer)
  else:
    zip.include_files("./*")

  print("Compressing...")
  zip.compress()
  print("Directory compressed.")
except IPWorksZipError as e:
  fireError(e)


