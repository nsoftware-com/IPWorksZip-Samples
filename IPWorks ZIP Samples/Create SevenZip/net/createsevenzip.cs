/*
 * IPWorks ZIP 2024 .NET Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks ZIP in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworkszip
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 * 
 */

﻿using System;
using System.IO;
using nsoftware.IPWorksZip;

class createsevenzipDemo
{
  private static SevenZip sevenzip = new nsoftware.IPWorksZip.SevenZip();

  static void Main(string[] args)
  {
    if (args.Length < 4)
    {
      Console.WriteLine("usage: createsevenzip /n name /p path [/r]\n");
      Console.WriteLine("  name     the name of the 7z file to create");
      Console.WriteLine("  path     the path of the directory to compress");
      Console.WriteLine("  /r       whether to recurse subdirectories (optional)");
      Console.WriteLine("\nExample: createsevenzip /n test.7z /p c:\\mydir /r\n");
    }
    else
    {
      try
      {
        System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        sevenzip.ArchiveFile = myArgs["n"];
        sevenzip.RecurseSubdirectories = myArgs.ContainsKey("r");
        sevenzip.IncludeFiles(myArgs["p"]);

        Console.WriteLine("Compressing...");
        sevenzip.Compress();
        Console.WriteLine("Directory compressed.");
      }
      catch (IPWorksZipException ex)
      {
        Console.WriteLine($"IPWorks ZIP exception thrown: {ex.Code} [{ex.Message}].");
      }
      catch (Exception ex)
      {
        Console.WriteLine(ex.Message);
      }
    }
  }
}




class ConsoleDemo
{
  /// <summary>
  /// Takes a list of switch arguments or name-value arguments and turns it into a dictionary.
  /// </summary>
  public static System.Collections.Generic.Dictionary<string, string> ParseArgs(string[] args)
  {
    System.Collections.Generic.Dictionary<string, string> dict = new System.Collections.Generic.Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // Add a key to the dictionary for each argument.
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/", then it is a value.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Save the value and skip the next entry in the list of arguments.
          dict.Add(args[i].ToLower().TrimStart('/'), args[i + 1]);
          i++;
        }
        else
        {
          // If the next argument starts with a "/", then we assume the current one is a switch.
          dict.Add(args[i].ToLower().TrimStart('/'), "");
        }
      }
      else
      {
        // If the argument does not start with a "/", store the argument based on the index.
        dict.Add(i.ToString(), args[i].ToLower());
      }
    }
    return dict;
  }
  /// <summary>
  /// Asks for user input interactively and returns the string response.
  /// </summary>
  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}