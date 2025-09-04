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
using System.Text;
using nsoftware.IPWorksZip;

class opensevenzipDemo
{
  private static SevenZip opensevenzip = new nsoftware.IPWorksZip.SevenZip();

  static void Main(string[] args)
  {
    if (args.Length < 2)
    {
      Console.WriteLine("usage: opensevenzip /f inputfile\n");
      Console.WriteLine("  inputfile       the path to the 7z archive");
      Console.WriteLine("\nExample: opensevenzip /f ..\\..\\..\\samplezip.7z\n");
    }
    else
    {
      try
      {
        System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        opensevenzip.ArchiveFile = myArgs["f"];

        // Process user commands.
        Console.WriteLine("Type \"?\" or \"help\" for a list of commands.");
        Console.Write("opensevenzip> ");
        string command;
        string[] arguments;

        while (true)
        {
          command = Console.ReadLine();
          arguments = command.Split();

          if (arguments[0] == "?" || arguments[0] == "help")
          {
            Console.WriteLine("Commands: ");
            Console.WriteLine("  ?                                      display the list of valid commands");
            Console.WriteLine("  help                                   display the list of valid commands");
            Console.WriteLine("  scan                                   scan the compressed archive");
            Console.WriteLine("  extract <files> <path>                 extract the specified files to the specified path (separate multiple files with |)");
            Console.WriteLine("  extractall <path>                      extract all files in the archive to the specified path");
            Console.WriteLine("  quit                                   exit the application");
          }
          else if (arguments[0] == "scan")
          {
            opensevenzip.Scan();
            foreach (SevenZipFile file in opensevenzip.Files)
            {
              Console.WriteLine(file.CompressedName);
            }
          }
          else if (arguments[0] == "extract")
          {
            if (arguments.Length > 2)
            {
              opensevenzip.ExtractToPath = arguments[2];
              opensevenzip.Extract(arguments[1]);
            }
          }
          else if (arguments[0] == "extractall")
          {
            if (arguments.Length > 1)
            {
              opensevenzip.ExtractToPath = arguments[1];
              opensevenzip.ExtractAll();
            }
          }
          else if (arguments[0] == "quit" || arguments[0] == "exit")
          {
            break;
          }
          else if (arguments[0] == "")
          {
            // Do nothing.
          }
          else
          {
            Console.WriteLine("Invalid command.");
          } // End of command checking.

          Console.Write("opensevenzip> ");
        }
      }
      catch (Exception ex)
      {
        Console.WriteLine("Error: " + ex.Message);
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