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
using System.Text;
using nsoftware.IPWorksZip;

class zipstreamDemo
{
  private static ZipStream zipstream = new nsoftware.IPWorksZip.ZipStream();

  static void Main(string[] args)
  {
    try
    {
      Console.WriteLine("Type \"?\" for a list of commands.");
      Console.Write("zipstream> ");
      string command;
      string[] arguments;
      while (true)
      {
        command = Console.ReadLine();
        arguments = command.Split();

        if (arguments[0] == "?" || arguments[0] == "help")
        {
          Console.WriteLine("Commands: ");
          Console.WriteLine("  ?                               display the list of valid commands");
          Console.WriteLine("  help                            display the list of valid commands");
          Console.WriteLine("  format <format>                 set the compression format to use, chosen from {Deflate, Zlib, Gzip}");
          Console.WriteLine("  compress <file> <data>          compress text data to the specified file stream");
          Console.WriteLine("  decompress <file>               decompress text data from the specified file stream");
          Console.WriteLine("  quit                            exit the application");
        }
        else if (arguments[0] == "quit" || arguments[0] == "exit")
        {
          break;
        }
        else if (arguments[0] == "format")
        {
          if (arguments.Length > 1) SelectFormat(arguments[1]);
        }
        else if (arguments[0] == "compress")
        {
          if (arguments.Length > 2)
          {
            string text = "";
            for (int i = 2; i < arguments.Length; i++)
            {
              text += arguments[i] + " ";
            }

            using FileStream fs = new FileStream(arguments[1], FileMode.Create);
            using (Stream cs = zipstream.GetCompressionStream(fs))
            {
              byte[] data = Encoding.UTF8.GetBytes(text);
              cs.WriteAsync(data, 0, data.Length);
            }
            Console.WriteLine("Data compressed to file stream.");
          }
        }
        else if (arguments[0] == "decompress")
        {
          if (arguments.Length > 1)
          {
            using FileStream fs = new FileStream(arguments[1], FileMode.Open);
            using (Stream ds = zipstream.GetDecompressionStream(fs))
            {
              StreamReader reader = new StreamReader(ds);
              Console.WriteLine("Data decompressed from file stream: " + reader.ReadToEndAsync());
            }
          }
        }
        else if (arguments[0] == "")
        {
          // Do nothing.
        }
        else
        {
          Console.WriteLine("Invalid command.");
        } // End of command checking.

        Console.Write("zipstream> ");
      }
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static void SelectFormat(string format)
  {
    switch (format.ToLower())
    {
      case "deflate":
        zipstream.StreamFormat = ZipStreamStreamFormats.sfDeflate;
        break;
      case "zlib":
        zipstream.StreamFormat = ZipStreamStreamFormats.sfZlib;
        break;
      case "gzip":
        zipstream.StreamFormat = ZipStreamStreamFormats.sfGzip;
        break;
      default:
        throw new Exception("Invalid format selection.\n");
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
      // Add an key to the dictionary for each argument
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/" then it is a value.
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