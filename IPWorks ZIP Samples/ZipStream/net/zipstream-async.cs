/*
 * IPWorks ZIP 2022 .NET Edition - Sample Project
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

using System.Collections.Generic;
﻿using System;
using System.IO;
using System.Text;
using System.Threading.Tasks;
using nsoftware.async.IPWorksZip;

class zipstreamDemo
{
  private static Zipstream zipstream = new nsoftware.async.IPWorksZip.Zipstream();

  static async Task Main(string[] args)
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

            await using FileStream fs = new FileStream(arguments[1], FileMode.Create);
            await using (Stream cs = await zipstream.GetCompressionStream(fs))
            {
              byte[] data = Encoding.UTF8.GetBytes(text);
              await cs.WriteAsync(data, 0, data.Length);
            }
            Console.WriteLine("Data compressed to file stream.");
          }
        }
        else if (arguments[0] == "decompress")
        {
          if (arguments.Length > 1)
          {
            await using FileStream fs = new FileStream(arguments[1], FileMode.Open);
            await using (Stream ds = await zipstream.GetDecompressionStream(fs))
            {
              StreamReader reader = new StreamReader(ds);
              Console.WriteLine("Data decompressed from file stream: " + await reader.ReadToEndAsync());
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
        zipstream.StreamFormat = ZipstreamStreamFormats.sfDeflate;
        break;
      case "zlib":
        zipstream.StreamFormat = ZipstreamStreamFormats.sfZlib;
        break;
      case "gzip":
        zipstream.StreamFormat = ZipstreamStreamFormats.sfGzip;
        break;
      default:
        throw new Exception("Invalid format selection.\n");
    }
  }
}


class ConsoleDemo
{
  public static Dictionary<string, string> ParseArgs(string[] args)
  {
    Dictionary<string, string> dict = new Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // If it starts with a "/" check the next argument.
      // If the next argument does NOT start with a "/" then this is paired, and the next argument is the value.
      // Otherwise, the next argument starts with a "/" and the current argument is a switch.

      // If it doesn't start with a "/" then it's not paired and we assume it's a standalone argument.

      if (args[i].StartsWith("/"))
      {
        // Either a paired argument or a switch.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Paired argument.
          dict.Add(args[i].TrimStart('/'), args[i + 1]);
          // Skip the value in the next iteration.
          i++;
        }
        else
        {
          // Switch, no value.
          dict.Add(args[i].TrimStart('/'), "");
        }
      }
      else
      {
        // Standalone argument. The argument is the value, use the index as a key.
        dict.Add(i.ToString(), args[i]);
      }
    }
    return dict;
  }

  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}