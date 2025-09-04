<?php
/*
 * IPWorks ZIP 2024 PHP Edition - Sample Project
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
 */
require_once('../include/ipworkszip_sevenzip.php');
require_once('../include/ipworkszip_const.php');
?>
<?php
if ($argc < 2) {
  echo "Usage: php open_sevenzip.php inputfile\n\n";
  echo "  inputfile       the path to the 7z archive\n";
  echo "\nExample: php open_sevenzip.php samplezip.7z\n\n";
} else {
  try {
    $opensevenzip = new IPWorksZip_SevenZip();

    $opensevenzip->setArchiveFile($argv[1]);

    echo "Type \"?\" or \"help\" for a list of commands.\n";
    echo "opensevenzip> ";
    $command = "";
    $arguments = [];

    while (true) {
      $command = readline();
      $arguments = explode(" ", $command);

      if ($arguments[0] == "?" || $arguments[0] == "help") {
        echo "Commands: \n";
        echo "  ?                                   display the list of valid commands\n";
        echo "  help                                display the list of valid commands\n";
        echo "  scan                                scan the compressed archive\n";
        echo "  extract <files> <path>              extract the specified files to the specified path (separate multiple files with |)\n";
        echo "  extractall <path>                   extract all files in the archive to the specified path\n";
        echo "  quit                                exit the application\n";
      } else if ($arguments[0] == "scan") {
        $opensevenzip->doScan();
        for ($i = 0; $i < $opensevenzip->getFileCount(); $i++) {
          echo $opensevenzip->getFileCompressedName($i), "\n";
        }
      } else if ($arguments[0] == "extract") {
        if (count($arguments) > 2) {
          $opensevenzip->setExtractToPath($arguments[2]);
          $opensevenzip->doExtract($arguments[1]);
        }
      } else if ($arguments[0] == "extractall") {
        if (count($arguments) > 1) {
          $opensevenzip->setExtractToPath($arguments[1]);
          $opensevenzip->doExtractAll();
        }
      } else if ($arguments[0] == "quit" || $arguments[0] == "exit") {
        break;
      } else if ($arguments[0] == "") {
        // Do nothing.
      } else {
        echo "Invalid command.\n";
      } // End of command checking.

      echo "opensevenzip> ";
    }
  } catch (Exception $e) {
    echo "Error: " . $e->getMessage() . "\n";
  }
}
?>