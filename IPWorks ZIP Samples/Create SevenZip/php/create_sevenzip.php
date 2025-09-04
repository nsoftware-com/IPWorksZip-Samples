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
if ($argc < 3) {
  echo "Usage: php create_sevenzip.php name path [recurse]\n\n";
  echo "  name        the name of the 7z file to create\n";
  echo "  path        the path of the directory to compress\n";
  echo "  recurse     whether to recurse subdirectories (optional)\n";
  echo "\nExample: php create_sevenzip.php test.7z c:\\mydir recurse\n\n";
} else {
  try {
    $sevenzip = new IPWorksZip_SevenZip();

    $sevenzip->setArchiveFile($argv[1]);
    $sevenzip->setRecurseSubdirectories(in_array("recurse", $argv));
    $sevenzip->doIncludeFiles($argv[2]);

    echo "Compressing...\n";
    $sevenzip->doCompress();
    echo "Directory compressed.\n";
  } catch (Exception $e) {
    echo "Error: " . $e->getMessage() . "\n";
  }
}
?>