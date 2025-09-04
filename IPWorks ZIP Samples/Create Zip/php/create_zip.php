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
require_once('../include/ipworkszip_zip.php');
require_once('../include/ipworkszip_const.php');
?>
<?php
if ($argc < 3) {
  echo "Usage: php create_zip.php name path [recurse]\n\n";
  echo "  name        the name of the zip file to create\n";
  echo "  path        the path of the directory to compress\n";
  echo "  recurse     whether to recurse subdirectories (optional)\n";
  echo "\nExample: php create_zip.php test.zip c:\\mydir recurse\n\n";
} else {
  try {
    $zip = new IPWorksZip_Zip();

    $zip->setArchiveFile($argv[1]);
    $zip->setRecurseSubdirectories(in_array("recurse", $argv));
    $zip->doIncludeFiles($argv[2]);

    echo "Compressing...\n";
    $zip->doCompress();
    echo "Directory compressed.\n";
  } catch (Exception $e) {
    echo "Error: " . $e->getMessage() . "\n";
  }
}
?>