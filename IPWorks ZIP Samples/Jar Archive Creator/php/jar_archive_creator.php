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
require_once('../include/ipworkszip_jar.php');
require_once('../include/ipworkszip_const.php');
?>
<?php
if ($argc < 3) {
  echo "Usage: php jar_archive_creator.php name path [recurse]\n\n";
  echo "  name        the name of the archive to create\n";
  echo "  path        the files to compress\n";
  echo "  recurse     whether to recurse subdirectories (optional)\n";
  echo "\nExample: php jar_archive_creator.php test.jar c:\\mydir\\*.class\n\n";
} else {
  try {
    $jar = new IPWorksZip_Jar();

    $jar->setArchiveFile($argv[1]);
    $jar->setRecurseSubdirectories(in_array("recurse", $argv));
    $jar->doIncludeFiles($argv[2]);

    echo "Compressing...\n";
    $jar->doCompress();
    echo "Archive compressed.\n";
  } catch (Exception $e) {
    echo "Error: " . $e->getMessage() . "\n";
  }
}
?>