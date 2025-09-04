/*
 * IPWorks ZIP 2024 JavaScript Edition - Sample Project
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
 
const readline = require("readline");
const ipworkszip = require("@nsoftware/ipworkszip");

if(!ipworkszip) {
  console.error("Cannot find ipworkszip.");
  process.exit(1);
}
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

main();

async function main() {
  const zip = new ipworkszip.zip();

  prompt('archivefile','Please enter the name of the zip file to create', ':', 'test.zip');

  rl.on('line', async function(line)
  {
    switch(lastPrompt)
    {
      case 'archivefile':
      {
        if(line === '')
          zip.setArchiveFile(lastDefault);
        else
          zip.setArchiveFile(line);

        prompt('recurse','Recurse?',':','n');
        break;
      }
      case 'recurse':
      {
        if(line !== '')
          zip.setRecurseSubdirectories(true);

        prompt('folder','Please enter the path of the directory to compress',':','./*');
        break;
      }
      case 'folder':
      {
        if(line === '')
          zip.includeFiles(lastDefault);
        else
          zip.includeFiles(line);

        console.log('Compressing ...');
        try {
          await zip.compress();
          console.log('Compressed directory');
          process.exit();
        }
        catch (e) {
          console.log(e);
          process.exit();
        }
      }
    }
  });
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}]${punctuation} `);
}
