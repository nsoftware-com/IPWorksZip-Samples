/*
 * IPWorks ZIP 2024 C++ Edition - Sample Project
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "../../include/ipworkszip.h"
#define LINE_LEN 100

int main()
{

	int ret_code = 0;
	SevenZip sevenzip1;
	char buffer[LINE_LEN + 1];

	printf("**********************************************************\n");
	printf("* This is a demo of the SevenZip component               *\n");
	printf("**********************************************************\n\n");

	printf("Please enter the name of the archive to create [test.7z]: ");
	fgets(buffer, LINE_LEN, stdin);
	buffer[strlen(buffer) - 1] = '\0';
	if (strlen(buffer) == 0) strcat(buffer, "test.7z");
	sevenzip1.SetArchiveFile(buffer);

	printf("Recurse Sub-Directories? [N]: ");
	fgets(buffer, LINE_LEN, stdin);
	buffer[strlen(buffer) - 1] = '\0';
	if (strlen(buffer) == 0) strcat(buffer, "N");

	if (strcmp("n", buffer) == 0 || strcmp("N", buffer) == 0)
		sevenzip1.SetRecurseSubdirectories(false);

	printf("Please enter the path and filemask of the directory to compress [*.cpp]: ");
	fgets(buffer, LINE_LEN, stdin);
	buffer[strlen(buffer) - 1] = '\0';
	if (strlen(buffer) == 0) strcat(buffer, "*.cpp");
	sevenzip1.IncludeFiles(buffer);

	printf("\nCompressing ...\n\n");
	ret_code = sevenzip1.Compress();

	if (!ret_code)
		printf("Completed.\n\n");
	else
		printf("Error compressing: [%i] %s\n", ret_code, sevenzip1.GetLastError());


	fprintf(stderr, "\npress <return> to continue...\n");
	getchar();

	return ret_code;
}





