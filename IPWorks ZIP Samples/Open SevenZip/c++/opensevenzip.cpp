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
	SevenZip sevenzip;
	char buffer[LINE_LEN + 1];

	printf("**********************************************************\n");
	printf("* This is a demo of the SevenZip component               *\n");
	printf("**********************************************************\n\n");

	printf("Please enter the name of the archive to extract [samplezip.7z]: ");
	fgets(buffer, LINE_LEN, stdin);
	buffer[strlen(buffer) - 1] = '\0';
	if (strlen(buffer) == 0) strcat(buffer, "samplezip.7z");
	sevenzip.SetArchiveFile(buffer);

	printf("Please enter the path to which the files will be extracted [. (current dir)]: ");
	fgets(buffer, LINE_LEN, stdin);
	buffer[strlen(buffer) - 1] = '\0';
	if (strlen(buffer) == 0) strcat(buffer, ".");
	sevenzip.SetExtractToPath(buffer);

	printf("\nExtracting ...\n\n");
	ret_code = sevenzip.ExtractAll();

	if (!ret_code)
		printf("Completed.\n\n");
	else
		printf("Error extracting: [%i] %s\n", ret_code, sevenzip.GetLastError());

	fprintf(stderr, "\npress <return> to continue...\n");
	getchar();

	return ret_code;
}








