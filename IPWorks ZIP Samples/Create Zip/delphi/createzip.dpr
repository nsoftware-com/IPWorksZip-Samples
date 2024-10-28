(*
 * IPWorks ZIP 2024 Delphi Edition - Sample Project
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
 *)

program createzip;

uses
  Forms,
  createzipf in 'createzipf.pas' {FormCreatezip};

begin
  Application.Initialize;

  Application.CreateForm(TFormCreatezip, FormCreatezip);
  Application.Run;
end.


         
