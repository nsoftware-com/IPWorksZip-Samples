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

program selfextractor;

uses
  Forms,
  selfextractorf in 'selfextractorf.pas' {FormSelfextractor};

begin
  Application.Initialize;

  Application.CreateForm(TFormSelfextractor, FormSelfextractor);
  Application.Run;
end.


         
