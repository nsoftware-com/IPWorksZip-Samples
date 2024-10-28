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
unit selfextractorf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, FileCtrl, ipzcore, ipztypes, ipzzipsfx;

type
  TFormSelfExtractor = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    txtSourceDir: TEdit;
    txtOutputName: TEdit;
    txtExtractorTitle: TEdit;
    btnBrowse: TButton;
    Label5: TLabel;
    btnCreate: TButton;
    memBannerText: TMemo;
    ProgressBar1: TProgressBar;
    ZipSFX1: TipzZipSFX;
    SaveDialog1: TSaveDialog;
    btnSaveOutput: TButton;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure ZipSFX1Progress(Sender: TObject; const Filename: string;
      BytesProcessed: Int64; PercentProcessed: Integer);
    procedure btnSaveOutputClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSelfExtractor: TFormSelfExtractor;

implementation

{$R *.dfm}

procedure TFormSelfExtractor.btnBrowseClick(Sender: TObject);
var
DirSelected: UnicodeString;
options: TSelectDirOpts;
begin
  if SelectDirectory(DirSelected, options, 0) then
  begin
    ChDir(DirSelected);
    txtSourceDir.Text := DirSelected;
  end;
end;

procedure TFormSelfExtractor.btnSaveOutputClick(Sender: TObject);
begin
  SaveDialog1.Title := 'Save your EXE file.';
  SaveDialog1.Filter := 'Exe Files (*.exe)|*.exe';
  if SaveDialog1.Execute then
  begin
     txtOutputName.Text := SaveDialog1.FileName;
  end;
end;

procedure TFormSelfExtractor.btnCreateClick(Sender: TObject);
begin
  ZipSFX1.SourceDirectory := txtSourceDir.Text;
  ZipSFX1.ArchiveFile := txtOutputName.Text;
  ZipSFX1.CaptionText := txtExtractorTitle.Text;
  ZipSFX1.BannerText := memBannerText.Text;

  ZipSFX1.RecurseSubdirectories := true;

  ZipSFX1.CreateSFX();

  ShowMessage('Complete!');
end;

procedure TFormSelfExtractor.ZipSFX1Progress(Sender: TObject;
  const Filename: string; BytesProcessed: Int64; PercentProcessed: Integer);
var test: Integer;
begin
  ProgressBar1.Position := PercentProcessed;
end;

end.

