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
unit openzipf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ipzcore, ipztypes, ipzzip, FileCtrl;

type
  TFormOpenzip = class(TForm)
    Label1: TLabel;
    btnScan: TButton;
    Label2: TLabel;
    txtArchive: TEdit;
    btnBrowse: TButton;
    lvwFiles: TListView;
    Label3: TLabel;
    btnExtractAll: TButton;
    btnExtractSelected: TButton;
    Label4: TLabel;
    txtExtractToPath: TEdit;
    btnSave: TButton;
    ProgressBar1: TProgressBar;
    OpenDialog1: TOpenDialog;
    Zip1: TipzZip;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnExtractAllClick(Sender: TObject);
    procedure btnExtractSelectedClick(Sender: TObject);
    procedure Zip1Progress(Sender: TObject; Data: String; DataB: TBytes;
      const Filename: String; BytesProcessed: Int64;
      PercentProcessed: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOpenzip: TFormOpenzip;

implementation

{$R *.dfm}

procedure TFormOpenzip.btnBrowseClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Select a Zip file to scan.';
  OpenDialog1.Filter := 'Zip Files (*.zip)|*.zip';
  OpenDialog1.InitialDir := GetCurrentDir;
  if(OpenDialog1.Execute) then
  begin
      txtArchive.Text := OpenDialog1.FileName;
  end;
end;

procedure TFormOpenzip.btnScanClick(Sender: TObject);
var
i: Integer;
attrs: AnsiString;
begin
  try
  Zip1.Reset;
  lvwFiles.Clear;
  attrs := '';

  Zip1.ArchiveFile := txtArchive.Text;
  Zip1.Scan;

  for i := 0 to Zip1.FileCount -1 do begin
    if((Zip1.FileAttributes[i] and $00000001) >0) then
       attrs := attrs + 'Read Only '
    else if((Zip1.FileAttributes[i] and $00000002) >0) then
       attrs := attrs + 'Hidden '
    else if((Zip1.FileAttributes[i] and $00000004) >0) then
       attrs := attrs + 'System '
    else if((Zip1.FileAttributes[i] and $00000010) >0) then
       attrs := attrs + 'Directory '
    else if((Zip1.FileAttributes[i] and $00000020) >0) then
       attrs := attrs + 'Archive ';

    lvwFiles.Items.Add();
    lvwFiles.Items[lvwFiles.Items.Count - 1].Caption := Zip1.FileDecompressedName[i];
    lvwFiles.Items[lvwFiles.Items.Count - 1].SubItems.Add(IntToStr(Zip1.FileDecompressedSize[i]));
    lvwFiles.Items[lvwFiles.Items.Count - 1].SubItems.Add(IntToHex(Zip1.FileCRC[i],4));
    lvwFiles.Items[lvwFiles.Items.Count - 1].SubItems.Add(attrs);
    lvwFiles.Items[lvwFiles.Items.Count - 1].SubItems.Add(Zip1.FileComment[i]);

    if(Zip1.FilePasswordRequired[i]) then
       lvwFiles.Items[lvwFiles.Items.Count - 1].SubItems.Add('TRUE');
  end;
  
  except on E: EIPWorksZip do
    ShowMessage(E.Message);
  end;
end;

procedure TFormOpenzip.btnSaveClick(Sender: TObject);
var
DirSelected: UnicodeString;
options: TSelectDirOpts;
begin
        if SelectDirectory(DirSelected, options, 0) then
        begin
                txtExtractToPath.Text := DirSelected;
        end;

end;

procedure TFormOpenzip.btnExtractAllClick(Sender: TObject);
begin
  try
    ProgressBar1.Position := 0;
    Zip1.ExtractToPath := txtExtractToPath.Text;
    Zip1.ExtractAll;
    ShowMessage('Extract Complete.');
  except on E: EIPWorksZip do
    ShowMessage(E.Message);
  end;
end;

procedure TFormOpenzip.btnExtractSelectedClick(Sender: TObject);
var
curListItem: TListItem;
begin
  try
    ProgressBar1.Position := 0;
    Zip1.ExtractToPath := txtExtractToPath.Text;

    curListItem := lvwFiles.Selected;
    if Assigned(curListItem) then
    begin
        while Assigned(curListItem) do
        begin
                Zip1.Extract(curListItem.Caption);
                curListItem := lvwFiles.GetNextItem(curListItem,sdAll,[isSelected]);
        end;
    end;

    ShowMessage('Extract Complete.');
  except on E: EIPWorksZip do
    ShowMessage(E.Message);
  end;
end;

procedure TFormOpenzip.Zip1Progress(Sender: TObject; Data: String; DataB: TBytes;
  const Filename: String; BytesProcessed: Int64;
  PercentProcessed: Integer);
begin
  ProgressBar1.Position := PercentProcessed;
end;

end.

