(*
 * IPWorks ZIP 2022 Delphi Edition - Sample Project
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
unit opensevenzipf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ipzcore, ipztypes, ipzsevenzip, FileCtrl;

type
  TFormOpenSevenZip = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    btnScan: TButton;
    txtArchive: TEdit;
    btnBrowse: TButton;
    lvwFiles: TListView;
    btnExtractAll: TButton;
    btnExtractSelected: TButton;
    txtExtractToPath: TEdit;
    btnSave: TButton;
    ProgressBar1: TProgressBar;
    OpenDialog1: TOpenDialog;
    SevenZip1: TipzSevenZip;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnExtractAllClick(Sender: TObject);
    procedure btnExtractSelectedClick(Sender: TObject);
    procedure SevenZip1Progress(Sender: TObject; Data: string;
      const Filename: string; BytesProcessed: Int64; PercentProcessed: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOpenSevenZip: TFormOpenSevenZip;

implementation

{$R *.dfm}


procedure TFormOpenSevenZip.btnBrowseClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Select a 7-Zip file to scan.';
  OpenDialog1.Filter := '7-Zip Files (*.7z)|*.7z';
  OpenDialog1.InitialDir := GetCurrentDir;
  if (OpenDialog1.Execute) then
  begin
    txtArchive.Text := OpenDialog1.FileName;
  end;
end;

procedure TFormOpenSevenZip.btnScanClick(Sender: TObject);
var
i: Integer;
attrs: AnsiString;
begin

  try
    SevenZip1.Reset;
    lvwFiles.Clear;
    SevenZip1.ArchiveFile := txtArchive.Text;
    SevenZip1.Scan;

    for i := 0 to SevenZip1.FileCount - 1 do begin
	  attrs := '';
      if ((SevenZip1.FileAttributes[i] and $00000001) > 0) then
        attrs := attrs + 'Read Only '
      else if ((SevenZip1.FileAttributes[i] and $00000002) > 0) then
        attrs := attrs + 'Hidden '
      else if ((SevenZip1.FileAttributes[i] and $00000004) > 0) then
        attrs := attrs + 'System '
      else if ((SevenZip1.FileAttributes[i] and $00000010) > 0) then
        attrs := attrs + 'Directory '
      else if ((SevenZip1.FileAttributes[i] and $00000020) > 0) then
        attrs := attrs + 'Archive ';

      lvwFiles.Items.Add();
      lvwFiles.Items[lvwFiles.Items.Count - 1].Caption := SevenZip1.FileDecompressedName[i];
      lvwFiles.Items[lvwFiles.Items.Count - 1].SubItems.Add(IntToStr(SevenZip1.FileDecompressedSize[i]));
      lvwFiles.Items[lvwFiles.Items.Count - 1].SubItems.Add(IntToHex(SevenZip1.FileCRC[i], 4));
      lvwFiles.Items[lvwFiles.Items.Count - 1].SubItems.Add(attrs);
    end;
  except on e: EipzSevenZip do
    ShowMessage(E.Message);
  end;
end;

procedure TFormOpenSevenZip.btnSaveClick(Sender: TObject);
var
DirSelected: UnicodeString;
options: TSelectDirOpts;
begin
  if (SelectDirectory(DirSelected, options, 0)) then
  begin
    txtExtractToPath.Text := DirSelected;
  end;
end;

procedure TFormOpenSevenZip.btnExtractAllClick(Sender: TObject);
begin
  try
    ProgressBar1.Position := 0;
    SevenZip1.ExtractToPath := txtExtractToPath.Text;
    SevenZip1.ExtractAll;
    ShowMessage('Extract Complete.');
  except on E: EipzSevenZip do
    ShowMessage(E.Message);
  end;
end;

procedure TFormOpenSevenZip.btnExtractSelectedClick(Sender: TObject);
var
curListItem: TListItem;
begin
  try
    ProgressBar1.Position := 0;
    SevenZip1.ExtractToPath := txtExtractToPath.Text;

    curListItem := lvwFiles.Selected;
    if Assigned(curListItem) then
    begin
      while Assigned(curListItem) do
      begin
        SevenZip1.Extract(curListItem.Caption);
        curListItem := lvwFiles.GetNextItem(curListItem, sdAll, [isSelected]);
      end;
    end;

    ShowMessage('Extract Complete.');
  except on E: EipzSevenZip do
    ShowMessage(E.Message);
  end;
end;

procedure TFormOpenSevenZip.SevenZip1Progress(Sender: TObject; Data: string;
  const Filename: string; BytesProcessed: Int64; PercentProcessed: Integer);
begin
  ProgressBar1.Position := PercentProcessed;
end;

end.

