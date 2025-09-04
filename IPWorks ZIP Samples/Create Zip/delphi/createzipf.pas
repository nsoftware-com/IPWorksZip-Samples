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
unit createzipf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, FileCtrl, ipzcore, ipztypes, ipzzip;

type
  TFormCreatezip = class(TForm)
    Label1: TLabel;
    btnOpenFolder: TButton;
    label2: TLabel;
    lstLocalFiles: TListView;
    Label3: TLabel;
    txtArchiveFile: TEdit;
    btnSaveFile: TButton;
    txtPassword: TEdit;
    Label4: TLabel;
    cmbEncryption: TComboBox;
    btnZip: TButton;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    Zip1: TipzZip;
    lblDirectory: TLabel;
    procedure btnOpenFolderClick(Sender: TObject);
    procedure btnSaveFileClick(Sender: TObject);
    procedure btnZipClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Zip1Progress(Sender: TObject; Data: string;
      DataB: TBytes; const Filename: string; BytesProcessed: Int64; PercentProcessed: Integer);
  private
    { Private declarations }
    procedure UpdateLocal;
  public
    { Public declarations }
  end;

var
  FormCreatezip: TFormCreatezip;

implementation

{$R *.dfm}

procedure TFormCreatezip.btnOpenFolderClick(Sender: TObject);
var
  DirSelected: UnicodeString;
  options: TSelectDirOpts;
begin
  if SelectDirectory(DirSelected, options, 0) then
  begin
    ChDir(DirSelected);
    lblDirectory.Caption := DirSelected;
    UpdateLocal;
  end;
end;

procedure TFormCreatezip.UpdateLocal;
var
  SearchRec: TSearchRec;
begin
  Screen.Cursor := crAppStart;
  lstLocalFiles.Clear;

  if FindFirst('*', faAnyFile, SearchRec) = 0 then
  repeat
    lstLocalFiles.Items.Add();
    if (SearchRec.Attr and faDirectory) <> 0 then
      lstLocalFiles.Items[lstLocalFiles.Items.Count - 1].Caption := '<DIR>  ' + SearchRec.Name
    else
      lstLocalFiles.Items[lstLocalFiles.Items.Count - 1].Caption := SearchRec.Name;

    lstLocalFiles.Items[lstLocalFiles.Items.Count - 1].SubItems.Add(IntToStr(SearchRec.Size));

    lstLocalFiles.Items[lstLocalFiles.Items.Count - 1].SubItems.Add(DateTimeToStr(FileDateToDateTime(SearchRec.Time)));
  until FindNext(SearchRec) <> 0;

  FindClose(SearchRec);
  Screen.Cursor := crDefault;
end;

procedure TFormCreatezip.btnSaveFileClick(Sender: TObject);
begin
  SaveDialog1.Title := 'Save your zip file.';
  SaveDialog1.Filter := 'Zip Files (*.zip)|*.zip';
  if SaveDialog1.Execute then
  begin
    txtArchiveFile.Text := SaveDialog1.FileName;
  end;
end;

procedure TFormCreatezip.btnZipClick(Sender: TObject);
var
  curListItem : TListItem;
begin
  try
    Zip1.Reset;
    ProgressBar1.Position := 0;
    Zip1.ArchiveFile := txtArchiveFile.Text;

    curListItem := lstLocalFiles.Selected;
    if Assigned(curListItem) then
    begin
      while Assigned(curListItem) do
      begin
        Zip1.IncludeFiles(lblDirectory.Caption + '\' + curListItem.Caption);
        curListItem := lstLocalFiles.GetNextItem(curListItem,sdAll,[isSelected]);
      end;
    end;

    // You can also add files to an archive using a file maks for instance:
    // Zip1.IncludeFiles('c:\*.txt');

    if not(txtPassword.Text = '') then
    begin
      Zip1.EncryptionAlgorithm := TipzZipEncryptionAlgorithms(cmbEncryption.ItemIndex);
      Zip1.Password := txtPassword.Text;
    end;

    Zip1.Compress;

    ShowMessage('Zip complete.');
    
  except on E: EIPWorksZip do
    ShowMessage(E.Message);
  end;

  Zip1.ArchiveFile := ''; // Release the handle on the archive file.
end;

procedure TFormCreatezip.FormCreate(Sender: TObject);
begin
  cmbEncryption.ItemIndex := 0;
  lblDirectory.Caption := GetCurrentDir;
  ChDir(GetCurrentDir);
  UpdateLocal;
end;

procedure TFormCreatezip.Zip1Progress(Sender: TObject; Data: string;
  DataB: TBytes; const Filename: string; BytesProcessed: Int64; PercentProcessed: Integer);
begin
  ProgressBar1.Position := PercentProcessed;
end;

end.

