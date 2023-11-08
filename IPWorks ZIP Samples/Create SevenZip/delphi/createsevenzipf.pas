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
unit createsevenzipf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, FileCtrl, ipzcore, ipztypes, ipzzip, ipzsevenzip;

type
  TFormCreatesevenzip = class(TForm)
    Label1: TLabel;
    btnOpenFolder: TButton;
    label2: TLabel;
    lstLocalFiles: TListView;
    Label3: TLabel;
    txtArchiveFile: TEdit;
    btnSaveFile: TButton;
    txtPassword: TEdit;
    Label4: TLabel;
    btnZip: TButton;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    lblDirectory: TLabel;
    SevenZip1: TipzSevenZip;
    procedure btnOpenFolderClick(Sender: TObject);
    procedure btnSaveFileClick(Sender: TObject);
    procedure btnZipClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SevenZip1Progress(Sender: TObject; Data: string;
      const Filename: string; BytesProcessed: Int64; PercentProcessed: Integer);
  private
    { Private declarations }
    procedure UpdateLocal;
  public
    { Public declarations }
  end;

var
  FormCreatesevenzip: TFormCreatesevenzip;

implementation

{$R *.dfm}

procedure TFormCreatesevenzip.btnOpenFolderClick(Sender: TObject);
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

procedure TFormCreatesevenzip.UpdateLocal;
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

procedure TFormCreatesevenzip.btnSaveFileClick(Sender: TObject);
begin
  SaveDialog1.Title := 'Save your 7z file.';
  SaveDialog1.Filter := 'SevenZip Files (*.7z)|*.7z';
  SaveDialog1.FileName := txtArchiveFile.Text;
  if SaveDialog1.Execute then
  begin
     txtArchiveFile.Text := SaveDialog1.FileName;
  end;
end;

procedure TFormCreatesevenzip.btnZipClick(Sender: TObject);
var
curListItem : TListItem;
begin
  try
    SevenZip1.Reset;
    ProgressBar1.Position := 0;
    SevenZip1.ArchiveFile := txtArchiveFile.Text;

    curListItem := lstLocalFiles.Selected;
    if Assigned(curListItem) then
    begin
        while Assigned(curListItem) do
        begin
                SevenZip1.IncludeFiles(lblDirectory.Caption + '\' + curListItem.Caption);
                curListItem := lstLocalFiles.GetNextItem(curListItem,sdAll,[isSelected]);
        end;
    end;

    //You can also add files to an archive using a filemask for instance:
    //SevenZip1.IncludeFiles('c:\*.txt');

    if not(txtPassword.Text = '') then
    begin
        SevenZip1.Password := txtPassword.Text;

    end;

    SevenZip1.Compress;

    ShowMessage('SevenZip complete.');
    
  except on E: EipzZip do
    ShowMessage(E.Message);
  end;

    SevenZip1.ArchiveFile := ''; //Release the handle on the archive file.
end;

procedure TFormCreatesevenzip.FormCreate(Sender: TObject);
begin
 lblDirectory.Caption := GetCurrentDir;
 ChDir(GetCurrentDir);
 UpdateLocal;
end;

procedure TFormCreatesevenzip.SevenZip1Progress(Sender: TObject; Data: string;
  const Filename: string; BytesProcessed: Int64; PercentProcessed: Integer);
begin
     ProgressBar1.Position := PercentProcessed;
end;

end.

