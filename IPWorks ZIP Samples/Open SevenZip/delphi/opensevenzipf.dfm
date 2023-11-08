object FormOpenSevenZip: TFormOpenSevenZip
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Open 7-Zip Demo'
  ClientHeight = 428
  ClientWidth = 697
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 685
    Height = 13
    Caption = 
      'This demo shows how to extract files from an archive using the S' +
      'evenZip component. To begin, simply browse to an archive and the' +
      'n click Scan.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 40
    Height = 13
    Caption = 'Archive:'
  end
  object Label3: TLabel
    Left = 8
    Top = 360
    Width = 42
    Height = 13
    Caption = 'Extract: '
  end
  object Label4: TLabel
    Left = 144
    Top = 360
    Width = 16
    Height = 13
    Caption = 'To:'
  end
  object btnScan: TButton
    Left = 616
    Top = 40
    Width = 75
    Height = 25
    Caption = '&Scan'
    TabOrder = 2
    OnClick = btnScanClick
  end
  object txtArchive: TEdit
    Left = 56
    Top = 40
    Width = 473
    Height = 21
    TabOrder = 0
  end
  object btnBrowse: TButton
    Left = 536
    Top = 40
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object lvwFiles: TListView
    Left = 8
    Top = 72
    Width = 681
    Height = 281
    Columns = <
      item
        Caption = 'Name'
        Width = 150
      end
      item
        Caption = 'Size'
        Width = 100
      end
      item
        Caption = 'CRC32'
        Width = 150
      end
      item
        Caption = 'Attributes'
        Width = 250
      end>
    HideSelection = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 3
    ViewStyle = vsReport
  end
  object btnExtractAll: TButton
    Left = 56
    Top = 360
    Width = 75
    Height = 25
    Caption = '&All'
    TabOrder = 4
    OnClick = btnExtractAllClick
  end
  object btnExtractSelected: TButton
    Left = 56
    Top = 392
    Width = 75
    Height = 25
    Caption = 'S&elected'
    TabOrder = 5
    OnClick = btnExtractSelectedClick
  end
  object txtExtractToPath: TEdit
    Left = 168
    Top = 360
    Width = 489
    Height = 21
    TabOrder = 6
  end
  object btnSave: TButton
    Left = 664
    Top = 360
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 7
    OnClick = btnSaveClick
  end
  object ProgressBar1: TProgressBar
    Left = 144
    Top = 392
    Width = 545
    Height = 25
    TabOrder = 8
  end
  object OpenDialog1: TOpenDialog
    Left = 568
    Top = 40
  end
  object SevenZip1: TipzSevenZip
    OnProgress = SevenZip1Progress
    Left = 584
    Top = 40
  end
end


