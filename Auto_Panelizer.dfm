object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 424
  ClientWidth = 356
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
    Left = 22
    Top = 21
    Width = 76
    Height = 13
    Caption = 'Search for PCB:'
  end
  object GroupBox2: TGroupBox
    Left = 20
    Top = 160
    Width = 316
    Height = 144
    Caption = 'Panel Dimensions'
    TabOrder = 6
    object Label2: TLabel
      Left = 10
      Top = 30
      Width = 110
      Height = 13
      Caption = 'Number of Boards in X:'
    end
    object Label3: TLabel
      Left = 10
      Top = 65
      Width = 110
      Height = 13
      Caption = 'Number of Boards in Y:'
    end
    object Label8: TLabel
      Left = 10
      Top = 100
      Width = 113
      Height = 13
      Caption = 'Total Panel Dimensions:'
    end
    object Label9: TLabel
      Left = 270
      Top = 96
      Width = 32
      Height = 21
      Caption = 'mm'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object YEntry: TEdit
      Left = 140
      Top = 62
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '1'
      OnChange = YEntryChange
    end
    object XEntry: TEdit
      Left = 140
      Top = 27
      Width = 121
      Height = 21
      TabOrder = 2
      Text = '1'
      OnChange = XEntryChange
    end
    object PanelDimEntry: TEdit
      Left = 140
      Top = 97
      Width = 121
      Height = 21
      TabOrder = 0
    end
  end
  object PCBEntry: TEdit
    Left = 17
    Top = 42
    Width = 239
    Height = 21
    TabOrder = 0
  end
  object OKButton: TButton
    Left = 200
    Top = 383
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = OKButtonClick
  end
  object SearchButton: TButton
    Left = 264
    Top = 37
    Width = 75
    Height = 25
    Caption = 'Search'
    TabOrder = 2
    OnClick = SearchButtonClick
  end
  object GroupBox1: TGroupBox
    Left = 20
    Top = 73
    Width = 316
    Height = 71
    Caption = 'PCB Dimensions'
    TabOrder = 3
    object Label4: TLabel
      Left = 22
      Top = 29
      Width = 18
      Height = 21
      Caption = 'X:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 174
      Top = 29
      Width = 17
      Height = 21
      Caption = 'Y:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label6: TLabel
      Left = 110
      Top = 29
      Width = 32
      Height = 21
      Caption = 'mm'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label7: TLabel
      Left = 262
      Top = 29
      Width = 32
      Height = 21
      Caption = 'mm'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object xPCBDimEntry: TEdit
    Left = 65
    Top = 102
    Width = 61
    Height = 24
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 4
  end
  object yPCBDimEntry: TEdit
    Left = 215
    Top = 102
    Width = 61
    Height = 24
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
end
