procedure calculatePanelDimensions; forward;
procedure CreateAndAddPCBDocument; forward;

var
    XCount, YCount: Integer;
    FileName: string;
    BoardBounds: TCoordRect;
    WidthMM, HeightMM: Real;
    OpenDlg: TOpenDialog;
    xPanelOrigin : Real;
    yPanelOrigin : Real;

    PCBDocument: IPCB_BoardDocument;
    CurrentProject: IProject;

function SplitAndGetLast(const InputString: string; const Delimiter: Char): string;
    var
      StringList: TStringList;
    begin
      StringList := TStringList.Create;
      try
        // Split the input string into multiple strings using the specified delimiter
        StringList.Delimiter := Delimiter;
        StringList.StrictDelimiter := True; // Treat consecutive delimiters as one
        StringList.DelimitedText := InputString;

        // Check if there are any elements in the list
        if StringList.Count > 0 then
        begin
          // Return the last element from the list
          Result := StringList[StringList.Count - 1];
        end
        else
        begin
          // If the list is empty, return an empty string
          Result := '';
        end;
      finally
        StringList.Free;
      end;
    end;

function MilsToMM(Mils: Real): Real;
    begin
        // Conversion factor from mils to millimeters
        Result := FormatFloat('0.0', Mils * 0.0254 / 10000);
    end;



procedure TForm1.SearchButtonClick(Sender: TObject);
     var
        PCBBoard: IPCB_Board;

     begin
                  // Create an Open Dialog
       OpenDlg := TOpenDialog.Create(nil);
       OpenDlg.Title := 'Open PCB Document';
       OpenDlg.Filter := 'PCB Files (*.PcbDoc)|*.PcbDoc';

       if OpenDlg.Execute then
       begin
            FileName := OpenDlg.FileName;
            // Check if the file exists
            if not FileExists(FileName) then
            begin
              ShowMessage('File not found: ' + FileName);
              Exit;
            end;

            PCBEntry.Text := SplitAndGetLast(FileName, '\');
            PCBBoard := PCBServer.GetPCBBoardByPath(FileName);
            // Checks that PCBBOard exists
            if PCBBoard = nil then
               begin
                  ShowMessage('Failed to open PCB document.');
                  Exit;
               end;

            BoardBounds := PCBBoard.BoardOutline.BoundingRectangle;
            // Convert the dimensions to millimeters
            WidthMM := MilsToMM(BoardBounds.Right - BoardBounds.Left);
            HeightMM := MilsToMM(BoardBounds.Top - BoardBounds.Bottom);

            xPCBDimEntry.Text :=  FloatToStr(WidthMM);
            yPCBDimEntry.Text :=  FloatToStr(HeightMM);

            calculatePanelDimensions;
       end;
    end;

procedure TForm1.OKButtonClick(Sender: TObject);
    begin
         CreateAndAddPCBDocument;
    end;

procedure calculatePanelDimensions;
var
   xTotal, yTotal :  Real;

begin
if (WidthMM <> 0.0) and (HeightMM <> 0.0) then
begin
   xPanelOrigin := 11;
   yPanelOrigin := 11;

   if TryStrToInt(XEntry.Text, XCount) and TryStrToInt(YEntry.Text, YCount) then
   begin
      XCount := StrToInt(XEntry.Text);
      YCount := StrToInt(YEntry.Text);

      xTotal := xPanelOrigin + 2*(XCount-1) + WidthMM * XCount;
      yTotal := yPanelOrigin + 2*(YCount-1) + HeightMM * YCount;

      PanelDimEntry.Text := FloatToStr(xTotal) + 'x' + FloatToStr(yTotal);
   end

   else
   begin
       PanelDimEntry.Text := 'error';
   end;
end;
end;

procedure TForm1.XEntryChange(Sender: TObject);
begin
     calculatePanelDimensions;
end;


procedure TForm1.YEntryChange(Sender: TObject);
begin
     calculatePanelDimensions;
end;


procedure CreateAndAddPCBDocument;
var
  Board: IPCB_Board;
  Outline: IPCB_Outline;
  Point1, Point2: TCoordPoint;

begin
// If the document is not open, open it
  Board := PCBServer.GetCurrentPCBBoard;
  if Board = nil then
  begin
    ShowMessage('No PCB board is currently active.');
    Exit;
  end;

// Set the new board dimensions to 100x100 millimeters
  Outline := Board.BoardOutline;


end;


// https://blog.mbedded.ninja/electronics/general/altium/altium-scripting-and-using-the-api/pcb-related-api/
