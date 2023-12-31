function SplitAndGetLast(const InputString: string; const Delimiter: Char): string; forward;
function MilsToMM(Mils: Real): Real;                    forward;
function CreatePad(xPos : TCoord, yPos : TCoord, HoleSize : TCoord) : IPCB_Pad;     forward;
function CreateFiducial(Layer : TLayer, xPos : TCoord, yPos : TCoord) : IPCB_Pad;   forward;
procedure calculatePanelDimensions;                     forward;
procedure CreatePCBDocument;                            forward;
procedure BasicViewLayers;                              forward;
procedure BoardLayerOnly;                               forward;
procedure InitializeImages;                             forward;
procedure PlacePolygonCutouts(PanelPCB : IPCB_Board);   forward;
procedure DeleteAllObjectsOnPCB(PanelPCB : IPCB_Board); forward;
procedure CopyBoardOutline(PCB_Board: IPCB_Board, AWidth : Coord; ALayer : TLayer); forward;
procedure PlaceBoardTitle(PanelPCB : IPCB_Board);       forward;
procedure PlacePanelFiducials(PanelPCB : IPCB_Board);   forward;
procedure PlacePanelPads (PanelPCB : IPCB_Board);       forward;
procedure TForm1.SearchButtonClick(Sender: TObject);    forward;
procedure TForm1.OKButtonClick(Sender: TObject);        forward;
procedure TForm1.XEntryChange(Sender: TObject);         forward;
procedure TForm1.YEntryChange(Sender: TObject);         forward;

const
     eBoardTrackWidth = 10; // 10mils or 0,254mm
     eBoardLayer = 21;
     eTopLayer = 1;
     cTextHeight   = 5;
     cTextWidth    = 0.5;
     cLineWidth    = 1;
     eTextOffset = 3;
     imageWidth = 664;
     eOffsetHalfTitleLenght = 16;

var
    Workspace: IWorkspace;
    PanelPCB_Board : IPCB_Board;
    PCBBoard: IPCB_Board;
    CurrentProjectDir, FileName: WideString;
    OpenDlg: TOpenDialog;

    BoardBounds: TCoordRect;
    // PCB Width and Height
    WidthMM, HeightMM: Real;
    // Panel Measurements.
    xPanelOrigin, yPanelOrigin, WidthPanel, HeightPanel, ColSpace, RowSpace, HeightEmbeddedObject, WidthEmbeddedObject : Real;
    XCount, YCount: Integer;

    PCBTitle, PanelPCBDoc : string;
    PanelPcbDocFilePath : WideString;

    // Test Image
    ImageList: TStringList;
    CurrentImageIndex: Integer;

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

function MmToMils(Millimeters: Real): Real;
begin
  // Conversion factor from millimeters to mils
  Result := Millimeters * 39.3701 *10000;
end;

function GetPCBWidthAndHeight(PcbBoundaries : TCoordRect);
begin
   // Convert the dimensions to millimeters
   WidthMM := MilsToMM(BoardBounds.Right - BoardBounds.Left);
   HeightMM := MilsToMM(BoardBounds.Top - BoardBounds.Bottom);
end;

function GetEmbeddedBoards(ABoard : IPCB_Board) : TObjectList;
Var
    EmbedObj   : IPCB_EmbeddedBoard;
    BIterator  : IPCB_BoardIterator;
    BLayerSet  : IPCB_LayerSet;
    Primitive  : IPCB_Primitive;

begin

    Result := TObjectList.Create;
    Result.OwnsObjects := false;      // critical!

    BLayerSet := LayerSetUtils.CreateLayerSet.IncludeAllLayers;
    BIterator := ABoard.BoardIterator_Create;
    BIterator.AddFilter_ObjectSet(MkSet(eEmbeddedBoardObject));
    BIterator.AddFilter_IPCB_LayerSet(BLayerSet);
    BIterator.AddFilter_Method(eProcessAll);


    EmbedObj := BIterator.FirstPCBObject;
    while (EmbedObj <> Nil) do
    begin
        Result.Add(EmbedObj);
        EmbedObj := BIterator.NextPCBObject;
    end;

    ABoard.BoardIterator_Destroy(BIterator);
end;

function AddEmbeddedBoardObj(PCB_Board : IPCB_Board) : IPCB_Embedded;
begin
    Result := PCBServer.PCBObjectFactory(eEmbeddedBoardObject, eNoDimension, eCreate_Default);
    Result.DocumentPath := FileName;

    Result.RowCount := YCount;
    Result.ColCount := XCount;

    Result.ColSpacing := MmToMils( WidthMM + ColSpace);
    Result.RowSpacing := MmToMils( HeightMM + RowSpace);
    Result.XLocation  := MmToMils(xPanelOrigin);
    Result.YLocation  := MmToMils(yPanelOrigin);

    PCB_Board.AddPCBObject(Result);
end;

function CreatePad(xPos : TCoord, yPos : TCoord, HoleSize : TCoord) : IPCB_Pad;
begin
     Result := PCBServer.PCBObjectFactory(ePadObject, eNoDimension, eCreate_Default);
     Result.X := xPos;
     Result.Y := yPos;
     Result.SetState_HoleSize(MMsToCoord(3));
end;

function CreateFiducial(Layer : TLayer, xPos : TCoord, yPos : TCoord) : IPCB_Pad;
begin
     Result := PCBServer.PCBObjectFactory(ePadObject, eNoDimension, eCreate_Default);
     Result.Layer := Layer;
     Result.X := xPos;
     Result.Y := yPos;
     Result.TopXSize := MmToMils(2);
     Result.TopYSize := MmToMils(2);
end;

function CreateTrack(StartPoint : TPoint, EndPoint : TPoint, Width: TCoord, Layer : TLayer) : IPCB_Track;
begin
     Result := PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);
     Result.Layer := Layer;
     Result.x1 := StartPoint.X;
     Result.y1 := StartPoint.Y;
     Result.x2 := EndPoint.X;
     Result.y2 := EndPoint.Y;
     Result.Width := Width;
end;

procedure SetCurrentLayer(Board : IPCB_Board, Layer : TLayer);
begin
     Board.CurrentLayer := Layer;
end;

procedure AddEmbeddedBoard(PanelPCB: IPCB_Board);
var
    EmbeddedBoardList : TObjectList;

begin
    EmbeddedBoardList := GetEmbeddedBoards(PCBBoard);

    AddEmbeddedBoardObj(PanelPCB);
end;

procedure ResizePCBBoard(PanelPCB: IPCB_Board);
Var
    Track : IPCB_Track;
    xDimension : IPCB_Dimension;
    ALayer : TLayer;
    startPoint, endPoint : TPoint;

Begin
    PCBServer.PreProcess;
    ALayer := LayerUtils.MechanicalLayer(eBoardLayer);

    Track := CreateTrack( Point(0, 0), Point(MmToMils(WidthPanel), 0), MilsToCoord(eBoardTrackWidth), ALayer);
    PanelPCB.AddPCBObject(Track);
    Track.Selected := true;

    Track := CreateTrack( Point(MmToMils(WidthPanel), 0), Point(MmToMils(WidthPanel), MmToMils(HeightPanel)), MilsToCoord(eBoardTrackWidth), ALayer);
    PanelPCB.AddPCBObject(Track);
    Track.Selected := true;

    Track := CreateTrack( Point(MmToMils(WidthPanel), MmToMils(HeightPanel)), Point(0, MmToMils(HeightPanel)), MilsToCoord(eBoardTrackWidth), ALayer);
    PanelPCB.AddPCBObject(Track);
    Track.Selected := true;

    Track := CreateTrack( Point(0, MmToMils(HeightPanel)), Point(0, 0), MilsToCoord(eBoardTrackWidth), ALayer);
    PanelPCB.AddPCBObject(Track);
    Track.Selected := true;

    // Display (unconditionally) the layer selected by the user.
    PanelPCB.LayerIsDisplayed[ALayer] := True;

    // Refresh PCB workspace.
    ResetParameters();
    AddStringParameter('Mode', 'BOARDOUTLINE_FROM_SEL_PRIMS');
    RunProcess('PCB:PlaceBoardOutline');

    BasicViewLayers;
    SetCurrentLayer(PanelPCB, ALayer);

    PCBServer.PostProcess;
End;

procedure TForm1.SearchButtonClick(Sender: TObject);
begin
    // Create an Open Dialog
    OpenDlg := TOpenDialog.Create(nil);
    Workspace := GetWorkSpace;
    CurrentProjectDir := AnsiReplaceText(Workspace.DM_FocusedProject.DM_ProjectFullPath, Workspace.DM_FocusedProject.DM_ProjectFileName , '');
    OpenDlg.InitialDir := CurrentProjectDir;
    OpenDlg.Title := 'Open PCB Document';
    OpenDlg.Filter := 'PCB Files (*.PcbDoc)|*.PcbDoc';

    if OpenDlg.Execute then
    begin
        FileName := OpenDlg.FileName;

        // Check if the file exists
        if not FileExists(FileName) then
        begin
          ShowError('File not found: ' + FileName);
          Exit;
        end;

        Workspace.DM_OpenProject(FileName, False);
        PCBBoard := PCBServer.GetPCBBoardByPath(FileName);

        if PCBServer = nil then
        begin
             ShowError('PCBServer Failed.');
             Exit;
        end;
        // Checks that PCBBOard exists
        if PCBBoard = nil then
        begin
             ShowError('Failed to open PCB document: ' + FileName);
             Exit;
        end;

        PCBEntry.Text := SplitAndGetLast(FileName, '\');
        BoardBounds := PCBBoard.BoardOutline.BoundingRectangle;

        GetPCBWidthAndHeight( BoardBounds );

        xPCBDimEntry.Text :=  FloatToStr(WidthMM);
        yPCBDimEntry.Text :=  FloatToStr(HeightMM);

        // Save the name of the PCB Selected and place the title
        PCBTitle := AnsiReplaceText(SplitAndGetLast(FileName, '\'), '.PcbDoc', '');
        titleLabel.Caption := PCBTitle;
        titleLabel.Left := (imageWidth/2) - (titleLabel.Canvas.TextWidth(titleLabel.Caption)/2);

        calculatePanelDimensions;
        OKButton.Enabled := True;
    end;
end;

procedure TForm1.OKButtonClick(Sender: TObject);
begin
     //We open the PCB document where we are creating the panel.
     Workspace.DM_OpenProject(PanelPcbDocFilePath, True);
     DeleteAllObjectsOnPCB(PanelPCB_Board);
     if CopyBoardButton.Checked then CopyBoardOutline(PCBBoard, MmToMils(0.254), LayerUtils.MechanicalLayer(eBoardLayer));
     ResizePCBBoard(PanelPCB_Board);

     AddEmbeddedBoard(PanelPCB_Board);
     PlacePanelPads(PanelPCB_Board);
     PlacePanelFiducials(PanelPCB_Board);
     //PlacePolygonCutouts(PanelPCB_Board);
     if not TitleEnableButton.Checked then PlaceBoardTitle(PanelPCB_Board);

      Close;
end;

procedure calculatePanelDimensions;
begin
if (WidthMM <> 0.0) and (HeightMM <> 0.0) then
begin
   xPanelOrigin := StrToFloat(xPanelOriginEntry.Text);
   yPanelOrigin := StrToFloat(yPanelOriginEntry.Text);

   if TryStrToInt(XEntry.Text, XCount) and TryStrToInt(YEntry.Text, YCount) then
   begin
      XCount := StrToInt(XEntry.Text);
      YCount := StrToInt(YEntry.Text);

      // Arbitrary hardcoded: will modify with corresponding entry in gui
      RowSpace := StrToFloat(RowSpaceEntry.Text);
      ColSpace := StrToFloat(ColumnSpaceEntry.Text);

      WidthEmbeddedObject :=  ColSpace*(XCount-1) + WidthMM * XCount;
      HeightEmbeddedObject := RowSpace*(YCount-1) + HeightMM * YCount;

      WidthPanel  := (xPanelOrigin *2) + WidthEmbeddedObject;
      HeightPanel := (yPanelOrigin *2) + HeightEmbeddedObject;

      PanelDimEntry.Text := FloatToStr(WidthPanel) + 'x' + FloatToStr(HeightPanel);
      PanelHeightEntry.Text := (HeightPanel);
      PanelWidthEntry.Text := (WidthPanel);

   end

   else
   begin
       PanelDimEntry.Text := 'error';
   end;
end;
end;

procedure TForm1.CancelButtonClick(Sender: TObject);
begin
     Close;
end;

procedure CreatePCBDocument;
// From the file CreatePCBObjects.pas
var
   WorkSpace : IWorkSpace;
   ret : string;

begin
     if PCBBoard = nil then
     begin
        ShowError('Please select a PCB to panelize.');
        Exit;
     end;

    WorkSpace := GetWorkSpace;
    If WorkSpace = Nil Then Exit;

    ret := Workspace.DM_CreateNewDocument('PCB');
    ShowMessage(ret);
    // Initialize PCB server.
    PCBServer.PreProcess;

    PCBServer.PostProcess;
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

end;

procedure TForm1.Form1Create(Sender: TObject);
begin
     //InitializeImages;
     PanelPCB_Board := PCBServer.GetCurrentPCBBoard;
     PanelPcbDocFilePath := PanelPCB_Board.FileName;
     PanelPCBDoc := SplitAndGetLast(PanelPcbDocFilePath, '\');
     PanelPCBDocEntry.Text := PanelPCBDoc;

end;

Procedure BasicViewLayers;
Begin
    ResetParameters;
    AddStringParameter ('TopSignal'         , 'True'  );
    AddStringParameter ('Mid1'              , 'False' );
    AddStringParameter ('Mid2'              , 'False' );
    AddStringParameter ('Mid3'              , 'False' );
    AddStringParameter ('Mid4'              , 'False' );
    AddStringParameter ('Mid5'              , 'False' );
    AddStringParameter ('Mid6'              , 'False' );
    AddStringParameter ('Mid7'              , 'False' );
    AddStringParameter ('Mid8'              , 'False' );
    AddStringParameter ('Mid9'              , 'False' );
    AddStringParameter ('Mid10'             , 'False' );
    AddStringParameter ('Mid11'             , 'False' );
    AddStringParameter ('Mid12'             , 'False' );
    AddStringParameter ('Mid13'             , 'False' );
    AddStringParameter ('Mid14'             , 'False' );
    AddStringParameter ('Mid15'             , 'False' );
    AddStringParameter ('Mid16'             , 'False' );
    AddStringParameter ('Mid17'             , 'False' );
    AddStringParameter ('Mid18'             , 'False' );
    AddStringParameter ('Mid19'             , 'False' );
    AddStringParameter ('Mid20'             , 'False' );
    AddStringParameter ('Mid21'             , 'False' );
    AddStringParameter ('Mid22'             , 'False' );
    AddStringParameter ('Mid23'             , 'False' );
    AddStringParameter ('Mid24'             , 'False' );
    AddStringParameter ('Mid25'             , 'False' );
    AddStringParameter ('Mid26'             , 'False' );
    AddStringParameter ('Mid27'             , 'False' );
    AddStringParameter ('Mid28'             , 'False' );
    AddStringParameter ('Mid29'             , 'False' );
    AddStringParameter ('Mid30'             , 'False' );
    AddStringParameter ('BottomSignal'      , 'True' );
    AddStringParameter ('TopOverlay'        , 'True'  );
    AddStringParameter ('BottomOverlay'     , 'True' );
    AddStringParameter ('TopPaste'          , 'True'  );
    AddStringParameter ('BottomPaste'       , 'True' );
    AddStringParameter ('TopSolder'         , 'True'  );
    AddStringParameter ('BottomSolder'      , 'True' );
    AddStringParameter ('Plane1'            , 'False' );
    AddStringParameter ('Plane2'            , 'False' );
    AddStringParameter ('Plane3'            , 'False' );
    AddStringParameter ('Plane4'            , 'False' );
    AddStringParameter ('Plane5'            , 'False' );
    AddStringParameter ('Plane6'            , 'False' );
    AddStringParameter ('Plane7'            , 'False' );
    AddStringParameter ('Plane8'            , 'False' );
    AddStringParameter ('Plane9'            , 'False' );
    AddStringParameter ('Plane10'           , 'False' );
    AddStringParameter ('Plane11'           , 'False' );
    AddStringParameter ('Plane12'           , 'False' );
    AddStringParameter ('Plane13'           , 'False' );
    AddStringParameter ('Plane14'           , 'False' );
    AddStringParameter ('Plane15'           , 'False' );
    AddStringParameter ('Plane16'           , 'False' );
    AddStringParameter ('DrillGuide'        , 'True' );
    AddStringParameter ('KeepOut'           , 'True' );
    AddStringParameter ('Mechanical1'       , 'False' );
    AddStringParameter ('Mechanical2'       , 'False' );
    AddStringParameter ('Mechanical3'       , 'False' );
    AddStringParameter ('Mechanical4'       , 'False' );
    AddStringParameter ('Mechanical5'       , 'False' );
    AddStringParameter ('Mechanical6'       , 'False' );
    AddStringParameter ('Mechanical7'       , 'False' );
    AddStringParameter ('Mechanical8'       , 'False' );
    AddStringParameter ('Mechanical9'       , 'False' );
    AddStringParameter ('Mechanical10'      , 'False' );
    AddStringParameter ('Mechanical11'      , 'False' );
    AddStringParameter ('Mechanical12'      , 'False' );
    AddStringParameter ('Mechanical13'      , 'False' );
    AddStringParameter ('Mechanical14'      , 'False' );
    AddStringParameter ('Mechanical15'      , 'False' );
    AddStringParameter ('Mechanical16'      , 'False' );
    AddStringParameter ('Mechanical21'      , 'True' );
    AddStringParameter ('DrillDrawing'      , 'True' );
    AddStringParameter ('MultiLayer'        , 'True'  );

    RunProcess ('Pcb:DocumentPreferences');
end;

Procedure BoardLayerOnly;
Begin
    ResetParameters;
    AddStringParameter ('TopSignal'         , 'False'  );
    AddStringParameter ('Mid1'              , 'False' );
    AddStringParameter ('Mid2'              , 'False' );
    AddStringParameter ('Mid3'              , 'False' );
    AddStringParameter ('Mid4'              , 'False' );
    AddStringParameter ('Mid5'              , 'False' );
    AddStringParameter ('Mid6'              , 'False' );
    AddStringParameter ('Mid7'              , 'False' );
    AddStringParameter ('Mid8'              , 'False' );
    AddStringParameter ('Mid9'              , 'False' );
    AddStringParameter ('Mid10'             , 'False' );
    AddStringParameter ('Mid11'             , 'False' );
    AddStringParameter ('Mid12'             , 'False' );
    AddStringParameter ('Mid13'             , 'False' );
    AddStringParameter ('Mid14'             , 'False' );
    AddStringParameter ('Mid15'             , 'False' );
    AddStringParameter ('Mid16'             , 'False' );
    AddStringParameter ('Mid17'             , 'False' );
    AddStringParameter ('Mid18'             , 'False' );
    AddStringParameter ('Mid19'             , 'False' );
    AddStringParameter ('Mid20'             , 'False' );
    AddStringParameter ('Mid21'             , 'False' );
    AddStringParameter ('Mid22'             , 'False' );
    AddStringParameter ('Mid23'             , 'False' );
    AddStringParameter ('Mid24'             , 'False' );
    AddStringParameter ('Mid25'             , 'False' );
    AddStringParameter ('Mid26'             , 'False' );
    AddStringParameter ('Mid27'             , 'False' );
    AddStringParameter ('Mid28'             , 'False' );
    AddStringParameter ('Mid29'             , 'False' );
    AddStringParameter ('Mid30'             , 'False' );
    AddStringParameter ('BottomSignal'      , 'False' );
    AddStringParameter ('TopOverlay'        , 'False'  );
    AddStringParameter ('BottomOverlay'     , 'False' );
    AddStringParameter ('TopPaste'          , 'False'  );
    AddStringParameter ('BottomPaste'       , 'False' );
    AddStringParameter ('TopSolder'         , 'False'  );
    AddStringParameter ('BottomSolder'      , 'False' );
    AddStringParameter ('Plane1'            , 'False' );
    AddStringParameter ('Plane2'            , 'False' );
    AddStringParameter ('Plane3'            , 'False' );
    AddStringParameter ('Plane4'            , 'False' );
    AddStringParameter ('Plane5'            , 'False' );
    AddStringParameter ('Plane6'            , 'False' );
    AddStringParameter ('Plane7'            , 'False' );
    AddStringParameter ('Plane8'            , 'False' );
    AddStringParameter ('Plane9'            , 'False' );
    AddStringParameter ('Plane10'           , 'False' );
    AddStringParameter ('Plane11'           , 'False' );
    AddStringParameter ('Plane12'           , 'False' );
    AddStringParameter ('Plane13'           , 'False' );
    AddStringParameter ('Plane14'           , 'False' );
    AddStringParameter ('Plane15'           , 'False' );
    AddStringParameter ('Plane16'           , 'False' );
    AddStringParameter ('DrillGuide'        , 'False' );
    AddStringParameter ('KeepOut'           , 'False' );
    AddStringParameter ('Mechanical1'       , 'False' );
    AddStringParameter ('Mechanical2'       , 'False' );
    AddStringParameter ('Mechanical3'       , 'False' );
    AddStringParameter ('Mechanical4'       , 'False' );
    AddStringParameter ('Mechanical5'       , 'False' );
    AddStringParameter ('Mechanical6'       , 'False' );
    AddStringParameter ('Mechanical7'       , 'False' );
    AddStringParameter ('Mechanical8'       , 'False' );
    AddStringParameter ('Mechanical9'       , 'False' );
    AddStringParameter ('Mechanical10'      , 'False' );
    AddStringParameter ('Mechanical11'      , 'False' );
    AddStringParameter ('Mechanical12'      , 'False' );
    AddStringParameter ('Mechanical13'      , 'False' );
    AddStringParameter ('Mechanical14'      , 'False' );
    AddStringParameter ('Mechanical15'      , 'False' );
    AddStringParameter ('Mechanical16'      , 'False' );
    AddStringParameter ('Mechanical21'      , 'True' );
    AddStringParameter ('DrillDrawing'      , 'False' );
    AddStringParameter ('MultiLayer'        , 'False'  );

    RunProcess ('Pcb:DocumentPreferences');

End;

procedure PlacePanelPads (PanelPCB : IPCB_Board);
var
  Pad: IPCB_Pad;

begin
  Pad := CreatePad(MmToMils(5), MmToMils(5), MMsToCoord(3));
  PanelPCB.AddPCBObject(Pad);

  Pad := CreatePad(MmToMils(WidthPanel - 5), MmToMils(5), MMsToCoord(3));
  PanelPCB.AddPCBObject(Pad);

  Pad := CreatePad(MmToMils(WidthPanel - 5), MmToMils(HeightPanel - 5), MMsToCoord(3));
  PanelPCB.AddPCBObject(Pad);

  Pad := CreatePad(MmToMils(5), MmToMils(HeightPanel - 5), MMsToCoord(3));
  PanelPCB.AddPCBObject(Pad);

end;

procedure PlacePanelFiducials (PanelPCB : IPCB_Board);
var
   Fid : IPCB_Pad;
begin
   Fid := CreateFiducial(eTopLayer, MmToMils(5+10), MmToMils(5));
   PanelPCB.AddPCBObject(Fid);

   Fid := CreateFiducial(eBottomLayer, MmToMils(5+10), MmToMils(5));
   PanelPCB.AddPCBObject(Fid);

   Fid := CreateFiducial(eTopLayer, MmToMils(WidthPanel - 5 - 10), MmToMils(5));
   PanelPCB.AddPCBObject(Fid);

   Fid := CreateFiducial(eBottomLayer, MmToMils(WidthPanel - 5 - 10), MmToMils(5));
   PanelPCB.AddPCBObject(Fid);

   Fid := CreateFiducial(eTopLayer, MmToMils(WidthPanel - 5 - 20), MmToMils(HeightPanel - 5));
   PanelPCB.AddPCBObject(Fid);

   Fid := CreateFiducial(eBottomLayer, MmToMils(WidthPanel - 5 - 20), MmToMils(HeightPanel - 5));
   PanelPCB.AddPCBObject(Fid);
end;

procedure PlacePolygonCutouts (PanelPCB : IPCB_Board);
var
   PolyCut1    : IPCB_Region;
   polygonSegment1, polygonSegment2, polygonSegment3, polygonSegment4  : TPolySegment;
   contour : IPCB_Contour;

begin
   PCBServer.PreProcess;

   contour :=  IPCB_Contour;
   contour.AddPoint(23, 20);
   contour.AddPoint(23, 27);
   contour.AddPoint(30, 27);
   contour.AddPoint(30, 20);

   region := PCBServer.PCBObjectFactory(eRegionObject, eNoDimension, eCreate_Default);
   region.SetOutlineContour(contour);
   region.Layer := eTopLayer;

   Board.AddPCBObject(region);

   PCBServer.PostProcess;

   ShowInfo(PolyCut1.GetState_AreaSize);
end;

procedure PlaceBoardTitle(PanelPCB : IPCB_Board);
var
   Title : IPCB_Text;
   Location : TLocation;
   xTextPost, yTextPost : Real;

begin
    PCBServer.PreProcess;
    yTextPost := yPanelOrigin + HeightEmbeddedObject + eTextOffset;
    // TODO:
    // Change this eOffsetHalfTitleLenght constant and make it 'dinamic' according to actual text.
    xTextPost := (WidthPanel/2) - eOffsetHalfTitleLenght;
    Location := Point(MmToMils(xTextPost), MmToMils(yTextPost));

    Title := PCBServer.PCBObjectFactory(eTextObject, eNoDimension, eCreate_Default);

    Title.XLocation  := Location.X;
    Title.YLocation  := Location.Y;
    Title.Layer      := eTopOverlay;
    Title.UseTTFonts := true;
    Title.Text       := PCBTitle;
    Title.Size       := MmToMils(HeightPanel - yTextPost);

    PanelPCB.AddPCBObject(Title);
    PCBServer.PostProcess;

end;

// Taken and modified from CopyBoardOutlineForm script: https://www.altium.com/documentation/altium-designer/script-example-analysis
// It doesn't seem to work properly when the board outline has a complex form.
procedure CopyBoardOutline(PCB_Board: IPCB_Board, AWidth : Coord; ALayer : TLayer);
Var
    Track     : IPCB_Track;
    Arc       : IPCB_Arc;
    I,J       : Integer;
Begin
    PCB_Board.BoardOutline.Invalidate;
    PCB_Board.BoardOutline.Rebuild;
    PCB_Board.BoardOutline.Validate;

    PCBServer.PreProcess;
    // Step through each of the vertices of the Board Outline in turn.
    For I := 0 To PCB_Board.BoardOutline.PointCount - 1 Do
    Begin
        // Set the value of J to point to the "next" vertex; this is normally
        // I + 1, but needs to be set to 0 instead for the very last vertex
        // that is processed by this loop.
        If I = PCB_Board.BoardOutline.PointCount - 1 Then
            J := 0
        Else
            J := I + 1;

        If PCB_Board.BoardOutline.Segments[I].Kind = ePolySegmentLine Then
        Begin
            // Current segment is a straight line; create a Track object.
            Track          := PCBServer.PCBObjectFactory(eTrackObject,
                                                         eNoDimension,
                                                         eCreate_Default);

            Track.X1       := PCB_Board.BoardOutline.Segments[I].vx;
            Track.Y1       := PCB_Board.BoardOutline.Segments[I].vy;
            Track.X2       := PCB_Board.BoardOutline.Segments[J].vx;
            Track.Y2       := PCB_Board.BoardOutline.Segments[J].vy;
            Track.Layer    := ALayer;
            Track.Width    := AWidth;
            PCB_Board.AddPCBObject(Track);
        End
        Else
        Begin
            // Current segment is an arc; create an Arc object.
            Arc := PCBServer.PCBObjectFactory(eArcObject,
                                              eNoDimension,
                                              eCreate_Default);

            Arc.XCenter    := PCB_Board.BoardOutline.Segments[I].cx;
            Arc.YCenter    := PCB_Board.BoardOutline.Segments[I].cy;
            Arc.Layer      := ALayer;
            Arc.LineWidth  := AWidth;
            Arc.Radius     := PCB_Board.BoardOutline.Segments[I].Radius;
            Arc.StartAngle := PCB_Board.BoardOutline.Segments[I].Angle1;
            Arc.EndAngle   := PCB_Board.BoardOutline.Segments[I].Angle2;
            PCB_Board.AddPCBObject(Arc);
        End;
    End;
    PCBServer.PostProcess;

    // Display (unconditionally) the layer selected by the user.
    PCB_Board.LayerIsDisplayed[ALayer] := True;

    // Refresh PCB workspace.
    ResetParameters;
    AddStringParameter('Action', 'Redraw');
    RunProcess('PCB:Zoom');
End;

procedure TForm1.TitleEnableButtonClick(Sender: TObject);
begin
     if TitleEnableButton.Checked then
        begin
          titleLabel.Visible := false;
        end
     else
         begin
          titleLabel.Visible := true;
         end;
end;

procedure DeleteAllObjectsOnPCB(PanelPCB : IPCB_Board);
var
  PCBBoard: IPCB_Board;
  Iterator: IPCB_BoardIterator;
  CurrentObject: IPCB_Object;
  BLayerSet  : IPCB_LayerSet;

begin
  PCBServer.PreProcess;
  BLayerSet := LayerSetUtils.CreateLayerSet.IncludeAllLayers;

  // Create an iterator to traverse all objects on the PCB
  Iterator := PanelPCB.BoardIterator_Create;
  Iterator.AddFilter_IPCB_LayerSet(BLayerSet);
  Iterator.AddFilter_Method(eProcessAll);
  
  try
    // Iterate through all objects and delete them
    CurrentObject := Iterator.FirstPCBObject;
    while CurrentObject <> nil do
    begin
      PanelPCB.RemovePCBObject(CurrentObject);
      CurrentObject := Iterator.NextPCBObject;
    end;
  finally
    PanelPCB.BoardIterator_Destroy(Iterator);
  end;

  // Refresh the PCB editor to reflect the changes
  PCBServer.PostProcess;
end;

procedure TForm1.CommonChangeCallback(Sender: TObject);
var
   FloatValue : Real;
begin
     if TryStrToFloat(Sender.Text, FloatValue) then
        begin
          calculatePanelDimensions;
          Sender.Color := clWhite;
          OKButton.Enabled := True;
        end
     else
      begin
        Sender.Color := clRed;
        OKButton.Enabled := False;
      end;
end;




