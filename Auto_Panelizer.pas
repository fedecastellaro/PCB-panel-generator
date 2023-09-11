function SplitAndGetLast(const InputString: string; const Delimiter: Char): string; forward;
function MilsToMM(Mils: Real): Real; forward;
procedure calculatePanelDimensions;  forward;
procedure CreatePCBDocument;   forward;
procedure BasicViewLayers;   forward;
procedure BoardLayerOnly;   forward;
procedure PlacePolygonCutouts(PanelPCB : IPCB_Board);   forward; PlaceBoardTitle
procedure PlaceBoardTitle(PanelPCB : IPCB_Board);   forward;
procedure PlacePanelFiducials(PanelPCB : IPCB_Board);   forward;
procedure PlacePanelPads (PanelPCB : IPCB_Board);   forward;
procedure TForm1.SearchButtonClick(Sender: TObject); forward;
procedure TForm1.OKButtonClick(Sender: TObject);     forward;
procedure TForm1.XEntryChange(Sender: TObject);      forward;
procedure TForm1.YEntryChange(Sender: TObject);      forward;

const
     eBoardTrackWidth = 10; // 10mils or 0,254mm
     eBoardLayer = 21;
     eTopLayer = 1;
     cTextHeight   = 5;
     cTextWidth    = 0.5;
     cLineWidth    = 1;
     CoutoutWidth = 2;

var
    PCBBoard: IPCB_Board;
    FileName: WideString;
    OpenDlg: TOpenDialog;

    BoardBounds: TCoordRect;
    // PCB Width and Height
    WidthMM, HeightMM: Real;
    // Panel Measurements.
    xPanelOrigin, yPanelOrigin, WidthPanel, HeightPanel, ColSpace, RowSpace : Real;
    XCount, YCount: Integer;

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

{...............................................................................}

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

{..............................................................................}

function AddEmbeddedBoardObj(PCB_Board : IPCB_Board) : IPCB_Embedded;
var
   BoardBounds : TCoordRect;

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

{..............................................................................}

procedure AddEmbeddedBoard(PanelPCB: IPCB_Board);
var
    EmbeddedBoardList : TObjectList;

begin
    EmbeddedBoardList := GetEmbeddedBoards(PCBBoard);

    if EmbeddedBoardList.Count < 1 then
    begin
        AddEmbeddedBoardObj(PanelPCB);
    end

    else
    begin
        ShowWarning('document already has embedded boards !  ' + IntToStr(EmbeddedBoardList.Count) );
    end;
end;

procedure ResizePCBBoard(PanelPCB: IPCB_Board);
Var
    Track1, Track2, Track3, Track4 : IPCB_Track;
    Iterator: IPCB_BoardIterator;
    CurrentObject: IPCB_Track;
    ALayer : TLayer;

Begin
    PCBServer.PreProcess;

    ALayer := LayerUtils.MechanicalLayer(eBoardLayer);

    Track1 := PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);
    Track1.x1 := 0;
    Track1.y1 := 0;
    Track1.x2 := MmToMils(WidthPanel);
    Track1.y2 := 0;
    Track1.Layer := ALayer;
    Track1.Width := MilsToCoord(eBoardTrackWidth);
    PanelPCB.AddPCBObject(Track1);
    Track1.Selected := true;

    Track2 := PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);
    Track2.x1 := MmToMils(WidthPanel);
    Track2.y1 := 0;
    Track2.x2 := MmToMils(WidthPanel);
    Track2.y2 := MmToMils(HeightPanel);
    Track2.Layer := ALayer;
    Track2.Width := MilsToCoord(eBoardTrackWidth);
    PanelPCB.AddPCBObject(Track2);
    Track2.Selected := true;

    Track3 := PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);
    Track3.x1 := MmToMils(WidthPanel);
    Track3.y1 := MmToMils(HeightPanel);
    Track3.x2 := 0;
    Track3.y2 := MmToMils(HeightPanel);
    Track3.Layer := ALayer;
    Track3.Width := MilsToCoord(eBoardTrackWidth);
    PanelPCB.AddPCBObject(Track3);
    Track3.Selected := true;

    Track4 := PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);
    Track4.x1 := 0;
    Track4.y1 := MmToMils(HeightPanel);
    Track4.x2 := 0;
    Track4.y2 := 0;
    Track4.Layer := ALayer;
    Track4.Width := MilsToCoord(eBoardTrackWidth);
    PanelPCB.AddPCBObject(Track4);
    Track4.Selected := true;

    // Display (unconditionally) the layer selected by the user.
    PanelPCB.LayerIsDisplayed[ALayer] := True;

    // Refresh PCB workspace.
    ResetParameters();
    AddStringParameter('Mode', 'BOARDOUTLINE_FROM_SEL_PRIMS');
    RunProcess('PCB:PlaceBoardOutline');

    BasicViewLayers;

    PCBServer.PostProcess;
End;

procedure TForm1.SearchButtonClick(Sender: TObject);
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
          ShowError('File not found: ' + FileName);
          Exit;
        end;

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

        calculatePanelDimensions;
    end;
end;

procedure TForm1.OKButtonClick(Sender: TObject);
var
    PanelPCB_Board : IPCB_Board;
begin
         PanelPCB_Board := PCBServer.GetCurrentPCBBoard;
         ResizePCBBoard(PanelPCB_Board);
         AddEmbeddedBoard(PanelPCB_Board);
         PlacePanelPads(PanelPCB_Board);
         PlacePanelFiducials(PanelPCB_Board);
         //PlacePolygonCutouts(PanelPCB_Board);
         PlaceBoardTitle(PanelPCB_Board);
         Close;
end;

procedure calculatePanelDimensions;
begin
if (WidthMM <> 0.0) and (HeightMM <> 0.0) then
begin
   xPanelOrigin := 11;
   yPanelOrigin := 11;

   if TryStrToInt(XEntry.Text, XCount) and TryStrToInt(YEntry.Text, YCount) then
   begin
      XCount := StrToInt(XEntry.Text);
      YCount := StrToInt(YEntry.Text);

      // Arbitrary hardcoded: will modify with corresponding entry in gui
      RowSpace := 2;
      ColSpace := 2;

      WidthPanel  := (xPanelOrigin *2) + ColSpace*(XCount-1) + WidthMM * XCount;
      HeightPanel := (yPanelOrigin *2) + RowSpace*(YCount-1) + HeightMM * YCount;

      PanelDimEntry.Text := FloatToStr(WidthPanel) + 'x' + FloatToStr(HeightPanel);
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
     {PCBBoard := PCBServer.GetCurrentPCBBoard;
     if PCBBoard = Nil then Exit;

     FileName := PCBBoard.FileName;
     PCBEntry.Text := SplitAndGetLast(FileName, '\');
     BoardBounds := PCBBoard.BoardOutline.BoundingRectangle;

     GetPCBWidthAndHeight( BoardBounds );

     xPCBDimEntry.Text :=  FloatToStr(WidthMM);
     yPCBDimEntry.Text :=  FloatToStr(HeightMM);

     calculatePanelDimensions;}
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
  Pad1, Pad2, Pad3, Pad4: IPCB_Pad;

begin
  // This is also declared in

  Pad1 := PCBServer.PCBObjectFactory(ePadObject, eNoDimension, eCreate_Default);
  Pad1.X := MmToMils(5);
  Pad1.Y := MmToMils(5);
  Pad1.SetState_HoleSize(MMsToCoord(3));

  Pad2 := PCBServer.PCBObjectFactory(ePadObject, eNoDimension, eCreate_Default);
  Pad2.X := MmToMils(WidthPanel -5);
  Pad2.Y := MmToMils(5);
  Pad2.SetState_HoleSize(MMsToCoord(3));

  Pad3 := PCBServer.PCBObjectFactory(ePadObject, eNoDimension, eCreate_Default);
  Pad3.X := MmToMils(WidthPanel - 5);
  Pad3.Y := MmToMils(HeightPanel - 5);
  Pad3.SetState_HoleSize(MMsToCoord(3));

  Pad4 := PCBServer.PCBObjectFactory(ePadObject, eNoDimension, eCreate_Default);
  Pad4.X := MmToMils(5);
  Pad4.Y := MmToMils(HeightPanel - 5);
  Pad4.SetState_HoleSize(MMsToCoord(3));

  PanelPCB.AddPCBObject(Pad1);
  PanelPCB.AddPCBObject(Pad2);
  PanelPCB.AddPCBObject(Pad3);
  PanelPCB.AddPCBObject(Pad4);
end;

procedure PlacePanelFiducials (PanelPCB : IPCB_Board);
var
   Fid1, Fid2, Fid3: IPCB_Pad;
begin
   Fid1 := PCBServer.PCBObjectFactory(ePadObject, eNoDimension, eCreate_Default);
   Fid1.Layer := eTopLayer;
   Fid1.X := MmToMils(5+10);
   Fid1.Y := MmToMils(5);
   Fid1.TopXSize := MmToMils(2);
   Fid1.TopYSize := MmToMils(2);

   Fid2 := PCBServer.PCBObjectFactory(ePadObject, eNoDimension, eCreate_Default);
   Fid2.Layer := eTopLayer;
   Fid2.X := MmToMils(WidthPanel - 5 - 10);
   Fid2.Y := MmToMils(5);
   Fid2.TopXSize := MmToMils(2);
   Fid2.TopYSize := MmToMils(2);

   Fid3 := PCBServer.PCBObjectFactory(ePadObject, eNoDimension, eCreate_Default);
   Fid3.Layer := eTopLayer;
   Fid3.X := MmToMils(WidthPanel - 5 - 20);
   Fid3.Y := MmToMils(HeightPanel - 5);
   Fid3.TopXSize := MmToMils(2);
   Fid3.TopYSize := MmToMils(2);

   PanelPCB.AddPCBObject(Fid1);
   PanelPCB.AddPCBObject(Fid2);
   PanelPCB.AddPCBObject(Fid3);
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

   //PanelPCB.AddPCBObject(PolyCut1);

   //PolyCut1.Rebuild;

   PCBServer.PostProcess;

   {ResetParameters();
   AddStringParameter('Action', 'Redraw');
   RunProcess('PCB:Zoom');
   }
   ShowInfo(PolyCut1.GetState_AreaSize);
end;

procedure PlaceBoardTitle(PanelPCB : IPCB_Board);
var
   Title : IPCB_Text;
   Location : TLocation;
   xTextPost, yTextPost : Real;

begin
    PCBServer.PreProcess;
    yTextPost := HeightPanel - yPanelOrigin + CoutoutWidth;
    xTextPost := (WidthPanel - xPanelOrigin*2)/2;
    Location := Point(MmToMils(xTextPost), MmToMils(yTextPost));

    Title := PCBServer.PCBObjectFactory(eTextObject, eNoDimension, eCreate_Default);

    Title.XLocation  := Location.X;
    Title.YLocation  := Location.Y;
    Title.Layer      := eTopOverlay;
//    Result.IsHidden := false;
    Title.UseTTFonts := false;
    Title.UnderlyingString  := AnsiReplaceText(SplitAndGetLast(FileName, '\'), '.PcbDoc', '');
    Title.Size       := MmToMils((HeightPanel - yTextPost) * 0.85);
    Title.Width      := MmToMils(cTextWidth);
    //Title.UnionIndex := UIndex;

    PanelPCB.AddPCBObject(Title);
    //PCBServer.SendMessageToRobots(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Result.I_ObjectAddress);

    PCBServer.PostProcess;
end;
