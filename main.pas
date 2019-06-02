unit Main;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ShellCtrls,
  ExtCtrls, ComCtrls, LCLIntf;

{ TBackupForm }

type
  TBackupForm = class(TForm)
    AddSourceButton: TButton;
    AddDestButton: TButton;
    Bevel: TBevel;
    DestLabel: TLabel;
    DestBox: TListBox;
    ProgressBar: TProgressBar;
    RemoveSourceButton: TButton;
    SourceLabel: TLabel;
    SourceBox: TListBox;
    RemoveDestButton: TButton;
    CloseButton: TButton;
    BackupButton: TButton;
    SourceTree: TShellTreeView;
    DestTree: TShellTreeView;
    BackupTimer: TTimer;
    procedure AddButtonClick(Sender: TObject);
    procedure BackupButtonClick(Sender: TObject);
    procedure BackupTimerTimer(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure BoxDblClick(Sender: TObject);
  private
    FThread: TThread;
    FStart: TDateTime;
    procedure HandleResize(Sender: TObject);
    procedure AddItem(Tree: TShellTreeView; Box: TListBox);
    procedure RemoveItem(Box: TListBox);
  public

  end;

var
  BackupForm: TBackupForm;

implementation

{$R *.lfm}

{ TBackupForm }

var
  InternalConfig: string;

function Config: string;
begin
  if InternalConfig = '' then
    InternalConfig := IncludeTrailingPathDelimiter(GetAppConfigDir(False));
  Result := InternalConfig;
end;

procedure TBackupForm.FormShow(Sender: TObject);
begin
  Application.BringToFront;
  OnShow := nil;
end;

function LoadText(FileName: string): string;
begin
  if FileExists(FileName) then
    with TStringList.Create do
    try
      LoadFromFile(FileName);
      Result := Trim(Text);
    finally
      Free;
    end;
end;

procedure SaveText(FileName, Value: string);
begin
  with TStringList.Create do
  try
    Text := Value;
    SaveToFile(FileName);
  finally
    Free;
  end;
end;

procedure AppendText(FileName, Value: string);
var
  Exists: Boolean;
  F: TextFile;
begin
  Exists := FileExists(FileName);
  Assign(F, FileName);
  if Exists then
    Append(F)
  else
    Rewrite(F);
  WriteLn(F, Value);
  Close(F);
end;

procedure AppendLog(const Value: string);
var
  D: string;
begin
  D := FormatDateTime('YYYY-MM-DD h:nn:ss am/pm ', Now);
  AppendText(Config + 'events.log', D + Value);
end;

procedure TBackupForm.FormCreate(Sender: TObject);
begin
  ForceDirectories(Config);
  if FileExists(Config + 'source') then
    SourceBox.Items.LoadFromFile(Config + 'source');
  if FileExists(Config + 'dest') then
    DestBox.Items.LoadFromFile(Config + 'dest');
  BackupButton.Enabled := (SourceBox.Count > 0) and (DestBox.Count > 0);
  if FileExists(Config + 'last') then
    Caption := Caption + ' - last backup ' + LoadText(Config + 'last');
  {$ifdef darwin}
  SourceTree.Root := '/Users';
  DestTree.Root := '/Volumes';
  {$else}
  SourceTree.Root := '/home';
  DestTree.Root := '/media';
  {$endif}
  ClientWidth := Bevel.Width + 16;
  ClientHeight := CloseButton.Top + CloseButton.Height + 8;
  Constraints.MinWidth := Width - 100;
  Constraints.MinHeight := Height - 200;
  ProgressBar.Top := CloseButton.Top + (CloseButton.Height - ProgressBar.Height) div 2 + 1;
  SourceTree.Anchors := [akLeft, akTop, akBottom];
  AddSourceButton.Anchors := [akLeft, akBottom];
  SourceBox.Anchors := [akLeft, akBottom];
  RemoveSourceButton.Anchors := [akLeft, akBottom];
  DestTree.Anchors := [akLeft, akTop, akBottom];
  AddDestButton.Anchors := [akLeft, akBottom];
  DestBox.Anchors := [akLeft, akBottom];
  RemoveDestButton.Anchors := [akLeft, akBottom];
  Bevel.Anchors := [akLeft, akRight, akBottom];
  ProgressBar.Anchors := [akLeft, akRight, akBottom];
  BackupButton.Anchors := [akRight, akBottom];
  CloseButton.Anchors := [akRight, akBottom];
  OnResize := HandleResize;
  AppendLog('backer program started');
end;

procedure TBackupForm.HandleResize(Sender: TObject);
var
  W, L: Integer;
begin
  W := (ClientWidth - 24) div 2;
  SourceTree.Width := W;
  SourceBox.Width := W;
  L := W + 16;
  DestLabel.Left := L;
  DestTree.Left := L;
  DestTree.Width := W;
  DestBox.Left := L;
  DestBox.Width := W;
  AddDestButton.Left := L;
  RemoveDestButton.Left := L;
end;

procedure TBackupForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TBackupForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FThread <> nil then
    CloseAction := caNone
  else
    AppendLog('backer program exited');
end;

procedure TBackupForm.AddItem(Tree: TShellTreeView; Box: TListBox);
var
  Items: TStringList;
  N: TTreeNode;
  S: string;
begin
  N := Tree.Selected;
  if N = nil then
    Exit;
  S := N.Text;
  N := N.Parent;
  while N <> nil do
  begin
    S := IncludeTrailingPathDelimiter(N.Text) +  S;
    N := N.Parent;
  end;
  Items := TStringList.Create;
  try
    Items.Duplicates := dupIgnore;
    Items.Sorted := True;
    Items.Assign(Box.Items);
    if Items.IndexOf(S) > -1 then
    begin
      if Box = SourceBox then
        MessageDlg('Information', 'Folder "' + S + '" is already in the backup list.', mtInformation, [mbOk], 0)
      else
        MessageDlg('Information', 'Storage location "' + S + '" is already in the storage location list.', mtInformation, [mbOk], 0);
    end
    else
      Items.Add(S);
    Box.Items := Items;
  finally
    Items.Free;
  end;
  BackupButton.Enabled := (SourceBox.Count > 0) and (DestBox.Count > 0);
end;

procedure TBackupForm.RemoveItem(Box: TListBox);
var
  I: Integer;
begin
  I := Box.ItemIndex;
  if Box.ItemIndex < 0 then
    Exit;
  Box.Items.Delete(Box.ItemIndex);
  BackupButton.Enabled := (SourceBox.Count > 0) and (DestBox.Count > 0);
  if I = Box.Items.Count then
    Dec(I);
  if I > -1 then
    Box.ItemIndex := I;
end;

procedure TBackupForm.AddButtonClick(Sender: TObject);
begin
  if Sender = AddSourceButton then
    AddItem(SourceTree, SourceBox)
  else
    AddItem(DestTree, DestBox);
end;

procedure TBackupForm.RemoveButtonClick(Sender: TObject);
begin
  if Sender = RemoveSourceButton then
    RemoveItem(SourceBox)
  else
    RemoveItem(DestBox);
end;

procedure TBackupForm.BoxDblClick(Sender: TObject);
var
  Box: TListBox;
  I: Integer;
begin
  Box := Sender as TListBox;
  I := Box.ItemIndex;
  if Box.ItemIndex < 0 then
    Exit;
  OpenDocument(Box.Items[I]);
end;

{ TBackupThread }

type
  TBackupThread = class(TThread)
  private
    FSource: TStrings;
    FDest: TStrings;
  protected
    procedure Execute; override;
  public
    constructor Create(Source, Dest: TStrings);
    destructor Destroy; override;
  end;

constructor TBackupThread.Create(Source, Dest: TStrings);
begin
  FSource := TStringList.Create;
  FSource.Assign(Source);
  FDest := TStringList.Create;
  FDest.Assign(Dest);
  inherited Create(False);
end;

destructor TBackupThread.Destroy;
begin
  FSource.Free;
  FDest.Free;
  BackupForm.FThread := nil;
  inherited Destroy;
end;

procedure TBackupThread.Execute;

    procedure Backup(Source, Dest: string);
    begin
      CreateDir(Dest);
      Source := '"' + IncludeTrailingPathDelimiter(Source) + '"';
      Dest := '"' + IncludeTrailingPathDelimiter(Dest) + '"';
      AppendLog('executing: /usr/bin/rsync -a ' + Source + ' ' + Dest);
      ExecuteProcess('/usr/bin/rsync', '-a ' + Source + ' ' + Dest, []);
    end;

    procedure BackupFiles(Source, Dest: TStrings);
    var
      S, F, D: string;
      I, J: Integer;
    begin
      for I := 0 to Source.Count - 1 do
      begin
        S := Source[I];
        F := ExtractFileName(S);
        for J := 0 to Dest.Count - 1 do
        begin
          D := IncludeTrailingPathDelimiter(Dest[J]) + F;
          Backup(S, D);
        end;
      end;
    end;

var
  D: string;
begin
  FreeOnTerminate := True;
  try
    AppendLog('backup thread started');
    BackupFiles(FSource, FDest);
    AppendLog('backup thread completed');
  except
    on E: Exception do
    begin
      AppendLog('error in backup thread');
      D := FormatDateTime('YYYY-MM-DD h:nn:ss am/pm', Now);
      AppendText(Config + 'errors.log', D + ' ' + E.ClassName + ' - ' + E.Message);
    end;
  end;
end;

procedure TBackupForm.BackupButtonClick(Sender: TObject);

  function Validate: Boolean;
  var
    S: string;
    I: Integer;
  begin
    Result := False;
    for I := 0 to SourceBox.Items.Count - 1 do
    begin
      S := SourceBox.Items[I];
      if DirectoryExists(S) then
        Continue;
      MessageDlg('Aborting Backup', 'Backup folder "' + S + '" could not be found.', mtError, [mbOk], 0);
      SourceBox.ItemIndex := I;
      RemoveSourceButton.SetFocus;
      Exit;
    end;
    for I := 0 to DestBox.Items.Count - 1 do
    begin
      S := DestBox.Items[I];
      if DirectoryExists(S) then
        Continue;
      MessageDlg('Aborting Backup', 'Storage location "' + S + '" could not be found.', mtError, [mbOk], 0);
      DestBox.ItemIndex := I;
      RemoveDestButton.SetFocus;
      Exit;
    end;
    Result := True;
  end;

begin
  if not Validate then
    Exit;
  if MessageDlg('Confirmation', 'Are you sure you want to start the backup now?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    AppendLog('backup started');
    FThread := TBackupThread.Create(SourceBox.Items, DestBox.Items);
    FStart := Now;
    ProgressBar.Position := 0;
    ProgressBar.Visible := True;
    BackupTimer.Enabled := True;
    Enabled := False;
  end;
end;

procedure TBackupForm.BackupTimerTimer(Sender: TObject);
var
  D: string;
begin
  if FThread <> nil then
  begin
    if ProgressBar.Position = ProgressBar.Max then
      ProgressBar.Position := ProgressBar.Min;
    ProgressBar.Position := ProgressBar.Position + 10;
    BackupButton.Caption := FormatDateTime('h:nn:ss', Now - FStart);
  end
  else
  begin
    SourceBox.Items.SaveToFile(Config + 'source');
    DestBox.Items.SaveToFile(Config + 'dest');
    D := FormatDateTime('YYYY-MM-DD h:nn:ss am/pm', Now);
    SaveText(Config + 'last', D);
    AppendText(Config + 'backup.log', 'Backup on ' + D + #13'Sources'#13 + Trim(SourceBox.Items.Text) +
      #13'Destination'#13 + Trim(DestBox.Items.Text) + #13#13);
    AppendLog('backup complete');
    Caption := 'Backup - last backup ' + D;
    BackupButton.Caption := 'Start Backup';
    ProgressBar.Visible := False;
    Enabled := True;
    BackupTimer.Enabled := False;
  end;
end;

end.

