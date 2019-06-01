program Backer;

{$mode delphi}
{$define UseCThreads}

uses
  {$IFDEF Unix}{$IFDEF UseCThreads}CThreads, {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, Main
  { Add units here };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TBackupForm, BackupForm);
  Application.Run;
end.

