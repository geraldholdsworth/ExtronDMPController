program ExtronDMPControl;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
 cthreads,
 {$ENDIF}
 {$IFDEF HASAMIGA}
 athreads,
 {$ENDIF}
 Interfaces, // this includes the LCL widgetset
 Forms, MainUnit, CommsUnit
 { you can add units after this };

{$R *.res}

begin
 RequireDerivedFormResource:=True;
 Application.Title:='Extron DMP Control';
 Application.Scaled:=True;
 {$PUSH}{$WARN 5044 OFF}
 Application.MainFormOnTaskbar:=True;
 {$POP}
 Application.Initialize;
 Application.CreateForm(TCommsForm, CommsForm);
 Application.CreateForm(TMainForm, MainForm);
 Application.Run;
end.

