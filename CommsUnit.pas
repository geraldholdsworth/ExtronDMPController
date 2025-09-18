unit CommsUnit;

{Extron DMP controller V1.00 (c)2025 Gerald J Holdsworth
DMP units are (c) Extron Electronics.

Currently TCP comms only
}

{$mode ObjFPC}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, MainUnit,
 LazSynaSer;

type

 { TCommsForm }

 TCommsForm = class(TForm)
   RS232: TRadioButton;
   TCP: TRadioButton;
   IPAddress: TEdit;
   Label1: TLabel;
   OKButton: TButton;
   CancelButton: TButton;
   SerialPortList: TComboBox;
   procedure CancelButtonClick(Sender: TObject);
   procedure FormShow(Sender: TObject);
   procedure OKButtonClick(Sender: TObject);
   function CheckForSerialPorts: Boolean;
 private

 public

 end;

var
 CommsForm: TCommsForm;

implementation

{$R *.lfm}

{ TCommsForm }

procedure TCommsForm.OKButtonClick(Sender: TObject);
begin
 MainForm.CommsOverTCP  :=TCP.Checked;
 MainForm.CommsOverRS232:=RS232.Checked;
 if MainForm.ConnectToClient then
 begin
  MainForm.Show;
  Hide;
 end;
end;

procedure TCommsForm.CancelButtonClick(Sender: TObject);
begin
 Application.Terminate;
end;

procedure TCommsForm.FormShow(Sender: TObject);
begin
 IPAddress.Text:='192.168.0.210';
 CheckForSerialPorts;
end;

function TCommsForm.CheckForSerialPorts: Boolean;
var
 LPorts: TStringArray;
 LItem : String;
begin
 SerialPortList.Items.Clear;
 LItem:=GetSerialPortNames;
 Result:=not LItem.IsEmpty;
 if Result then
 begin
  LPorts:=LItem.Split(' ');
  for LItem in LPorts do if not LItem.IsEmpty then SerialPortList.Items.Add(LItem);
 end;
end;

end.

