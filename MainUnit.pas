unit MainUnit;

{Extron DMP controller V1.00 (c)2025 Gerald J Holdsworth
DMP devices are (c) Extron Electronics.}

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
 StdCtrls, Buttons, StrUtils, TCPClient, RS232Client, GJHCustomComponents;

type

 { TMainForm }

 TMainForm = class(TForm)
   Inputs: TGroupBox;
   MuteIn_ch11: TPanel;
   MuteIn_ch10: TPanel;
   MuteOut_ch1: TPanel;
   MuteOut_ch2: TPanel;
   MuteOut_ch3: TPanel;
   MuteOut_ch4: TPanel;
   MuteOut_ch5: TPanel;
   MuteOut_ch6: TPanel;
   MuteOut_ch7: TPanel;
   MuteOut_ch8: TPanel;
   MuteIn_ch9: TPanel;
   MuteIn_ch2: TPanel;
   MuteIn_ch4: TPanel;
   MuteIn_ch3: TPanel;
   MuteIn_ch8: TPanel;
   MuteIn_ch7: TPanel;
   MuteIn_ch6: TPanel;
   MuteIn_ch5: TPanel;
   MuteIn_ch12: TPanel;
   Outputs: TGroupBox;
   MuteIn_ch1: TPanel;
   procedure Connect;
   procedure Disconnect;
   procedure FormCreate(Sender: TObject);
   function IsConnected: Boolean;
   procedure Send(query: String);
   function Receive: String;
   function GetLastError: String;
   procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
   procedure MuteClick(Sender: TObject);
   procedure UpdateMute(Button: Integer);
   procedure UpdateFader(Fader: Integer);
   procedure UpdateLabel(Fader: Integer);
   function ConnectToClient: Boolean;
   procedure FaderChange(Sender: TObject);
 private
  Client      : TTCPClient;
  SerClient   : TRS232Client;
  NumInFaders : Integer;
  NumOutFaders: Integer;
  Fupdating   : Boolean;
  InputFaders : array[0..11] of TRISCOSSlider;
  InputMutes  : array of TPanel;
  OutputFaders: array[0..7] of TRISCOSSlider;
  OutputMutes : array of TPanel;
  Modifier    : Integer;
  const //DMP Part numbers (from www.Extron.com)
   DMP128     : array[0..2] of String = ('60-1211-01',   //DMP 128
                                         '60-1179-01',   //DMP 128 C P
                                         '60-1179-10');  //DMP 128 C P AT
   DMP128Plus : array[0..5] of String = ('60-1511-01',   //DMP 128 Plus
                                         '60-1511-10',   //DMP 128 Plus AT
                                         '60-1512-01',   //DMP 128 Plus C
                                         '60-1512-10',   //DMP 128 Plus C AT
                                         '60-1513-01',   //DMP 128 Plus C V
                                         '60-1513-10');  //DMP 128 Plus C V AT
   DMP64      : array[0..0] of String = ('60-1054-01');  //DMP 64
   DMP64Plus  : array[0..3] of String = ('60-1823-01',   //DMP 64 Plus
                                         '60-1823-10',   //DMP 64 Plus C AT
                                         '60-1824-01',   //DMP 64 Plus C V
                                         '60-1824-10');  //DMP 64 Plus C V AT
   DMP44      : array[0..0] of String = ('60-2041-01');  //DMP 44 xi
   Fin = '4';
   Fout= '6';
   gains='wG';
   mutes='wM';
   FaderWidth = 50;
 public
  CommsOverTCP  : Boolean;
  CommsOverRS232: Boolean;
 end;

var
 MainForm: TMainForm;

implementation

{$R *.lfm}

uses CommsUnit;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
 Index: Integer=0;
begin
 for Index:=0 to 11 do
 begin
  InputFaders[Index]:=TRISCOSSlider.Create(Inputs as TComponent);
  InputFaders[Index].Parent:=Inputs as TWinControl;
  InputFaders[Index].Visible:=True;
  InputFaders[Index].Top:=0;
  InputFaders[Index].Left:=Index*FaderWidth;
  InputFaders[Index].Font.Size:=6;
  InputFaders[Index].Colour:=$000000;
  InputFaders[Index].Min:=-180;
  InputFaders[Index].Max:=800;
  InputFaders[Index].Position:=0;
  InputFaders[Index].Pointers:=True;
  InputFaders[Index].Faders:=True;
  InputFaders[Index].FaderColour:=csRed;
  InputFaders[Index].GradColour:=$FF0000;
  InputFaders[Index].FillSlider:=True;
  InputFaders[Index].Width:=FaderWidth;
  InputFaders[Index].Height:=680;
  InputFaders[Index].HexValue:=False;
  InputFaders[Index].ValueDiv:=10;
  InputFaders[Index].Suffix:='dB';
  InputFaders[Index].Caption:='Input';
  InputFaders[Index].ShowValue:=True;
  InputFaders[Index].Name:='Input'+IntToStr(Index+1);
  InputFaders[Index].Border3D:=True;
  InputFaders[Index].Outline:=csOutNone;
  InputFaders[Index].Tag:=Index+1;
  InputFaders[Index].OnChange:=@FaderChange;
 end;
 for Index:=0 to 7 do
 begin
  OutputFaders[Index]:=TRISCOSSlider.Create(Outputs as TComponent);
  OutputFaders[Index].Parent:=Outputs as TWinControl;
  OutputFaders[Index].Visible:=True;
  OutputFaders[Index].Top:=0;
  OutputFaders[Index].Left:=Index*FaderWidth;
  OutputFaders[Index].Font.Size:=6;
  OutputFaders[Index].Colour:=$000000;
  OutputFaders[Index].Min:=-1000;
  OutputFaders[Index].Max:=0;
  OutputFaders[Index].Position:=0;
  OutputFaders[Index].Pointers:=True;
  OutputFaders[Index].Faders:=True;
  OutputFaders[Index].FaderColour:=csBlue;
  OutputFaders[Index].GradColour:=$FF0000;
  OutputFaders[Index].FillSlider:=True;
  OutputFaders[Index].Width:=FaderWidth;
  OutputFaders[Index].Height:=680;
  OutputFaders[Index].HexValue:=False;
  OutputFaders[Index].ValueDiv:=10;
  OutputFaders[Index].Suffix:='dB';
  OutputFaders[Index].Caption:='Input';
  OutputFaders[Index].ShowValue:=True;
  OutputFaders[Index].Name:='Output'+IntToStr(Index+1);
  OutputFaders[Index].Border3D:=True;
  OutputFaders[Index].Outline:=csOutNone;
  OutputFaders[Index].Tag:=Index+21;
  OutputFaders[Index].OnChange:=@FaderChange;
 end;
 InputMutes  :=[MuteIn_ch1,MuteIn_ch2 ,MuteIn_ch3 ,MuteIn_ch4,
                MuteIn_ch5,MuteIn_ch6 ,MuteIn_ch7 ,MuteIn_ch8,
                MuteIn_ch9,MuteIn_ch10,MuteIn_ch11,MuteIn_ch12];
 OutputMutes :=[MuteOut_ch1,MuteOut_ch2,MuteOut_ch3,MuteOut_ch4,
                MuteOut_ch5,MuteOut_ch6,MuteOut_ch7,MuteOut_ch8];
end;

procedure TMainForm.Connect;
begin
{ if not IsConnected then
 begin}
  if CommsOverTCP then
  begin
   Client:=TTCPClient.Create;
   Client.Connect(CommsForm.IPAddress.Text,23);
  end;
  if(CommsOverRS232)and(CommsForm.SerialPortList.ItemIndex>=0)then
  begin
   SerClient:=TRS232Client.Create;
   SerClient.Port:=CommsForm.SerialPortList.Items[CommsForm.SerialPortList.ItemIndex];
   SerClient.Connect;
  end;
// end;
end;

procedure TMainForm.Disconnect;
begin
 if IsConnected then
 begin
  if CommsOverTCP   then Client.Free;
  if CommsOverRS232 then SerClient.Free;
 end;
end;

function TMainForm.IsConnected: Boolean;
begin
 Result:=False;
 if CommsOverTCP   then Result:=Client.Connected;
 if CommsOverRS232 then Result:=SerClient.Connected;
end;

procedure TMainForm.Send(query: String);
begin
 if IsConnected then
 begin
  if CommsOverTCP   then Client.Send(query);
  if CommsOverRS232 then SerClient.Send(query);
 end;
end;

function TMainForm.Receive: String;
var
 return: String='';
 status: Integer=0;
begin
 Result:='';
 if IsConnected then
 begin
  if CommsOverTCP   then Result:=Client.Receive;
  if CommsOverRS232 then Result:=SerClient.Receive;
 end;
end;

function TMainForm.GetLastError: String;
begin
 Result:='';
 if IsConnected then
 begin
  if CommsOverTCP   then Result:=Client.LastError;
  if CommsOverRS232 then Result:=SerClient.LastError;
 end;
end;

function TMainForm.ConnectToClient: Boolean;
var
 buffer: String = '';
 Fader : Integer=0;
 extron: Boolean=False;
begin
 Result:=False;
 Fupdating   :=False;
 //Connect to the device
 Connect;
 if IsConnected then
 begin
  //Get any opening messages
  buffer:=Receive; //But we don't care what they are - just clear the buffer
  //Determine what device it is
  Send('N'); //To get the part number (undocumented command)
  buffer:=Trim(Receive);
  //Should be an Digital Matrix Processor
  if(buffer in DMP128)or(buffer in DMP128Plus)then //Check for a DMP128
  begin
   //Adjust the number of faders accordingly
   NumInFaders :=12;
   NumOutFaders:=8;
   //Success flag
   extron:=True;
  end;
  if(buffer in DMP64)or(buffer in DMP64Plus)then //Check for a DMP64
  begin
   //Adjust the number of faders accordingly
   NumInFaders :=6;
   NumOutFaders:=4;
   //Success flag
   extron:=True;
  end;
  //The input/output levels are different depending on if it is a PLUS or not
  if(buffer in DMP64)    or(buffer in DMP128)    then Modifier:=2048;
  if(buffer in DMP64Plus)or(buffer in DMP128Plus)then Modifier:=0;
  //No DMP detected - Show the product code and quit
  if not extron then ShowMessage('Device not identified: '+buffer)
  else
  begin
   //Put the device name in the title
   Send(#$1B'CN'#13#10);
   buffer:=Trim(Receive);
   Caption:=buffer;
   //Show/Hide faders
   for Fader:=1 to 12 do
   begin
    if Fader<=NumInFaders then
    begin
     InputFaders[Fader-1].Visible:=True;
     InputMutes[Fader-1].Visible :=True;
    end
    else
    begin
     InputFaders[Fader-1].Visible:=False;
     InputMutes[Fader-1].Visible :=False;
    end;
    if Fader<=8 then
     if Fader<=NumOutFaders then
     begin
      OutputFaders[Fader-1].Visible:=True;
      OutputMutes[Fader-1].Visible :=True;
     end
     else
     begin
      OutputFaders[Fader-1].Visible:=False;
      OutputMutes[Fader-1].Visible :=False;
     end;
   end;
   //Adjust the widths
   Inputs.Width            :=(FaderWidth+1)*12;
   Outputs.Width           :=(FaderWidth+1)*8;
//   Outputs.Left            :=Inputs.Width+15;
//   Width                   :=Outputs.Left+Outputs.Width+15; //Should always be 1024
   //Update the fader levels, mute buttons, and labels
   for Fader:=1 to NumInFaders do
   begin
    //Inputs
    UpdateFader(Fader);
    UpdateMute(Fader);
    UpdateLabel(Fader);
    //Outputs
    if Fader<=NumOutFaders then
    begin
     UpdateFader(Fader+20);
     UpdateMute(Fader+20);
     UpdateLabel(Fader+20);
    end;
    Application.ProcessMessages;
   end;
   Result:=True;
  end;
  if not Result then Disconnect;
 end else ShowMessage('Could not connect : '+GetLastError);
end;

procedure TMainForm.FaderChange(Sender: TObject);
var
 query: String='';
 Fader: Integer=0;
begin
 //Only continue if connected OK
 if(IsConnected)and(not Fupdating)then
 begin
  //Get the fader number
  Fader:=(Sender as TRISCOSSlider).Tag;
  //Begin building the query to send
  query:=gains;
  //Input
  if Fader<20 then query:=query+Fin else
  begin //Output
   query:=query+Fout;
   dec(Fader,20);
  end;
  //Build the rest of the query
  query:=query+'00'+RightStr('00'+IntToStr(Fader-1),2)+'*'
          +RightStr('00000'+IntToStr((Sender as TRISCOSSlider).Position+Modifier),5)
          +'AU';
  //And send it to the device
  Send(query+#13#10);//Return is DsG40000*01868
  query:=Trim(Receive);//Otherwise it'll sit in the return buffer
 end;
end;

procedure TMainForm.UpdateFader(Fader: Integer);
var
 buffer: String = '';
 query : String = '';
 level : Integer=0;
 Input : Boolean=True;
begin
 Fupdating:=True;
 //Only if connected to the device
 if (IsConnected)
 and(((Fader>0)and(Fader<=NumInFaders)) //And are valid fader numbers
  or((Fader>20)and(Fader<=NumOutFaders+20)))then
 begin
  //Begin building the query
  query:=gains;
  //Input
  if Fader<20 then query:=query+Fin else
  begin //Output
   query:=query+Fout;
   dec(Fader,20);
   Input:=False;
  end;
  //Build the rest of the query
  query:=query+'00'+RightStr('00'+IntToStr(Fader-1),2)+'AU';
  //And send
  Send(query+#13#10);
  //Get a reply
  buffer:=Trim(Receive);
  //Only if not empty
  if not buffer.IsEmpty then
   //Ensure it is valid.
   if StrToIntDef(buffer,0)>0 then
   begin
    //Calculate the level in decibels/10
    level:=StrToInt(buffer)-Modifier;
    //Reflect this on the appropriate fader
    if Input then InputFaders[Fader-1].Position :=level
             else OutputFaders[Fader-1].Position:=level;
   end;
 end;
 Fupdating:=False;
end;

procedure TMainForm.MuteClick(Sender: TObject);
var
 Button: Integer=0;
 query : String='';
 buffer: String='';
 Muted : Boolean=False;
 Input : Boolean=True;
 colour: TColor;
begin
 //Only if connected
 if IsConnected then
 begin
  //Get the button number
  Button:=(Sender as TPanel).Tag;
  //And current MUTE status
  Muted :=(Sender as TPanel).Color=clRed;
  //Start building the query
  query:=mutes;
  //Inputs
  if Button<20 then query:=query+Fin else
  begin //Outputs
   query:=query+Fout;
   dec(Button,20);
   Input:=False;
  end;
  //Finish building the query
  query:=query+'00'+RightStr('00'+IntToStr(Button-1),2)+'*';
  //Adjusting for whether we want to MUTE or UNMUTE
  if Muted then query:=query+'0' else query:=query+'1';
  query:=query+'AU';
  //Send it
  Send(query+#13#10);//Return is DsM40000*0 or DsM40000*1
  //See if there is a reply
  buffer:=Trim(Receive);
  //And it isn't empty
  if not buffer.IsEmpty then
   //'DsM40000*<state>' shows that it has been accepted
   if LeftStr(buffer,3)='DsM' then
   begin
    //So change the button colour
    colour:=clLime;
    if buffer[10]='0' then colour:=clLime;
    if buffer[10]='1' then colour:=clRed;
    (Sender as TPanel).Color:=colour;
    //No reply, so ask
   end else if Input then UpdateMute(Button) else UpdateMute(Button+20);
 end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 Disconnect;
 CommsForm.Show;
end;

procedure TMainForm.UpdateMute(Button: Integer);
var
 buffer: String = '';
 query : String = '';
 muted : Boolean=False;
 Input : Boolean=True;
 colour: TColor;
begin
 //Only if connected
 if (IsConnected)
 and(((Button>0)and(Button<=NumInFaders)) //And button number is valid
  or((Button>20)and(Button<=NumOutFaders+20)))then
 begin
  //Start building the query
  query:=mutes;
  //Inputs
  if Button<20 then query:=query+Fin else
  begin //Outputs
   query:=query+Fout;
   dec(Button,20);
   Input:=False;
  end;
  //Finish building the query
  query:=query+'00'+RightStr('00'+IntToStr(Button-1),2)+'*AU';
  //And send it
  Send(query+#13#10);
  //Get the reply
  buffer:=Trim(Receive);
  //If not empty
  if not buffer.IsEmpty then
  begin
   //Make sure it is valid - it will either be 0 or 1
   if StrToIntDef(buffer,255)<>255 then
   begin
    //If it is 1, then MUTE is active
    muted:=buffer='1';
    //Change the button colour accordingly
    if Muted then colour:=clRed else colour:=clLime;
    if Input then InputMutes[Button-1].Color :=colour
             else OutputMutes[Button-1].Color:=colour;
   end;
  end;
 end;
end;

procedure TMainForm.UpdateLabel(Fader: Integer);
var
 buffer: String = '';
 query : String = '';
begin
 //Only if connected
 if (IsConnected)
 and(((Fader>0)and(Fader<=NumInFaders)) //And button number is valid
  or((Fader>20)and(Fader<=NumOutFaders+20)))then
 begin //Undocumented commands to retrieve the label names
  if Fader<20 then query:=#$1B+IntToStr(Fader)   +'NI'  //Inputs
              else query:=#$1B+IntToStr(Fader-20)+'NO'; //Outputs
  //Send the query
  Send(query+#13#10);
  //Get the reply
  buffer:=Trim(Receive);
  //If not empty
  if not buffer.IsEmpty then //Update the label
   if Fader<20 then InputFaders[Fader-1].Caption  :=buffer
               else OutputFaders[Fader-21].Caption:=buffer;
 end;
end;

end.

