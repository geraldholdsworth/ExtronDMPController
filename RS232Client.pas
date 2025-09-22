unit RS232Client;

{$mode ObjFPC}{$H+}{$M+}

interface

uses
 Classes, SysUtils, LazSynaSer;

type
  TRS232Client = class
  private
   FSerClient   : TBlockSerial;
   FBaud        : Cardinal;
   FBits        : Byte;
   FParity      : Char;
   FStopBits    : Integer;
   FSoftFlow    : Boolean;
   FHardFlow    : Boolean;
   FPort        : String;
   function IsConnected: Boolean;
   function GetLastError: String;
  public
   Constructor Create;
   Destructor Destroy; override;
  published
   procedure Connect;
   procedure Disconnect;
   function Receive: String;
   procedure Send(Output: String);
   property Connected : Boolean  read IsConnected;
   property LastError : String   read GetLastError;
   property Port      : String   read FPort     write FPort;
   property Baud      : Cardinal read FBaud     write FBaud;
   property Bits      : Byte     read FBits     write FBits;
   property Parity    : Char     read FParity   write FParity;
   property StopBits  : Integer  read FStopBits write FStopBits;
   property SoftFlow  : Boolean  read FSoftFlow write FSoftFlow;
   property HardFlow  : Boolean  read FHardFlow write FHardFlow;
end;

implementation

{ TRS232Client }

{-------------------------------------------------------------------------------
Creates the class
-------------------------------------------------------------------------------}
constructor TRS232Client.Create;
begin
 inherited Create;
 FSerClient:=nil;
 FPort     :='';
 FBaud     :=9600;
 FBits     :=8;
 FParity   :='N';
 FStopBits :=SB1;
 FSoftFlow :=False;
 FHardFlow :=False;
end;

{-------------------------------------------------------------------------------
Are we connected to a server?
-------------------------------------------------------------------------------}
function TRS232Client.IsConnected: Boolean;
begin
 Result:=FSerClient<>nil;
end;

{-------------------------------------------------------------------------------
Connect to a server, if not already
-------------------------------------------------------------------------------}
procedure TRS232Client.Connect;
begin
 //Make sure we're not already connected
 if not IsConnected then
 begin
  FSerClient:=TBlockSerial.Create;
  FSerClient.LinuxLock:=False;
  {$IFDEF UNIX}
  FSerClient.NonBlock:=True;
  {$ENDIF}
  FSerClient.Connect(FPort);
  FSerClient.Config(FBaud,FBits,FParity,FStopBits,FSoftFlow,FHardFlow);
 end;
end;

{-------------------------------------------------------------------------------
What was the last error reported?
-------------------------------------------------------------------------------}
function TRS232Client.GetLastError: String;
begin
 if IsConnected then Result:=FSerClient.LastErrorDesc else Result:='Not Connected';
end;

{-------------------------------------------------------------------------------
Disconnect
-------------------------------------------------------------------------------}
procedure TRS232Client.Disconnect;
begin
 if IsConnected then
 begin
  FSerClient.Free;
  FSerClient:=nil;
 end;
end;

{-------------------------------------------------------------------------------
Receives a response - no timeouts
-------------------------------------------------------------------------------}
function TRS232Client.Receive: String;
var
 return: String='';
 status: Integer=0;
begin
 Result:='';
 if IsConnected then
 begin
  status:=0;
  //And while there is data to be read
  while FSerClient.WaitingData>0 do
  begin
   return:=FSerClient.RecvPacket(0);
   status:=FSerClient.LastError;
   if status=0 then Result:=Result+return; //Add it to the return string
  end;
 end;
end;

{-------------------------------------------------------------------------------
Creates the class
-------------------------------------------------------------------------------}
procedure TRS232Client.Send(Output: String);
begin
 if IsConnected then
 begin
  FSerClient.SendString(Output);
  Sleep(100); //Make sure we don't rush it
 end;
end;

{-------------------------------------------------------------------------------
Destroys the class
-------------------------------------------------------------------------------}
destructor TRS232Client.Destroy;
begin
 if IsConnected then Disconnect;
 inherited Destroy;
end;

end.

