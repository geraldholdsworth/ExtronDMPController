unit TCPClient;

{$MODE objfpc}{$H+}{$M+}

interface

uses
  Classes,SysUtils,Sockets;

type
  TTCPClient = class
  private
   FSocket : LongInt;
   function IsConnected: Boolean;
   function GetLastError: String;
  public
   Constructor Create;
   Destructor Destroy; override;
  published
   procedure Connect(IP: String; Port: Integer);
   procedure Disconnect;
   function Receive: String;
   procedure Send(Output: String);
   property Connected : Boolean read IsConnected;
   property LastError : String read GetLastError;
end;

implementation

{ TTCPClient }

{-------------------------------------------------------------------------------
Creates the class
-------------------------------------------------------------------------------}
constructor TTCPClient.Create;
begin
 inherited Create;
 FSocket:=-1;
end;

{-------------------------------------------------------------------------------
Are we connected to a server?
-------------------------------------------------------------------------------}
function TTCPClient.IsConnected: Boolean;
begin
 Result:=FSocket<>-1;
end;

{-------------------------------------------------------------------------------
What was the last error reported?
-------------------------------------------------------------------------------}
function TTCPClient.GetLastError: String;
begin
 if IsConnected then
 begin
  Result:='Unknown error ('+IntToStr(socketerror)+')';
  case socketerror of
   EsockADDRINUSE      : Result:='Socket address is already in use';
   EsockEACCESS        : Result:='Access forbidden';
   EsockEBADF          : Result:='Alias: bad file descriptor';
   EsockEFAULT         : Result:='An error occurred';
   EsockEINTR          : Result:='Operation interrupted';
   EsockEINVAL         : Result:='Invalid value specified';
   EsockEMFILE         : Result:='Error code ?';
   EsockEMSGSIZE       : Result:='Wrong message size';
   EsockENOBUFS        : Result:='No buffer space available';
   EsockENOTCONN       : Result:='Not connected';
   EsockENOTSOCK       : Result:='File descriptor is not a socket';
   EsockEPROTONOSUPPORT: Result:='Protocol not supported';
   EsockEWOULDBLOCK    : Result:='Operation would block';
   //Undocumented error numbers - results are theoritical
   60                  : Result:='Request timed out';
   65                  : Result:='No route to host';
  end;
 end else Result:='Not Connected';
end;

{-------------------------------------------------------------------------------
Connect to a server, if not already
-------------------------------------------------------------------------------}
procedure TTCPClient.Connect(IP: String; Port: Integer);
var
 SAddr : TInetSockAddr;
begin
 //Make sure we're not already connected
 if not IsConnected then
 begin
  //Create the socket
  FSocket:=fpSocket(AF_INET,SOCK_STREAM,0);
  //Created OK?
  if FSocket<>-1 then
  begin
   //Set up the server details
   SAddr.sin_family:=AF_INET;
   SAddr.sin_port:=HtoNS(Port);
   SAddr.sin_addr.s_addr:=StrToNetAddr(IP).s_addr;
   //And connect - Timeout is 75 seconds
   if fpConnect(FSocket,@SAddr,SizeOf(SAddr))=-1 then Disconnect; //If not connected.
  end;
 end;
end;

{-------------------------------------------------------------------------------
Disconnect
-------------------------------------------------------------------------------}
procedure TTCPClient.Disconnect;
begin
 if IsConnected then
 begin
  CloseSocket(FSocket);
  FSocket:=-1;
 end;
end;

{-------------------------------------------------------------------------------
Receives a response - no timeouts
-------------------------------------------------------------------------------}
function TTCPClient.Receive: String;
var
 Buffer: String[255];
 Lcount: Integer;
 Index : Integer;
begin
 Result:='';
 //Make sure we're connected
 if IsConnected then
 begin
  //Initialise the buffer
  StrPCopy(@Buffer,StringOfChar(#0,255));
  //Then send it
  Lcount:=fpRecv(FSocket,@Buffer,SizeOf(Buffer),0);
  //Copy the response back to the result
  if Lcount>0 then
   for Index:=0 to Lcount do Result:=Result+Buffer[Index];
 end;
end;

{-------------------------------------------------------------------------------
Sends a string
-------------------------------------------------------------------------------}
procedure TTCPClient.Send(Output: String);
var
 Buffer: String[255];
begin
 //Make sure we're connected
 if IsConnected then
 begin
  //Copy the query into the buffer
  StrPCopy(@Buffer,Output);
  //Then send it
  fpSend(FSocket,@Buffer,Length(Output),0);
 end;
end;

{-------------------------------------------------------------------------------
Destroys the class
-------------------------------------------------------------------------------}
destructor TTCPClient.Destroy;
begin
 if IsConnected then Disconnect;
 inherited Destroy;
end;

end.
