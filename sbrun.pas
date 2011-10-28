program sbrun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, sbmain;

var
  SB:TSBProgram;
  s:string;
begin
  SBIO:=TSBIO.Create;
  SB:=TSBProgram.Create;

  if paramcount>0 then
  begin
    SBIO.OpenFile(0,paramstr(1),'r');
    while not SBIO.IsEof(0) do
    begin
      SB.AddCodeLine(SBIO.ReadFromFile(0));
    end;
    SBIO.CloseFile(0);
    s:=SB.Run;
    if s<>'' then writeln('Program returned the value:'+s);
  end;

  SB.Free;
  SBIO.Free;
end.

