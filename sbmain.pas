unit sbmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math,
  crt;

const
 iSBVer=1;
 sSBVer='0.1';

 chStr='"';

type
 TSBVar=record
  name,value:string;
 end;

 TStrStack=class
     data:array of string;
     p:integer;
     
     constructor Create;
     procedure Put(s:string);
     function Get:string;
     function GetTop:string;
     function GetSize:integer;
     function GetNext:string;
     procedure GoToBegin;
     procedure Print;
     destructor Destroy; virtual;
 end;
 
 TSBProgram=class
     Name,Parameters:string;

     Code:TStringList;
     ResultValue:string;
     CurPos:integer;
     ForceExit,isCompiled:boolean;
     Variables:array of TSBVar;
     
     sWhile,sFor,sDo,sIf,sElse:TStrStack;
     
     isSub:boolean;
     Subs:array of TSBProgram;

     constructor Create;
     procedure ClearCode;
     procedure AddCodeLine(s:string);
     function GetOperatorPriority(s:string):integer;
     function RunExpression(s:string):string;
     function Run:string;
     function DoOperator(s,s1,s2:string):string;
     function DoSub(s:string):string;
     procedure SetVar(s,v:string);
     function GetVar(s:string):string;
     destructor Destroy; virtual;
 end;
 
 // Даний клас реалізує процес введення/виведення даних та взаємодії з користувачем,
 // при необхідності його можна перекрити
 TSBIO=class
     Files:array of TextFile;
 
     constructor Create;
     destructor Destroy; virtual;
     
     procedure Print(s:string;caret:boolean); virtual;
     procedure ErrorF(s:string); virtual;
     function Input(s:string):string; virtual;
     
     procedure OpenFile(ind:integer;n,t:string); virtual;
     procedure WriteToFile(ind:integer;s:string); virtual;
     function ReadFromFile(ind:integer):string; virtual;
     function IsEof(ind:integer):boolean; virtual;
     procedure CloseFile(ind:integer); virtual;
     
     // Text mode terminal functions
     procedure TMT_Clear; virtual;
     procedure TMT_Sleep(t:integer); virtual;
     procedure TMT_DelLine; virtual;
     procedure TMT_InsLine; virtual;
     procedure TMT_Locate(x,y:integer); virtual;
     function TMT_KeyPressed:boolean; virtual;
     function TMT_ReadKey:string; virtual;
     procedure TMT_Color(c:integer); virtual;
     procedure TMT_Background(c:integer); virtual;
     procedure TMT_VideoMode(s:string); virtual;
 end;
 
function _pos(s1,s2:string):integer;
function _posbk(s1,s2:string):integer;
function _isnumber(s:string):boolean;
function _isoperator(s:string):boolean;
function _isconstruction(s:string):boolean;
function _isstr(s:string):string;
function _getstr(s:string):string;
function _makestr(s:string):string;
function _isidentifier(s:string):boolean;
function _isfunc(s:string):string;
function _floattostr(x:extended):string;
function _strtofloat(s:string):extended;

var
   OSDecSymbol:char;
   SBIO:TSBIO;

implementation

// String functions
function _floattostr(x:extended):string;
var
 t:string;
 i:integer;
begin
 t:=floattostr(x);
 i:=pos(osdecsymbol,t);
 if i>0 then  t[i]:='.';
 result:=t;
end;

function _strtofloat(s:string):extended;
var
 t:string;
 i:integer;
begin
 i:=pos('.',s);
 if i>0 then s[i]:=osdecsymbol;
 result:=strtofloat(s);
end;

function _isfunc(s:string):string;
var
 i:integer;
 t:string;
begin
 result:='';
 i:=_pos('(',s);
 if i>0 then
 begin
  t:=copy(s,1,i-1);
 end;
 if _isidentifier(t) then result:=t;
end;

function _isidentifier(s:string):boolean;
var
 i,d:integer;
begin
 if trim(s)='' then
 begin
   result:=false;
   exit;
 end;
 if trim(s)='?' then
 begin
   result:=true;
   exit;
 end;
 result:=true; d:=0;
 for i:=1 to length(s) do
 begin
   if (s[i]='[') then inc(d);
   if (s[i]=']') then dec(d);
   if not (s[i]in['a'..'z','A'..'Z','[',']',
                  '_','0'..'9','.']) then
   begin
    if d=0 then result:=false;
   end;
 end;
 if result then
  result:= s[1] in ['a'..'z','A'..'Z','_','.'];
end;

function _getstr(s:string):string;
var
 i,j:integer;
 t:string;
begin
 try
 result:=s;
 if (s[1]='"') and (s[length(s)]='"') then
 begin
  t:=copy(s,2,length(s)-2);
  i:=1;
  while i<length(t) do
  begin
   if (t[i]='"')and(t[i+1]='"') then delete(t,i,1);
   inc(i);
  end;
  result:=t;
 end;
 except
  result:=s;
//  writeln('Error:String parameter expected');
 end;
end;

function _makestr(s:string):string;
var
 i,j:integer;
 t:string;
begin
 t:=s;
 i:=1;
 while i<length(t) do
 begin
  if t[i]='"' then
  begin
   insert('"',t,i);
   inc(i);
  end;
  inc(i);
 end;
 result:='"'+t+'"';
end;

function _isstr(s:string):string;
var
 i,j:integer;
 t:string;
begin
 result:=#0;
 if s='' then exit;
 if (s[1]='"') and (s[length(s)]='"') then
 begin
  t:=copy(s,2,length(s)-2);
  i:=1;
  while i<length(t) do
  begin
   if (t[i]='"')and(t[i+1]='"') then delete(t,i,1);
   inc(i);
  end;
  result:=t;
 end;
end;

function _isoperator(s:string):boolean;
begin
 result:=false;
 if length(s)=1 then
 begin
  if s[1] in ['+','-','*','/','\','%','&','|','=','<','>','^','!'] then result:=true;
 end else
 begin
  if (s='<=')or(s='>=')or(s='!=') then result:=true;
 end;
end;

function _isconstruction(s:string):boolean;
begin
 result:=false;
// writeln(s);
 s:=lowercase(s);
 if (s='if')or(s='else')or(s='for')or(s='next')or(s='while')or
    (s='wend')or(s='do')or(s='until')or
    (s='end')or(s='sub')or(s='use') then result:=true;
end;

function _pos(s1,s2:string):integer;
var
 i,j:integer;
 st:boolean;
begin
 result:=0;
 st:=false;
 for i:=1 to length(s2) do
 begin
  if s2[i]=chStr then st:=not st;
  if (s2[i]=s1[1])and(not st) then
  begin
   if length(s1)<=length(s2)-i+1 then
   begin
    if trim(s1)=trim(copy(s2,i,length(s1))) then
    begin
     result:=i;
     exit;
    end;
   end;
  end;
 end;
end;

function _posbk(s1,s2:string):integer;
var
 i,j:integer;
 st:boolean;
begin
 result:=0;
 st:=false;
 for i:=length(s2) downto 1 do
 begin
  if s2[i]=chStr then st:=not st;
  if (s2[i]=s1[1])and(not st) then
  begin
   if length(s1)<=length(s2)-i+1 then
   begin
    if trim(s1)=trim(copy(s2,i,length(s1))) then
    begin
     result:=i;
     exit;
    end;
   end;
  end;
 end;
end;

function _isnumber(s:string):boolean;
var
  a:extended;
  i:integer;
begin
 val(s,a,i);
 result:=i=0;
end;


// String Stack
constructor TStrStack.Create;
begin
  p:=0;
end;

destructor TStrStack.Destroy;
begin
  //---
end;

procedure TStrStack.Put(s:string);
begin
 if s<>'' then
 begin
  setlength(data,length(data)+1);
  data[length(data)-1]:=s;
  inc(p);
 end;
end;

function TStrStack.Get:string;
var
 t:integer;
begin
  t:=length(data);
  if t>0 then
  begin
   result:=data[t-1];
   setlength(data,t-1);
  end else
   result:=#0;
end;

function TStrStack.GetTop:string;
begin
   result:=data[length(data)-1];
end;

function TStrStack.GetNext:string;
begin
 result:=data[p];
 inc(p);
end;

procedure TStrStack.GoToBegin;
begin
 p:=0;
end;

function TStrStack.GetSize:integer;
begin
  result:=length(data);
end;

procedure TStrStack.Print;
var
 i:integer;
begin
 for i:=0 to length(data)-1 do
  writeln('|',data[i]);
//  SBIO.ErrorF('TStrStack.GetSize is a plug');
end;

//  Simple Basic Program

constructor TSBProgram.Create;
begin
  Code:=TStringList.Create;
  sWhile:=TStrStack.Create;
  sFor:=TStrStack.Create;
  sDo:=TStrStack.Create;
  sIf:=TStrStack.Create;
  sElse:=TStrStack.Create;
  isSub:=false;
  setlength(Subs,0);
  isCompiled:=false;
{  Print:=MyPrint;
  ErrorF:=MyErrorF;
  Input:=MyInput;}
end;

destructor TSBProgram.Destroy;
var
 i:integer;
begin
  Code.Free;
  sFor.Free;
  sWhile.Free;
  sDo.Free;
  sIf.Free;
  sElse.Free;
  if length(Subs)>0 then
     for i:=0 to length(Subs)-1 do Subs[i].Free;
end;

function TSBProgram.RunExpression(s:string):string;  // Parser
var
 i,a,d,ta:integer;
 b:boolean;
 
 E,X,Y,Z:TStrStack;
 t,t1,o1,o2:string;
 tokens:array of string;
begin
 // Tokenize
 i:=1;
 E:=TStrStack.Create;
 b:=false;

 i:=0;
 repeat    // Конвертуємо унарні мінуси у форму -1* крім найпершого
  i:=_pos('-',s);
  if i>0 then
  begin
   s[i]:='@';
   if i>1 then
   begin
    if s[i-1] in ['(','[','+','*','-','/'] then insert('1*',s,i+1);
    if i<length(s) then
     if s[i+1]='(' then insert('1*',s,i+1);
   end else
   begin
    s[i]:='@';
    insert('1*',s,i+1);
   end;
  end;
 until i=0;

 i:=0;
 repeat
  i:=_pos('@',s);
  if i>0 then s[i]:='-';
 until i=0;

 i:=0;  // Обробляємо оператор NOT та його варіанти
 repeat
  i:=_pos('!',s);
  if (i>0)and(i<length(s)) then
   if s[i+1]<>'=' then s[i]:='@' else s[i]:='$';
 until i=0;

 i:=0;
 repeat
  i:=_pos('@',s);
  if i>0 then begin
               s[i]:='!';
               insert('0',s,i);
              end;
 until i=0;
 i:=0;
 repeat
  i:=_pos('$',s);
  if i>0 then begin
               s[i]:='!';
              end;
 until i=0;

 i:=1; ta:=0;
 while i<=length(s) do       // Виділяємо токени
 begin
   if s[i] = chStr then b:=not b;
   if (s[i]='[')and(not b) then begin inc(ta); end;
   if (s[i]=']')and(not b) then begin dec(ta); end;

   if (s[i]='(')and(not b)and(i>1)and(ta=0) then
   begin
    if not(s[i-1] in ['=','*','/','^','\','%',')','&','|','(','+','-','<','>']) then
    begin
     d:=0;
     for a:=i to length(s) do
     begin
       if s[a]=chStr then b:=not b else
       if (s[a]='(')and(not b) then inc(d) else
       if (s[a]=')')and(not b) then dec(d);

       if d=0 then break;
     end;
     t:=copy(s,1,a);
     E.Put(t);
     delete(s,1,a);
     i:=1;
     continue;
    end;
   end;
   
   if (s[i] = '(')and(not b)and(ta=0) then
   begin
    E.Put('(');
    delete(s,1,i);
    i:=1;
    continue;
   end;
   if (s[i] = '-')and(not b)and(ta=0) then
   begin
     if i=1 then
     begin
      inc(i);
      continue;
     end;
   end;

   if (s[i] in ['+','-'])and(i>2)and(not b)and(ta=0) then
   begin
    if (s[i-1] in ['e','E']) and (s[i-2] in ['1','2','3','4','5','6','7','8','9','0']) then
    begin
     inc(i);
     continue;
    end;
   end;
   if (s[i] in ['<','>','!'])and(i<length(s))and(not b)and(ta=0) then
   begin
    if s[i+1]='=' then
    begin
     t:=copy(s,1,i-1);
     E.Put(t);
     E.Put(s[i]+'=');
     delete(s,1,i+1);
     i:=1;
     continue;
    end;
   end;

   if (s[i] in ['+','-','*','/','\','%','^',')','=','<','>','&','|'])and(not b)and(ta=0) then
   begin
    t:=copy(s,1,i-1);
    E.Put(t);
    E.Put(s[i]);
    delete(s,1,i);
    i:=1;
    continue;
   end;
   inc(i);
 end;
 E.Put(s);
 
{ writeln('E:');
 E.Print;}

 // Reverse notation
 E.GoToBegin;
 X:=TStrStack.Create;
 Y:=TStrStack.Create;
 for i:=1 to E.GetSize do
 begin
   t:=E.GetNext;
   if _isoperator(t) then
   begin
     if Y.GetSize>0 then
     begin
      if GetOperatorPriority(t)=GetOperatorPriority(Y.GetTop) then
      begin
        X.Put(Y.Get);
        Y.Put(t);
      end else
      if GetOperatorPriority(t)>GetOperatorPriority(Y.GetTop) then
        Y.Put(t)
      else
      begin
        while 1=1 do
        begin
          if Y.GetSize=0 then break else
          begin
           if GetOperatorPriority(t)>=GetOperatorPriority(Y.GetTop) then
             break
           else
             X.Put(Y.Get);
          end;
        end;
        Y.Put(t);
      end;
     end else
        Y.Put(t);
   end else
   begin
     if t=')' then
     begin
        while (Y.GetSize>0) do
        begin
         t1:=Y.Get;
         if t1<>'(' then X.Put(t1);
        end;
     end else
     if t='(' then Y.Put(t) else X.Put(t);
   end;
 end;
 while Y.GetSize>0 do X.Put(Y.Get);

{  writeln('X:');
 X.Print;
 writeln('Y:');
 Y.Print;}


 // Run Expression
 Z:=TStrStack.Create;
 X.GoToBegin;
 for i:=1 to X.GetSize do
 begin
   t:=X.GetNext;
   if _isoperator(t) then
   begin
     o2:=Z.Get;
     o1:=Z.Get;
     if _pos('(',o1)>0 then o1:=DoSub(o1) else
          if _isidentifier(o1) then o1:=GetVar(o1);
     if _pos('(',o2)>0 then o2:=DoSub(o2) else
          if _isidentifier(o2) then o2:=GetVar(o2);
//     writeln('o1=',o1,'  o2=',o2);
     Z.Put(DoOperator(t,o1,o2));
   end else
     Z.Put(t);
 end;
 
 E.Free;
 X.Free;
 Y.Free;
 o1:=Z.Get;
// writeln(o1);
 if _pos('(',o1)>0 then o1:=DoSub(o1) else
           if _isidentifier(o1) then o1:=GetVar(o1);
 result:=o1;
// writeln('AAA:',result);
 Z.Free;
end;

function TSBProgram.GetOperatorPriority(s:string):integer;
begin
 result:=0;
 if s='(' then result:=1 else
 if (s='<')or(s='>')or(s='>=')or(s='>=')
     or(s='=')or(s='!=') then result:=3 else
 if (s='&')or(s='|') then result:=2 else
 if (s='+')or(s='-') then result:=5 else
 if s='^' then result:=7 else
 if s='!' then result:=8 else
 if (s='*')or(s='/')or(s='\')or(s='%') then result:=6;
end;

function TSBProgram.DoOperator(s,s1,s2:string):string;
var
 t:string;
 i,j:integer;
begin
 try
   if s='+' then begin
                if (s1[1]='"') or (s2[1]='"') then
                 result:=_makestr(_getstr(s1)+_getstr(s2))
                else
                 result:=_floattostr(_strtofloat(s1)+_strtofloat(s2));
               end;
   if s='-' then begin
                 if s1[1]='"' then
                 begin
                   t:=_getstr(s1);
                   delete(t,length(t)-strtoint(s2)+1,strtoint(s2));
                   result:=_makestr(t);
                 end else
                   result:=_floattostr(_strtofloat(s1)-_strtofloat(s2));
                 end;
   if s='*' then begin
                 if s1[1]='"' then
                 begin
                  t:='';
                  for j:=1 to strtoint(s2) do t:=t+_getstr(s1);
                  result:=_makestr(t);
                 end else
                   result:=_floattostr(_strtofloat(s1)*_strtofloat(s2));
                 end;
   if s='/' then  result:=_floattostr(_strtofloat(s1)/_strtofloat(s2));
if s='\' then  result:=inttostr(strtoint(s1) div strtoint(s2));
 if s='%' then  result:=inttostr(strtoint(s1) mod strtoint(s2));

 if s='=' then  begin
                // writeln('s1=',s1,'|  s2=',s2,'|');
                 if (s1[1]='"') or (s2[1]='"') then
                 begin if _getstr(s1)=_getstr(s2) then result:='1' else result:='0'; end
                 else
                  if _strtofloat(s1)=_strtofloat(s2) then result:='1' else result:='0';
                end;
 if s='!=' then  begin
                 if (s1[1]='"') or (s2[1]='"') then
                 begin if _getstr(s1)<>_getstr(s2) then result:='1' else result:='0'; end
                 else
                  if _strtofloat(s1)<>_strtofloat(s2) then result:='1' else result:='0';
                end;
 if s='<' then  begin
                 if (s1[1]='"') or (s2[1]='"') then
                 begin if _getstr(s1)<_getstr(s2) then result:='1' else result:='0'; end
                 else
                  if _strtofloat(s1)<_strtofloat(s2) then result:='1' else result:='0';
                end;
 if s='>' then  begin
                 if (s1[1]='"') or (s2[1]='"') then
                 begin if _getstr(s1)>_getstr(s2) then result:='1' else result:='0'; end
                 else
                  if _strtofloat(s1)>_strtofloat(s2) then result:='1' else result:='0';
                end;
 if s='<=' then  begin
                 if (s1[1]='"') or (s2[1]='"') then
                 begin if _getstr(s1)<=_getstr(s2) then result:='1' else result:='0'; end
                 else
                  if _strtofloat(s1)<=_strtofloat(s2) then result:='1' else result:='0';
                end;
 if s='>=' then  begin
                 if (s1[1]='"') or (s2[1]='"') then
                 begin if _getstr(s1)>=_getstr(s2) then result:='1' else result:='0'; end
                 else
                  if _strtofloat(s1)>=_strtofloat(s2) then result:='1' else result:='0';
                end;
 if s='&' then  begin
                 if (s1='1')and(s2='1') then result:='1' else result:='0';
                end;
 if s='|' then  begin
                 if ((s1='1')and(s2='0'))or((s1='0')and(s2='1'))or
                    ((s1='1')and(s2='1')) then result:='1' else result:='0';
                end;
 if s='!' then  begin
                 if s2='0' then result:='1' else result:='0';
                end;
 if s='^' then  begin
                  result:=_floattostr(power(_strtofloat(s1),_strtofloat(s2)));
                end;

  except
    SBIO.ErrorF('Error in operator: '+s1+s+s2);
    result:='0';
  end;
end;

procedure TSBProgram.ClearCode;
begin
  Code.Clear;
end;

procedure TSBProgram.AddCodeLine(s:string);
var
 i:integer;
 t:string;
begin
  i:=_pos('''',s);
  if i>0 then delete(s,i,length(s)-i+1);
  repeat
    i:=_pos(':',s);
    if i>0 then
    begin
     t:=trim(copy(s,1,i-1));
     if t<>'' then Code.Add(t);
     delete(s,1,i);
    end;
  until i=0;
  s:=trim(s);
  if s<>'' then Code.Add(s);
end;

function TSBProgram.Run:string;  // Запускаємо програму на виконання
var
 i,j,k,l,m,n:integer;
 s,s1,t,t1,t2,t3,t4,t5:string;
begin

if not isCompiled then begin   // якщо не компілювали, то запускаємо компіляцію

 // Перший прохід. Приводимо виклики функцій до загального вигляду
 for i:=0 to Code.Count-1 do
 begin
   s:=Code.Strings[i];
   j:=_pos(' ',s);
   if j>0 then
   begin
    t:=copy(s,1,j-1);
    if not _isconstruction(t) then
    begin
      if _isidentifier(t) then
      begin
        s[j]:='('; s:=s+')';
      end;
    end else s:='`'+s;     // Помічаємо оператори
   end else
    if not _isconstruction(s) then
    begin
      if _isidentifier(s) then s:=s+'()';
    end else s:='`'+s;    // Помічаємо оператори
   Code.Strings[i]:=s;
 end;
 
 // Прохід 1.25. Виділяємо оголошення функцій
 n:=Code.Count-1;
 i:=-1; k:=0; isSub:=false;
 while i<n do
 begin
   inc(i);  n:=Code.Count-1;
   s:=Code.Strings[i];
//   writeln('s=',s);
   if s[1]='`' then
   begin
     j:=_pos(' ',s);
     if j>0 then t:=copy(s,2,j-2) else t:=copy(s,2,length(s)-1);
//     writeln('t=',t);
     t:=lowercase(t);
     if t='sub' then begin
       isSub:=true;
       inc(k);
       setlength(Subs,k);
       Subs[k-1]:=TSBProgram.Create;
       l:=_pos('(',s);
       m:=_pos(')',s);
       if l>0 then t1:=copy(s,j+1,l-j-1) else t1:=copy(s,j+1,length(s)-j);
       t2:='';
       if (l>0)and(m>0)and(m>l) then t2:=copy(s,l+1,m-l-1);
       Subs[k-1].Name:=trim(lowercase(t1));
       Subs[k-1].Parameters:=trim(lowercase(t2));
       Code.Delete(i);
       dec(i);
       continue;
     end else
     if t='end' then begin
       if lowercase(s)='`end sub' then
       begin
         isSub:=false;
         Code.Delete(i);
         dec(i);
         continue;
       end;
     end;
   end;
   
     if isSub then
     begin
       //s:=Code.Strings[i];
       if s[1]='`' then delete(s,1,1);
       Subs[k-1].AddCodeLine(s);
//       writeln('SUB ',Subs[k-1].Name,' PARAM ',Subs[k-1].Parameters,' LINE ',s);
       Code.Delete(i);
       dec(i);
     end;

 end;


 // Прохід 1.5. Конвертуємо цикл for у while
 n:=Code.Count-1;
 i:=-1;
 while i<n do
 begin
   inc(i);  n:=Code.Count-1;
   s:=Code.Strings[i];
//   writeln('s=',s);
   if s[1]='`' then
   begin
     j:=_pos(' ',s);
     if j>0 then t:=copy(s,2,j-2) else t:=copy(s,2,length(s)-1);
//     writeln('t=',t);
     t:=lowercase(t);
     if t='for' then begin
       k:=_pos('=',s);
       s1:=lowercase(s);
       l:=_pos('to',s1);
       m:=_pos('step',s1);
       t1:=copy(s,j+1,k-j-1); // змінна циклу
       t2:=copy(s,k+1,l-k-2); // початкове значення
       if m>0 then t3:=copy(s,l+3,m-l-4) else t3:=copy(s,l+3,length(s)-l-2); // кінцеве значення
       if m>0 then t4:=copy(s,m+5,length(s)-m-4) else t4:='1'; // крок
{       writeln('s=',s);
       writeln('j=',j,'  k=',k,'  l=',l,'  m=',m);
       writeln('t1=',t1,'  t2=',t2,'  t3=',t3,'  t4=',t4);}
       sFor.Put(t1+'='+t1+'+'+t4);
       if t4[1]<>'-' then Code.Strings[i]:='`while '+t1+'<='+t3 else Code.Strings[i]:='`while '+t1+'>='+t3;
       Code.Insert(i,t1+'='+t2);
     end else
     if t='next' then begin
       t1:=sFor.Get;
       Code.Strings[i]:='`wend';
       Code.Insert(i,t1);
     end;
   end;
 end;

 // Другий прохід. Відмічаємо мітки для циклів
 for i:=0 to Code.Count-1 do
 begin
   s:=Code.Strings[i];
   if s[1]='`' then
   begin
     j:=_pos(' ',s);
     if j>0 then t:=copy(s,2,j-2) else t:=copy(s,2,length(s)-1);
//     writeln('t=',t);
     t:=lowercase(t);
     if t='while' then begin
//       writeln('i=',i);
       sWhile.Put(inttostr(i));
     end else
     if t='wend' then begin
       k:=strtoint(sWhile.Get);
       t1:=Code.Strings[k];
       insert(inttostr(i)+':',t1,2);
       Code.Strings[k]:=t1;
       insert(inttostr(k)+':',s,2);
       Code.Strings[i]:=s;
     end else
     if t='do' then begin
//       writeln('i=',i);
       sDo.Put(inttostr(i));
     end else
     if t='until' then begin
       k:=strtoint(sDo.Get);
       t1:=Code.Strings[k];
       insert(inttostr(i)+':',t1,2);
       Code.Strings[k]:=t1;
       insert(inttostr(k)+':',s,2);
       Code.Strings[i]:=s;
     end else
     if t='if' then begin
//       writeln('i=',i);
       sIf.Put(inttostr(i));
     end else
     if t='else' then begin
//       writeln('i=',i);
       sElse.Put(inttostr(i));
       k:=strtoint(sIf.Get);
       t1:=Code.Strings[k];
       insert(inttostr(i)+':',t1,2);
       Code.Strings[k]:=t1;
     end else
     if t='end' then begin
       if j>0 then t4:=copy(s,j+1,length(s)-j) else t4:='';
       t4:=lowercase(t4);
       if t4='if' then begin    // end if
          if sElse.GetSize>0 then   // if .. else .. end if
          begin
            k:=strtoint(sElse.Get);
            t1:=Code.Strings[k];
            insert(inttostr(i)+':',t1,2);
            Code.Strings[k]:=t1;
            insert('0:',s,2);
            Code.Strings[i]:=s;
          end else
          begin                     // if .. end if
            k:=strtoint(sIf.Get);
            t1:=Code.Strings[k];
            insert(inttostr(i)+':',t1,2);
            Code.Strings[k]:=t1;
            insert('0:',s,2);
            Code.Strings[i]:=s;
          end;
        end;
     end else

     begin
       SBIO.ErrorF('Operator '+t+' currently is not assigned');
     end;
   end;
 end;

{writeln('Precompiled code');
 for i:=0 to Code.Count-1 do
  writeln(i,':',Code.Strings[i]);
 writeln('-----------------------');}
 isCompiled:=true;

end;  // Компіляцію завершено

 CurPos:=-1;
 ForceExit:=false;
 // Виконуємо програму
 repeat
   inc(CurPos); if CurPos>(Code.Count-1) then break;
   s:=Code.Strings[CurPos];
   if s[1]<>'`' then     // Простий вираз (не конструкція)
   begin
     i:=_pos('=',s);
     if i>0 then
     begin
      t:=copy(s,1,i-1);
      if _isidentifier(t) then
      begin
        SetVar(t,RunExpression(copy(s,i+1,length(s)-i)));
        continue;
      end;
     end;
     t:=RunExpression(s);
   end else
   begin   // Оператор (конструкція)
     i:=_pos(':',s);
     t1:=copy(s,2,i-2);
     j:=_pos(' ',s);
     if j>0 then t:=copy(s,i+1,j-i-1) else t:=copy(s,i+1,length(s)-1);
     t:=lowercase(t);
     if t='while' then begin
       t2:=copy(s,j+1,length(s)-j);
       if RunExpression(t2)<>'1' then CurPos:=strtoint(t1);
     end else
     if t='wend' then begin
       CurPos:=strtoint(t1)-1;
     end else
     if t='do' then begin
       //CurPos:=strtoint(t1)-1;
       // Simply do nothing
     end else
     if t='until' then begin
       t2:=copy(s,j+1,length(s)-j);
       if RunExpression(t2)<>'1' then CurPos:=strtoint(t1);
     end else
     if t='if' then begin
       s1:=lowercase(s);
       k:=_pos('then',s1);
       if k>0 then t2:=copy(s,j+1,k-j-2) else t2:=copy(s,j+1,length(s)-j);
       //writeln('t2=',t2,'|');
       if RunExpression(t2)<>'1' then CurPos:=strtoint(t1);
     end else
     if t='else' then begin
       CurPos:=strtoint(t1);
     end else
     if t='end' then begin
       t2:=copy(s,j+1,length(s)-j);
       t2:=lowercase(t2);
       if t2='if' then begin
          // end if - Do nothing
       end else
       if t2='' then begin
          ForceExit:=true;
       end;
     end else
     begin
       SBIO.ErrorF('Running: Operator '+t+' currently is not assigned');
     end;
   end;
 until ForceExit;
end;

function TSBProgram.DoSub(s:string):string;
var
 params:array of string;
 paramnum:integer;
 i,a,j,z:integer;
 b:boolean;
 d,dt,k,m,q:integer;
 t,n,sr,t1,t2,t3:string;
 sp:PChar;
 p:TSBProgram;
 f:textfile;
begin
 result:='0';
 i:=_pos('(',s);
 setlength(params,1);
 params[0]:=copy(s,1,i-1);
 paramnum:=0;

 d:=0; b:=false;
 delete(s,1,i);
 i:=1;
 dt:=0;

 while i<=length(s) do
 begin
  if s[i]='"' then b:=not b;
  if (s[i]='[')and(not b) then inc(dt);
  if (s[i]=']')and(not b) then dec(dt);
  if (s[i]='(')and(not b)and(dt=0) then inc(d);
  if (s[i]=')')and(not b)and(dt=0) then
   if i=length(s) then begin
                        inc(paramnum);
                        setlength(params,paramnum+1);
                        params[paramnum]:=copy(s,1,i-1);
                        delete(s,1,i);
                        break;
                       end
                  else dec(d);

   if (s[i]=',')and(not b)and(d=0)and(dt=0) then
   begin
    inc(paramnum);
    setlength(params,paramnum+1);
    params[paramnum]:=copy(s,1,i-1);
    delete(s,1,i);
    i:=1;
    continue;
   end;

  inc(i);
 end;

 if params[1]='' then paramnum:=0;

 for i:=1 to paramnum do params[i]:=RunExpression(params[i]);

 params[0]:=lowercase(params[0]);

 if params[0]='sqr' then result:=_floattostr(sqr(_strtofloat(params[1]))) else
 if params[0]='sqrt' then result:=_floattostr(sqrt(_strtofloat(params[1]))) else
 if params[0]='trunc' then result:=inttostr(trunc(_strtofloat(params[1]))) else
 if params[0]='frac' then result:=_floattostr(frac(_strtofloat(params[1]))) else

 if params[0]='sin' then result:=_floattostr(sin(_strtofloat(params[1]))) else
 if params[0]='sinh' then result:=_floattostr(sin(_strtofloat(params[1]))) else
 if params[0]='arcsin' then result:=_floattostr(arcsin(_strtofloat(params[1]))) else
 if params[0]='arcsinh' then result:=_floattostr(arcsinh(_strtofloat(params[1]))) else
 if params[0]='cos' then result:=_floattostr(cos(_strtofloat(params[1]))) else
 if params[0]='cosh' then result:=_floattostr(cosh(_strtofloat(params[1]))) else
 if params[0]='arccos' then result:=_floattostr(arccos(_strtofloat(params[1]))) else
 if params[0]='arccosh' then result:=_floattostr(arccosh(_strtofloat(params[1]))) else
 if params[0]='cot' then result:=_floattostr(cot(_strtofloat(params[1]))) else
 if params[0]='sec' then result:=_floattostr(sec(_strtofloat(params[1]))) else
 if params[0]='csc' then result:=_floattostr(csc(_strtofloat(params[1]))) else
 if params[0]='tan' then result:=_floattostr(tan(_strtofloat(params[1]))) else
 if params[0]='tanh' then result:=_floattostr(tanh(_strtofloat(params[1]))) else
 if params[0]='arctan' then result:=_floattostr(arctan(_strtofloat(params[1]))) else
 if params[0]='arctanh' then result:=_floattostr(arctanh(_strtofloat(params[1]))) else
 if params[0]='cosecant' then result:=_floattostr(cosecant(_strtofloat(params[1]))) else

 if params[0]='gradtodeg' then result:=_floattostr(gradtodeg(_strtofloat(params[1]))) else
 if params[0]='gradtorad' then result:=_floattostr(gradtorad(_strtofloat(params[1]))) else
 if params[0]='radtodeg' then result:=_floattostr(radtodeg(_strtofloat(params[1]))) else
 if params[0]='radtograd' then result:=_floattostr(radtograd(_strtofloat(params[1]))) else
 if params[0]='degtorad' then result:=_floattostr(degtorad(_strtofloat(params[1]))) else
 if params[0]='degtograd' then result:=_floattostr(degtograd(_strtofloat(params[1]))) else

 if params[0]='randg' then result:=_floattostr(randg(_strtofloat(params[1]),_strtofloat(params[2]))) else

 if params[0]='ceil' then result:=inttostr(ceil(_strtofloat(params[1]))) else
 if params[0]='odd' then begin
  if odd(strtoint(params[1])) then result:='1' else result:='0';
 end else
 if params[0]='ord' then
 begin
  t:=_getstr(params[1]);
  result:=inttostr(ord(t[1]));
 end else
 if params[0]='chr' then
 begin
  result:=chr(strtoint(params[1]));
 end else

 if params[0]='ln' then result:=_floattostr(ln(_strtofloat(params[1]))) else
 if params[0]='log' then result:=_floattostr(log10(_strtofloat(params[1]))) else
 if params[0]='exp' then result:=_floattostr(exp(_strtofloat(params[1]))) else

 if params[0]='num' then result:=_getstr(params[1]) else

 if params[0]='abs' then result:=_floattostr(abs(_strtofloat(params[1]))) else

 if params[0]='round' then result:=inttostr(round(_strtofloat(params[1]))) else
 if params[0]='randomize' then randomize else
 if params[0]='rnd' then begin
   if paramnum=0 then result:=_floattostr(random) else
   result:=inttostr(random(strtoint(params[1])));
 end else
 if params[0]='str' then begin
  if params[2]='' then
   str(_strtofloat(params[1]):0:strtoint(params[3]),sr)
  else
   str(_strtofloat(params[1]):strtoint(params[2]):strtoint(params[3]),sr);
  result:=_makestr(sr);
 end else
 if params[0]='copy' then result:=_makestr(copy(_getstr(params[1]),strtoint(params[2]),strtoint(params[3]))) else
 if params[0]='getch' then result:=_makestr(copy(_getstr(params[1]),strtoint(params[2]),1)) else
 if params[0]='len' then result:=inttostr(length(_getstr(params[1]))) else
 if params[0]='delete' then
 begin
  t:=_getstr(params[1]);
  delete(t,strtoint(params[2]),strtoint(params[3]));
  result:=_makestr(t);
 end else
 if params[0]='pos' then result:=inttostr(pos(_getstr(params[1]),_getstr(params[2]))) else
 if params[0]='open' then begin
                           SBIO.OpenFile(strtoint(params[1]),_getstr(params[3]),_getstr(params[2]));
                          end else
 if params[0]='close' then SBIO.CloseFile(strtoint(params[1])) else
 if params[0]='printf' then begin
    SBIO.WriteToFile(strtoint(params[1]),_getstr(params[2]));
 end else
 if params[0]='print' then begin
  SBIO.Print(_getstr(params[1]),true);
 end else
 if params[0]='write' then begin
  SBIO.Print(_getstr(params[1]),false);
 end else
 if params[0]='eof' then begin
  if SBIO.IsEof(strtoint(params[1])) then result:='1' else result:='0';
 end else
 if params[0]='inputf' then begin
    result:=_makestr(SBIO.ReadFromFile(strtoint(params[1])));
 end else
 if params[0]='input' then begin
  if params[1]<>'' then
  begin
   t:=SBIO.Input(_getstr(params[1]));
   if (t='')and(paramnum>1) then t:=params[2];
   if _isnumber(t) then result:=t else result:=_makestr(t);
  end else t:=SBIO.Input('');
 end else

 if params[0]='return' then begin
  SetVar('RESULT',params[1]);
  result:=params[1];
 end else
 if params[0]='require' then begin
   if strtoint(params[1])>iSBVer then
   begin
     SBIO.ErrorF('For run this program you need SimpleBasic ver.'+_floattostr((strtoint(params[1])/10))+' but installed version '+sSBVer);
     ForceExit:=true;
   end;
 end else

 if params[0]='getsbver' then begin
  result:=inttostr(iSBVer);
 end else
 if params[0]='eval' then begin
  result:=RunExpression(_getstr(params[1]));
 end else
 

 // Text Mode Terminal Functions
 if params[0]='terminal.clear' then begin
   SBIO.TMT_Clear;
 end else
 if params[0]='terminal.backround' then begin
   SBIO.TMT_Background(strtoint(params[1]));
 end else
 if params[0]='terminal.color' then begin
   SBIO.TMT_Color(strtoint(params[1]));
 end else
 if params[0]='terminal.delline' then begin
   SBIO.TMT_DelLine;
 end else
 if params[0]='terminal.insline' then begin
   SBIO.TMT_InsLine;
 end else
 if params[0]='terminal.keypressed' then begin
   if SBIO.TMT_KeyPressed then result:='1' else result:='0';
 end else
 if params[0]='terminal.locate' then begin
   SBIO.TMT_Locate(strtoint(params[1]),strtoint(params[2]));
 end else
 if params[0]='terminal.key' then begin
   result:=_makestr(SBIO.TMT_ReadKey);
 end else
 if params[0]='terminal.sleep' then begin
   SBIO.TMT_Sleep(strtoint(params[1]));
 end else
 if params[0]='terminal.videomode' then begin
   SBIO.TMT_VideoMode(_getstr(params[1]));
 end else



 begin   // Do an user function
  k:=length(Subs);
  for i:=0 to k-1 do
   if Subs[i].Name=params[0] then
   begin
     t1:=Subs[i].Parameters;
     m:=0; q:=0;
     repeat
       m:=_pos(',',t1);
       if m>0 then
       begin
         t2:=copy(t1,1,m-1);
         delete(t1,1,m);
         inc(q);
         if paramnum>=q then
          Subs[i].SetVar(t2,params[q])
         else
          SBIO.ErrorF('Parameter '+t2+' required in function '+params[0]);
       end;
     until m=0;
     if t1<>'' then
     begin
         inc(q);
         if paramnum>=q then
          Subs[i].SetVar(t1,params[q])
         else
          SBIO.ErrorF('Parameter '+t1+' required in function '+params[0]);
     end;

     for k:=0 to length(Variables)-1 do
        Subs[i].SetVar('__'+Variables[k].name,Variables[k].value);

     Subs[i].Run;
     
     for k:=0 to length(Subs[i].Variables)-1 do
        if copy(Subs[i].Variables[k].name,1,2)='__' then
        begin
          t2:=copy(Subs[i].Variables[k].name,3,length(Subs[i].Variables[i].name)-2);
          SetVar(t2,Subs[i].Variables[k].value);
        end;

     result:=Subs[i].GetVar('RESULT');
     break;
   end;
  {t:=ExtF(params[0],paramnum,params);
  if t<>#0 then result:=t;}
 end;
end;

procedure TSBProgram.SetVar(s,v:string);
var
 a,d,i,j,k,l,m,paramnum:integer;
 st,n,t,t1,t2,t3:string;
 b:boolean;
 params:array of string;
begin
 i:=_pos('[',s);
 a:=_posbk('.',s);

 if i>0 then
 begin
  setlength(params,1);
  params[0]:=copy(s,1,i-1);
  if a>0 then
  begin
   st:=copy(s,a,length(s)-a+1);
   delete(s,a,length(s)-a+1);
  end else st:='';
  paramnum:=0;

  d:=0; b:=false;
  delete(s,1,i);
  i:=1;
  s[length(s)]:=')';
  while i<=length(s) do
  begin
   if s[i]='"' then b:=not b;
   if (s[i]='(')and(not b) then inc(d);
   if (s[i]=')')and(not b) then
   if i=length(s) then begin
                        inc(paramnum);
                        setlength(params,paramnum+1);
                        params[paramnum]:=copy(s,1,i-1);
                        delete(s,1,i);
                        break;
                       end
                  else dec(d);

   if (s[i]=',')and(not b)and(d=0) then
   begin
    inc(paramnum);
    setlength(params,paramnum+1);
    params[paramnum]:=copy(s,1,i-1);
    delete(s,1,i);
    i:=1;
    continue;
   end;

  inc(i);
 end;

  for i:=1 to paramnum do params[i]:=RunExpression(params[i]);
  s:=params[0];
  for i:=1 to paramnum do s:=s+'_'+params[i];
  n:=s+st;
  s:=n;
 end; // if

// writeln('::::::'+s+'='+v);
  
 for i:=0 to length(Variables)-1 do
 begin
   if Variables[i].name=lowercase(s) then
   begin
    Variables[i].value:=v;
    exit;
   end;
 end;
 // Declare new variable
 i:=length(Variables);
 setlength(Variables,i+1);
 Variables[i].name:=lowercase(s);
 Variables[i].value:=v;
end;

function TSBProgram.GetVar(s:string):string;
var
 a,d,i,j,k,l,m,paramnum:integer;
 st,n,t,t1,t2,t3:string;
 b:boolean;
 params:array of string;
begin
// writeln('GetVar:'+s+'|');

 i:=_pos('[',s);
 a:=_posbk('.',s);

 if i>0 then
 begin
  setlength(params,1);
  params[0]:=copy(s,1,i-1);
  if a>0 then
  begin
   st:=copy(s,a,length(s)-a+1);
   delete(s,a,length(s)-a+1);
  end else st:='';
  paramnum:=0;

  d:=0; b:=false;
  delete(s,1,i);
  i:=1;
  s[length(s)]:=')';
  while i<=length(s) do
  begin
   if s[i]='"' then b:=not b;
   if (s[i]='(')and(not b) then inc(d);
   if (s[i]=')')and(not b) then
   if i=length(s) then begin
                        inc(paramnum);
                        setlength(params,paramnum+1);
                        params[paramnum]:=copy(s,1,i-1);
                        delete(s,1,i);
                        break;
                       end
                  else dec(d);

   if (s[i]=',')and(not b)and(d=0) then
   begin
    inc(paramnum);
    setlength(params,paramnum+1);
    params[paramnum]:=copy(s,1,i-1);
    delete(s,1,i);
    i:=1;
    continue;
   end;

  inc(i);
 end;

  for i:=1 to paramnum do params[i]:=RunExpression(params[i]);
  s:=params[0];
  for i:=1 to paramnum do s:=s+'_'+params[i];
  n:=s+st;
  s:=n;
 end; // if

 for i:=0 to length(Variables)-1 do
 begin
   if Variables[i].name=lowercase(s) then
   begin
    result:=Variables[i].value;
    exit;
   end;
 end;
 result:='0';
end;

// Клас реалізації введення/виведення

constructor TSBIO.Create;
begin
//   setlength(Files,150);  // Файли #100+ зарезервовано для системних потреб
end;

destructor TSBIO.Destroy;
begin

end;

procedure TSBIO.Print(s:string;caret:boolean);
begin
  if caret then writeln(s) else write(s);
end;

procedure TSBIO.ErrorF(s:string);
begin
   Print('(Error) '+s,true);
end;

function TSBIO.Input(s:string):string;
var
 t:string;
begin
   Print(s,false);
   readln(t);
   result:=t;
end;

procedure TSBIO.OpenFile(ind:integer;n,t:string);
begin
   if (ind+1)>length(Files) then setlength(Files,ind+1);
   assignfile(Files[ind],n);
   if lowercase(t)='w' then rewrite(Files[ind]) else
   if lowercase(t)='r' then reset(Files[ind]) else
   if lowercase(t)='a' then append(Files[ind]);
end;

procedure TSBIO.WriteToFile(ind:integer;s:string);
begin
   writeln(Files[ind],s);
end;

function TSBIO.ReadFromFile(ind:integer):string;
var
 t:string;
begin
   readln(Files[ind],t);
   result:=t;
end;

function TSBIO.IsEof(ind:integer):boolean;
begin
   result:=eof(Files[ind]);
end;

procedure TSBIO.CloseFile(ind:integer);
begin
   System.Close(Files[ind]);
end;

procedure TSBIO.TMT_Clear;
begin
   ClrScr;
end;

procedure TSBIO.TMT_Sleep(t:integer);
begin
   Delay(t);
end;

procedure TSBIO.TMT_DelLine;
begin
   DelLine;
end;

procedure TSBIO.TMT_InsLine;
begin
   InsLine;
end;

procedure TSBIO.TMT_Locate(x,y:integer);
begin
   GotoXY(x,y);
end;

function TSBIO.TMT_KeyPressed:boolean;
begin
   result:=KeyPressed;
end;

function TSBIO.TMT_ReadKey:string;
var
 c1,c2:char;
begin
 c1:=readkey;
 case c1 of
  #0: begin
        c2:=readkey;
        case c2 of
           #$0:result:='nokey';
           #$1:result:='alt+esc';
           #$2:result:='alt+space';
           #$4:result:='ctrl+ins';
           #$5:result:='shift+ins';
           #$6:result:='ctrl+del';
           #$7:result:='shift+del';
           #$8:result:='alt+backspace';
           #$0f:result:='shift+tab';

           #$10:result:='alt+q';
           #$11:result:='alt+w';
           #$12:result:='alt+e';
           #$13:result:='alt+r';
           #$14:result:='alt+t';
           #$15:result:='alt+y';
           #$16:result:='alt+u';
           #$17:result:='alt+i';
           #$18:result:='alt+o';
           #$19:result:='alt+p';
           #$1a:result:='alt+leftbracket';
           #$1b:result:='alt+rightbracket';
           #$1e:result:='alt+a';
           #$1f:result:='alt+s';

           #$20:result:='alt+d';
           #$21:result:='alt+f';
           #$22:result:='alt+g';
           #$23:result:='alt+h';
           #$24:result:='alt+j';
           #$25:result:='alt+k';
           #$26:result:='alt+l';
           #$27:result:='alt+semicolon';
           #$28:result:='alt+quote';
           #$29:result:='alt+oppositequote';
           #$2b:result:='alt+backslash';

           #$2c:result:='alt+z';
           #$2d:result:='alt+x';
           #$2e:result:='alt+c';
           #$2f:result:='alt+v';
           #$30:result:='alt+b';
           #$31:result:='alt+n';
           #$32:result:='alt+m';
           #$33:result:='alt+comma';
           #$34:result:='alt+period';
           #$35:result:='alt+slash';

           #$3b:result:='f1';
           #$3c:result:='f2';
           #$3d:result:='f3';
           #$3e:result:='f4';
           #$3f:result:='f5';
           #$40:result:='f6';
           #$41:result:='f7';
           #$42:result:='f8';
           #$43:result:='f9';
           #$44:result:='f10';
           #$47:result:='home';
           #$48:result:='up';
           #$49:result:='pageup';
           #$4b:result:='left';
           #$4c:result:='center';
           #$4d:result:='right';
           #$4e:result:='alt+greyplus';
           #$4f:result:='end';
           #$50:result:='down';
           #$51:result:='pagedown';
           #$52:result:='insert';
           #$53:result:='delete';

           #$54:result:='shift+f1';
           #$55:result:='shift+f2';
           #$56:result:='shift+f3';
           #$57:result:='shift+f4';
           #$58:result:='shift+f5';
           #$59:result:='shift+f6';
           #$5a:result:='shift+f7';
           #$5b:result:='shift+f8';
           #$5c:result:='shift+f9';
           #$5d:result:='shift+f10';

           #$5e:result:='ctrl+f1';
           #$5f:result:='ctrl+f2';
           #$60:result:='ctrl+f3';
           #$61:result:='ctrl+f4';
           #$62:result:='ctrl+f5';
           #$63:result:='ctrl+f6';
           #$64:result:='ctrl+f7';
           #$65:result:='ctrl+f8';
           #$66:result:='ctrl+f9';
           #$67:result:='ctrl+f10';

           #$68:result:='alt+f1';
           #$69:result:='alt+f2';
           #$6a:result:='alt+f3';
           #$6b:result:='alt+f4';
           #$6c:result:='alt+f5';
           #$6d:result:='alt+f6';
           #$6e:result:='alt+f7';
           #$6f:result:='alt+f8';
           #$70:result:='alt+f9';
           #$71:result:='alt+f10';

           #$72:result:='ctrl+printscreen';
           #$73:result:='ctrl+left';
           #$74:result:='ctrl+right';
           #$75:result:='ctrl+end';
           #$76:result:='ctrl+pagedown';
           #$77:result:='ctrl+home';
           #$78:result:='alt+1';
           #$79:result:='alt+2';
           #$7a:result:='alt+3';
           #$7b:result:='alt+4';
           #$7c:result:='alt+5';
           #$7d:result:='alt+6';
           #$7e:result:='alt+7';
           #$7f:result:='alt+8';
           #$80:result:='alt+9';
           #$81:result:='alt+0';

           #$82:result:='alt+minus';
           #$83:result:='alt+equal';
           #$84:result:='ctrl+pgup';

           #$85:result:='f11';
           #$86:result:='f12';
           #$87:result:='shift+f11';
           #$88:result:='shift+f12';
           #$89:result:='ctrl+f11';
           #$8a:result:='ctrl+f12';
           #$8b:result:='alt+f11';
           #$8c:result:='alt+f12';

           #$8d:result:='ctrl+up';
           #$8e:result:='ctrl+minus';
           #$8f:result:='ctrl+center';
           #$90:result:='ctrl+greyplus';
           #$91:result:='ctrl+down';
           #$94:result:='ctrl+tab';

           #$97:result:='alt+home';
           #$98:result:='alt+up';
           #$99:result:='alt+pageup';
           #$9b:result:='alt+left';
           #$9d:result:='alt+right';
           #$9f:result:='alt+end';
           #$a0:result:='alt+down';
           #$a1:result:='alt+pagedown';
           #$a2:result:='alt+insert';
           #$a3:result:='alt+delete';
           #$a5:result:='alt+tab';
        end;
      end;
  #27:result:='esc';
  #9:result:='tab';
  #13:result:='enter';
  #8:result:='backspace';
 else
  result:=c1;
 end;
end;

procedure TSBIO.TMT_Color(c:integer);
begin
  TextColor(c);
end;

procedure TSBIO.TMT_Background(c:integer);
begin
  TextBackground(c);
end;

procedure TSBIO.TMT_VideoMode(s:string);
begin
  if s='normal' then normvideo else
  if s='high' then highvideo else
  if s='low' then lowvideo;
end;


// Код ініціалізації модуля
var
 t:string;
begin
 t:=floattostr(1/2);
 OSDecSymbol:=t[2];
 
end.

