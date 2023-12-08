program Answer;
{$H+}
uses sysutils;

var
    a, b, ans : String;
    i, j: integer;
begin
    readln(a);
    readln(b);
    ans := '';
    i := 1;
    j := 1;

    while (i <= Length(a)) and (j <= Length(b)) do
    begin
        if a[i] = b[j] then
        begin
            ans := ans + a[i];
            Inc(j)
        end
        else
            ans := ans + '-';
        Inc(i)
    end;

    writeln(ans.PadRight(Length(a), '-'));
end.