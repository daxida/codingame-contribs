open Hashtbl;;
let(t,h,c)=(read_int(),create 101,Array.init 2(fun _->0))in
List.init(read_int())(fun _->Scanf.sscanf(read_line())"%d %d %d"(fun a b c->(a,b,c)))
|>List.iter(fun(i,v,x)->if x-(try find h i with Not_found-> -200)>=t then begin c.(v)<-c.(v)+1;add h i x end);
Printf.printf"%d %d"c.(0) c.(1)
