// Solution by @About

import std;

void main() {
    foreach (_; readln.chomp.to!int.iota)
        foreach (w; readln.split!(c => "-\":;, '?!.\n".canFind(c))) {
            if (w.canFind("éx")) {
                w.writeln;
                continue;
            }
            foreach (bad; "éd éf ér éz éds éfs érs ézs".split)
                if (w.endsWith(bad)) {
                    w.writeln;
                    break;
                }
        }
}