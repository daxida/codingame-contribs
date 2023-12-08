const n = parseInt(readline());
const c = readline();
let s = readline().split(" ");
s[0] = '';
s = s.sort();

const group = Array.from({ length: n }, (_, i) => c.repeat(i));

console.log(group.filter(x => !s.includes(x)).join(" ") || n)