const s = readline();

sum = 0
for (c of s) sum += c.charCodeAt(0)

sol = "prime"
for (c of s) {
    if (sum % c.charCodeAt(0) == 0){
        sol = c
        break
    }
}

console.log(sol);
