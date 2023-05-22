function getAsciiSum(str: string): number {
    return [...str].reduce((sum, ch) => sum + ch.charCodeAt(0), 0);
}
  
function solve(str: string, sum: number): string {
    for (const ch of str) {
        if (sum % ch.charCodeAt(0) === 0) return ch;
    }
    return "prime";
}
  
const s: string = readline();
console.log(solve(s, getAsciiSum(s)));
