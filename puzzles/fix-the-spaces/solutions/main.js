function solve(original, words, used) {
    if (words.length === 0) {
        if (solution.length > 0) {
            console.log("Unsolvable")
            process.exit(0)
        }
        solution = used.join(" ")
    }

    const uniqueWords = new Set(words);
    for (const word of uniqueWords) {
        if (original.startsWith(word)) {
            const nwords = [...words];
            nwords.splice(words.indexOf(word), 1);   
            used.push(word)
            solve(original.substring(word.length), nwords, used)
            used.pop()
        }
    }
}

const original = readline()
const words = readline().split(" ")
var solution = ""
solve(original, words, [])
console.log(solution);

