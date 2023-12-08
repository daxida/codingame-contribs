function parse(line) {
  const letters = line.split('').map(c => (/[^-":;, '?!.\n]/.test(c) ? c : ' '));
  return letters.join('').split(/\s+/);
}

const errors = ["éd", "éds", "éf", "éfs", "ér", "érs", "éz", "ézs"];

const t = parseInt(readline());
for (let i = 0; i < t; i++) {
  const words = parse(readline());
  for (const word of words) {
    if (word.includes('éx') || errors.some(e => word.endsWith(e))) {
      console.log(word);
    }
  }
}
