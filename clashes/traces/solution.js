// https://en.wikipedia.org/wiki/Trace_monoid

function sameLetters(a, b) {
    const sa = a.split('').sort().join('');
    const sb = b.split('').sort().join('');

    return sa === sb;
}

function solve(test) {
    [base, target, fixed] = test.split(" ");
    
    // (IGNORE) Sanity check for the constraints.
    if (!([...fixed].every(x => base.includes(x) && target.includes(x))
           && [...base].every(x => x == x.toLowerCase())
           && [...target].every(x => x == x.toLowerCase())
           && [...fixed].every(x => fixed.match(x).length == 1))
           && test.length < 256) {
        console.log(`Error at ${test}`);
    }

    // Need same length and same chars
    if (base.length != target.length) return false;
    if (!sameLetters(base, target)) return false;
    
    // If you don't add this, the next logic will fail at:
    // abac acab bc
    for (let i = 0; i < fixed.length; i++) {
        const c1 = base[i];
        const c2 = target[i];
        if (fixed.includes(c1) || fixed.includes(c2)) {
            if (c1 != c2) return false;
        }
    }
    
    // Each of the "islands" needs to have the same chars
    // (we already know that they are of same length)
    let sep = new RegExp([...fixed].join("|"));
    let a = base.split(sep);
    let b = target.split(sep);
    for (let i = 0; i < a.length; i++) {
        const b1 = a[i];
        const b2 = b[i];
        if (!sameLetters(b1, b2)) {
            return false;
        }
    }

    return true;
}

const t = parseInt(readline());
for (let i = 0; i < t; i++) {
    const test = readline();
    const ans = solve(test);
    console.log(ans);
}
