"use strict"

const throttle = function (fn, t) {
    const users = new Map();
  
    return function (vote, time, id) {
        if (time - users.get(id) < t) return;

        users.set(id, time);
        fn(vote);
    };
};

const timeout = parseInt(readline());
const nEntries = parseInt(readline());

const counter = [0, 0];
const tally = (vote) => counter[vote] += 1;
const throttledTally = throttle(tally, timeout);

for (let i = 0; i < nEntries; i++) {
    const [vote, time, id] = readline().split(" ").map(Number);
    throttledTally(vote, time, id);
}

console.log(counter.join(" "));
