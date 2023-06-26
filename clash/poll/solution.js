"use strict"

const throttle = function (fn, t) {
    const users = new Map();
  
    return function (id, vote, time) {
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
    const [id, vote, time] = readline().split(" ").map(Number);
    throttledTally(id, vote, time);
}

console.log(counter.join(" "));
