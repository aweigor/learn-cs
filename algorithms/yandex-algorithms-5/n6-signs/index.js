n6-"use strict";

const fs = require("node:fs/promises");

async function readInput() {
  return await fs.readFile("./input.txt", { encoding: "utf8" });
}

function formatInput(data) {
  let [n, d] = String(data).split("\n");
  n = parseInt(n);
  d = d.split(" ").map((e) => parseInt(e));
  return [n, d];
}

function calculateSigns(n, d) {
  let sum = d[0];
  let signs = "";
  for (let i = 1; i < n; i++) {
    if (sum % 2 !== 0) {
      // odd
      if (d[i] % 2 !== 0) {
        // odd + odd
        // сохранить нечетность
        sum *= d[i];
        signs += "x";
      } else {
        // odd + even
        // сохранить нечетность
        sum += d[i];
        signs += "+";
      }
    } else {
      // even
      if (d[i] % 2 !== 0) {
        // even + odd
        // сохранить нечетность
        sum += d[i];
        signs += "+";
      } else {
        // even + even
        // знак не имеет значения
        sum += d[i];
        signs += "+";
      }
    }
  }
  return signs;
}

readInput().then((data) => {
  let [n, d] = formatInput(data);
  let answer = calculateSigns(n, d);
  fs.writeFile("output.txt", String(answer));
});
