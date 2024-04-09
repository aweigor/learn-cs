const fs = require("node:fs/promises");

async function readInput() {
  return await fs.readFile("./input.txt", { encoding: "utf8" });
}

function formatInput(data) {
  return String(data).split(" ");
}

function getFirstDayVygoda(n, k) {
  let vygoda = n * 10;
  let i;
  for (i = 0; i < 10; i++) {
    if ((vygoda + i) % k === 0) {
      vygoda = vygoda + i;
      break;
    }
  }
  if (i === 10) {
    return -1;
  }
  return vygoda;
}

readInput().then((data) => {
  let [n, k, d] = formatInput(data);
  d = parseInt(d);
  n = parseInt(n);
  k = parseInt(k);
  const firstDayVygoda = getFirstDayVygoda(n, k);
  let vygoda = firstDayVygoda;
  if (vygoda !== -1) {
    vygoda = String(vygoda).padEnd(String(n).length + d, "0");
  }
  fs.writeFile("output.txt", String(vygoda));
});
