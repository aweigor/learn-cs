const fs = require("node:fs/promises");

async function readInput() {
  return await fs.readFile("./input.txt", { encoding: "utf8" });
}

function formatInput(data) {
  const lines = String(data).split("\n");
  return lines.map((s) => {
    s = s.trim();
    return s.split("");
  });
}

function findFigureByCoords({ i, j }, figures) {
  return figures.find((f) => {
    return f.i === i && f.j === j;
  });
}

function checkCellUnderAttack(cell, figures) {
  let i = 0;
  let j = 0;
  let f = null;
  let box = []; // box - описывает квадрат вокруг клетки
  let boxMaskDiagonal = []; // маскирует диагонали, которые не бьются
  let boxMaskParallel = []; // маскирует параллели, которые точно не бьются

  const applyMask = function (box, mask) {
    for (let i = 0; i < box.length; i++) {
      if (mask[i] === null) box[i] = null;
    }
    return box;
  };

  for (i = 0; i < 8; i++) {
    // параллели
    box = [
      { i: cell.i - i, j: cell.j }, // center top
      { i: cell.i, j: cell.j + i }, // right center
      { i: cell.i + i, j: cell.j }, // center bottom
      { i: cell.i, j: cell.j - i }, // left center
    ];

    box = applyMask(box, boxMaskParallel);

    for (j = 0; j < box.length; j++) {
      const c = box[j];
      if (c === null) continue; // параллель не бьется
      f = findFigureByCoords(c, figures);
      if (f) {
        if (f.type === "R") {
          return true;
        } else if (f.type === "B") {
          boxMaskParallel[j] = null;
        }
      }
    }
    // диагонали
    box = [
      { i: cell.i - i, j: cell.j + i }, // right top
      { i: cell.i + i, j: cell.j + i }, // right bottom
      { i: cell.i + i, j: cell.j - i }, // left bottom
      { i: cell.i - i, j: cell.j - i }, // left top
    ];
    box = applyMask(box, boxMaskDiagonal);

    for (j = 0; j < box.length; j++) {
      const c = box[j];
      if (c === null) continue; // диагональ не бьется
      f = findFigureByCoords(c, figures);
      if (f) {
        if (f.type === "B") {
          return true;
        } else if (f.type === "R") {
          boxMaskDiagonal[j] = null;
        }
      }
    }
  }
  return false;
}

function computeCellsUnderAttack(board) {
  let i, j;
  const figures = [];
  const out = [];
  let underAttackCount = 0;
  // 1й проход - запишем фигуры
  for (i = 0; i < board.length; i++) {
    for (j = 0; j < board[i].length; j++) {
      if (board[i][j] !== "*") {
        figures.push({ i, j, type: board[i][j] });
      }
    }
  }
  // 2й проход - отметим клетки, которые бьются
  for (i = 0; i < board.length; i++) {
    out[i] = [];
    for (j = 0; j < board[i].length; j++) {
      if (checkCellUnderAttack({ i, j }, figures)) {
        underAttackCount++;
      }
    }
  }
  return underAttackCount;
}

readInput().then((data) => {
  const board = formatInput(data);
  const cellsUnderAttack = computeCellsUnderAttack(board);
  fs.writeFile("output.txt", String(8 * 8 - cellsUnderAttack));
});
