const fs = require('node:fs/promises');

async function readInput() {
  return await fs.readFile('./input.txt', { encoding: 'utf8' });
}

function formatInput(data) {
	const lines = String(data).split('\n');
	let N = lines[0];
	N = Number(N);
	const spacesPerLineArray = [];

	for (let i = 1; i <= N; i++) {
		let S = lines[i];
		S = Number(S);
		spacesPerLineArray.push(S);
	}

	return spacesPerLineArray;
}

// | ___ ___ ___ ___ |  
//
// 4 пробела, 4й пробел - tab (1 нажатие, включен в результат)
// 1й пробел - 1 space - вес 1
// 2й пробел - 2 space - вес 2
// 3й пробел - 1 tab + 1  backspace - вес 2
//
// (4 - offtab) & offtab) -- отступ от %4 (для 3 = 1, для 2 = 2, для 1 = 1)

function computeMinimumKeyboardPresses(spacesPerLineArray) 
{
	let result = 0;
	let tabs = 0;
	let offtab = 0;
	for (const spacesNeeded of spacesPerLineArray) {
		offtab = spacesNeeded % 4;
		tabs = Math.floor(spacesNeeded / 4);
		tabs += Math.floor(offtab / 3); // сделать еще tab, если остаток 3
		result += tabs + ((4 - offtab) & offtab);
	}
	return result;
}

readInput().then((data => {
	const spacesPerLineArray = formatInput(data);
	
	const minimumKeyboardPresses = computeMinimumKeyboardPresses(spacesPerLineArray);
	
	console.log('minimumKeyboardPresses',minimumKeyboardPresses)

	fs.writeFile('output.txt', String(minimumKeyboardPresses));
}));
