const fs = require('node:fs/promises');

async function readInput() {
  return await fs.readFile('./input.txt', { encoding: 'utf8' });
}

function formatInput(data) {
	const [G11, G21] = String(data).split('\n')[0].split(':').map((n) => parseInt(n));
	const [G12, G22] = String(data).split('\n')[1].split(':').map((n) => parseInt(n));
	let [PL] = String(data).split('\n')[2];
	PL = parseInt(PL);
	return [G11, G12, G21, G22, PL];
}

function computeGoalsToWin(G11, G12, G21, G22, PL) 
{
	// голы первой команды минус голы второй
	let totalDifference = (G11 + G12) - (G21 + G22);
	// первая команда уже выигрывает?
	if (totalDifference > 0) return 0;
	// если текущая игра в гостях, то предположение влияет на расчет
	const visitorDifference = PL === 1 ? G12 - G21 + Math.abs(totalDifference) : PL === 2 ? G11 - G22 : 0;
	return Math.abs(totalDifference) + Number(visitorDifference <= 0);
}

readInput().then((data => {
	const [G11, G12, G21, G22, PL] = formatInput(data);
	
	const goalsToWin = computeGoalsToWin(G11, G12, G21, G22, PL);
	
	fs.writeFile('output.txt', String(goalsToWin));
}));
